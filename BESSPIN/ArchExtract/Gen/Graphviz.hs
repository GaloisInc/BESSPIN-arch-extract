module BESSPIN.ArchExtract.Gen.Graphviz where

import Control.Monad
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Sequence (Seq, (|>), (<|))
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Debug.Trace

import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types
import Data.GraphViz.Types.Generalised

import BESSPIN.ArchExtract.Architecture


tSource = T.pack "source"
tSink = T.pack "sink"

tInputs = T.pack "Inputs"
tOutputs = T.pack "Outputs"


data NodeId = NConn Side Conn | NNet Int | NLogic Int
    deriving (Show, Eq, Ord)

type EdgeMap = Map NodeId (Seq NodeId)

-- Modify an `EdgeMap`, replacing edges ending at nodes matching predicate `f`
-- with new edges that bypass that node.  For example:
--
-- Before:
--         /-> c
--   a -> b
--         \-> d
--
-- Predicate: matches only `b`
--
-- After:
--    /------> c
--   a
--    \------> d
bypassNodes :: (NodeId -> Bool) -> EdgeMap -> EdgeMap
bypassNodes f edges = traceShow ("full map", edges) $ M.mapMaybeWithKey go edges
  where
    go :: NodeId -> Seq NodeId -> Maybe (Seq NodeId)
    go src _ | f src = Nothing
    go src tgts = Just $ join $ fmap (newTargets (Set.singleton src)) tgts

    -- Get the target(s) that replace `node` in the output.  If `not $ f node`,
    -- then `node` is kept unchanged; otherwise, it is replaced with its
    -- targets in `edges` (this creates the bypassing edges.  `seen` is used to
    -- prevent infinite loops when the graph contains a cycle.
    newTargets :: Set NodeId -> NodeId -> Seq NodeId
    newTargets seen node | traceShow ("consider", node, "saw", seen) False = undefined
    newTargets seen node | node `Set.member` seen =
        if not $ f node then S.singleton node else S.empty
    newTargets seen node | not $ f node = S.singleton node
    newTargets seen node =
        let tgts = fromMaybe S.empty $ M.lookup node edges in
        traceShow ("bypassing", node, "mapping over", tgts) $
        join $ fmap (newTargets (Set.insert node seen)) tgts

-- Like `bypassNodes`, but deduplicates edges internally for efficiency.
bypassNodesWithDedup :: (NodeId -> Bool) -> EdgeMap -> EdgeMap
bypassNodesWithDedup f edges = traceShow ("full map", edges) $ M.mapMaybeWithKey go edges
  where
    go :: NodeId -> Seq NodeId -> Maybe (Seq NodeId)
    go src _ | f src = Nothing
    go src tgts =
        let tgtsSet = Set.fromList $ toList tgts in
        let tgtsUniq = S.fromList $ Set.toList tgtsSet in
        Just $ loop tgtsSet tgtsUniq S.empty

    -- `seen`: set of all nodes that have ever been added to pending.  Used to
    -- ensure we process each node at most once.
    -- `pending`: queue of nodes left to process.
    loop seen pending acc = case S.viewl pending of
        S.EmptyL -> acc
        cur S.:< pending -> if f cur then
                let tgts = fromMaybe S.empty $ M.lookup cur edges in
                let (seen', pending') = foldl (\(s,p) n ->
                        if not $ n `Set.member` s then (Set.insert n s, p |> n)
                        else (s, p)) (seen, pending) tgts in
                loop seen' pending' acc
            else
                loop seen pending (acc |> cur)

dedupEdges m = fmap go m
  where
    go xs = fst $ foldl (\(acc, seen) x ->
            if not $ x `Set.member` seen then (acc |> x, Set.insert x seen)
            else (acc, seen))
        (S.empty, Set.empty) xs

netEdges :: Cfg -> Int -> Net -> Seq (NodeId, NodeId)
netEdges cfg idx net =
    fmap (\conn -> (connNode Source conn, NNet idx)) (netSources net) <>
    fmap (\conn -> (NNet idx, connNode Sink conn)) (netSinks net)
  where
    connNode side (LogicPort i _) = NLogic i
    connNode side conn = NConn side conn

allNetEdges :: Cfg -> Seq Net -> Seq (NodeId, NodeId)
allNetEdges cfg nets = join $ S.mapWithIndex (netEdges cfg) nets

filterEdges :: Cfg -> ModDecl -> Seq (NodeId, NodeId) -> Seq (NodeId, NodeId)
filterEdges cfg mod edges = edges'
  where
    edgeMap = foldl (\m (s, t) -> M.insertWith (<>) s (S.singleton t) m) M.empty edges
    bypass = if cfgDedupEdges cfg then bypassNodesWithDedup else bypassNodes
    edgeMap' = flip bypass edgeMap $ \n -> case n of
        NLogic _ -> not $ cfgDrawLogics cfg
        NNet i -> not $ drawNetNode cfg (modDeclNets mod `S.index` i)
        _ -> False
    edges' = M.foldlWithKey (\es s ts -> es <> fmap (\t -> (s, t)) ts) S.empty edgeMap'



data Cfg = Cfg
    { cfgPrefix :: Text
    , cfgDrawNets :: Bool
    , cfgDrawOnesidedNets :: Bool
    , cfgDrawLogics :: Bool
    , cfgHideNamedNets :: Set Text
    , cfgDedupEdges :: Bool
    }

defaultCfg = Cfg
    { cfgPrefix = T.empty
    , cfgDrawNets = True
    , cfgDrawOnesidedNets = True
    , cfgDrawLogics = True
    , cfgHideNamedNets = Set.empty
    , cfgDedupEdges = False
    }

drawNetNode cfg net =
    -- Note that `netEdges` assumes that `drawNetNode` implies `drawNetEdges`
    drawNetEdges cfg net &&
    cfgDrawNets cfg &&
    (not onesided || cfgDrawOnesidedNets cfg)
  where
    onesided = S.null (netSources net) || S.null (netSinks net)

drawNetEdges cfg net =
    (not $ last (T.splitOn (T.singleton '.') (netName net))
        `Set.member` cfgHideNamedNets cfg)


joinKey parts = T.intercalate (T.singleton '$') parts
joinGraphId parts = Str $ TL.fromStrict $ joinKey parts

sideKey Source = T.pack "source"
sideKey Sink = T.pack "sink"

portClusterId cfg side = joinGraphId [cfgPrefix cfg, T.pack "ext", sideKey side]

connKey cfg side (ExtPort i) =
    joinKey [cfgPrefix cfg, T.pack "ext", sideKey side, T.pack (show i)]
connKey cfg side (InstPort i j) =
    joinKey [cfgPrefix cfg, T.pack "inst", T.pack (show i), sideKey side, T.pack (show j)]
connKey cfg side (LogicPort i _) =
    --joinKey [cfgPrefix cfg, T.pack "logic", T.pack (show i), sideKey side, T.pack (show j)]
    logicKey cfg i

netKey cfg idx =
    joinKey [cfgPrefix cfg, T.pack "net", T.pack (show idx)]

logicKey cfg idx =
    joinKey [cfgPrefix cfg, T.pack "logic", T.pack (show idx)]

nodeIdKey cfg (NConn side conn) = connKey cfg side conn
nodeIdKey cfg (NNet idx) = netKey cfg idx
nodeIdKey cfg (NLogic idx) = logicKey cfg idx

mkLabel name = Label $ StrLabel $ TL.fromStrict name
mkTooltip name = Tooltip $ TL.fromStrict name

labelStmt name = GA $ GraphAttrs [mkLabel name]


portCluster :: Cfg -> Side -> Text -> Seq PortDecl -> DotStatement Text
portCluster cfg side label ports =
    SG $ DotSG True (Just $ portClusterId cfg side) stmts
  where
    stmts =
        S.mapWithIndex go ports |> labelStmt label
    go idx (PortDecl name _) =
        let key = connKey cfg side (ExtPort idx) in
        let label = name in
        DN $ DotNode key [mkLabel label]

netNode :: Cfg -> Int -> Net -> DotStatement Text
netNode cfg idx net =
    let name = netName net in
    let names = T.lines name in
    let suffix =
            if length names > 1 then
                T.pack " (+" <> T.pack (show $ length names) <> T.pack " more)"
            else T.empty in
    let shortName = head (T.lines name) <> suffix in

    DN $ DotNode (netKey cfg idx) [mkLabel shortName, mkTooltip name]

logicLabel LkOther = T.pack "(logic)"
logicLabel LkNetAlias = T.pack "(net alias)"

logicNode :: Cfg -> Int -> Logic -> DotStatement Text
logicNode cfg idx logic =
    DN $ DotNode (logicKey cfg idx)
        [ mkLabel $ logicLabel $ logicKind logic
        , Shape BoxShape ]

instCluster :: Design -> Cfg -> Int -> ModInst -> Seq (DotStatement Text)
instCluster _ _ _ inst | modInstId inst == -1 = S.empty
instCluster design cfg instIdx inst =
    S.singleton $ SG $ DotSG True (Just clusterId) stmts
  where
    clusterId = joinGraphId [cfgPrefix cfg, T.pack "inst", T.pack $ show instIdx]
    mod = designMods design `S.index` modInstId inst
    stmts =
        S.mapWithIndex (go Sink) (modDeclInputs mod) <>
        S.mapWithIndex (go Source) (modDeclOutputs mod) |>
        labelStmt (modDeclName mod <> T.singleton ' ' <> modInstName inst)
    go side idx port =
        DN $ DotNode (connKey cfg side (InstPort instIdx idx))
            [mkLabel $ portDeclName port]


edgeStmt cfg n1 n2 = DE $ DotEdge (nodeIdKey cfg n1) (nodeIdKey cfg n2) []


graphModule' :: Design -> Cfg -> ModDecl -> Seq (DotStatement Text)
graphModule' design cfg mod =
    portCluster cfg Source tInputs (modDeclInputs mod) <|
    portCluster cfg Sink tOutputs (modDeclOutputs mod) <|
    S.foldMapWithIndex (\idx net ->
        if drawNetNode cfg net then S.singleton $ netNode cfg idx net else S.empty)
        (modDeclNets mod) <>
    (if cfgDrawLogics cfg then S.mapWithIndex (logicNode cfg) (modDeclLogics mod)
        else S.empty) <>
    join (S.mapWithIndex (instCluster design cfg) (modDeclInsts mod)) <>
    fmap (uncurry $ edgeStmt cfg)
        (filterEdges cfg mod $ allNetEdges cfg $ modDeclNets mod)

graphModule :: Design -> Cfg -> ModDecl -> DotGraph Text
graphModule design cfg mod = traceShow mod $
    DotGraph False True (Just $ joinGraphId [cfgPrefix cfg])
        ( graphModule' design cfg mod
        |> labelStmt (modDeclName mod)
        |> GA (GraphAttrs [RankDir FromLeft, RankSep [2.0]])
        )

printGraphviz :: DotGraph Text -> String
printGraphviz g = TL.unpack $ printDotGraph g
