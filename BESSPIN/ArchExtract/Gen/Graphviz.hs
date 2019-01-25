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

import Data.GraphViz.Attributes.Complete hiding (portName)
import Data.GraphViz.Types
import Data.GraphViz.Types.Generalised

import BESSPIN.ArchExtract.Architecture


tSource = T.pack "source"
tSink = T.pack "sink"

tInputs = T.pack "Inputs"
tOutputs = T.pack "Outputs"


-- `NBasicLogic` is used for logic nodes where we hide the ports and only draw
-- a single graph node for the entire thing.
data NodeId = NConn Side Conn | NNet Int | NBasicLogic Int
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
bypassNodes f edges = M.mapMaybeWithKey go edges
  where
    go :: NodeId -> Seq NodeId -> Maybe (Seq NodeId)
    go src _ | f src = Nothing
    go src tgts = Just $ join $ fmap (newTargets (Set.singleton src)) tgts

    -- Get the target(s) that replace `node` in the output.  If `not $ f node`,
    -- then `node` is kept unchanged; otherwise, it is replaced with its
    -- targets in `edges` (this creates the bypassing edges.  `seen` is used to
    -- prevent infinite loops when the graph contains a cycle.
    newTargets :: Set NodeId -> NodeId -> Seq NodeId
    newTargets seen node | node `Set.member` seen =
        if not $ f node then S.singleton node else S.empty
    newTargets seen node | not $ f node = S.singleton node
    newTargets seen node =
        let tgts = fromMaybe S.empty $ M.lookup node edges in
        join $ fmap (newTargets (Set.insert node seen)) tgts

-- Like `bypassNodes`, but deduplicates edges internally for efficiency.
bypassNodesWithDedup :: (NodeId -> Bool) -> EdgeMap -> EdgeMap
bypassNodesWithDedup f edges = M.mapMaybeWithKey go edges
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

logicShowsPorts :: Module -> Int -> Bool
logicShowsPorts mod idx = case logicKind $ mod `moduleLogic` idx of
    LkInst _ -> True
    _ -> False

-- For all `NConn ... LogicPort` nodes where the logic index matches `f`,
-- switch to a `NBasicLogic` `NodeId`.  This is used to get proper
-- deduplication of edges to/from logics that don't show individual ports.
clearLogicPorts :: (Int -> Bool) -> Seq (NodeId, NodeId) -> Seq (NodeId, NodeId)
clearLogicPorts f edges = fmap go edges
  where
    go (a, b) = (goNode a, goNode b)
    goNode (NConn _ (LogicPort i _)) | f i = NBasicLogic i
    goNode n = n

dedupEdges m = fmap go m
  where
    go xs = fst $ foldl (\(acc, seen) x ->
            if not $ x `Set.member` seen then (acc |> x, Set.insert x seen)
            else (acc, seen))
        (S.empty, Set.empty) xs

netEdges :: Cfg -> Int -> Net -> Seq (NodeId, NodeId)
netEdges cfg idx net =
    fmap (\conn -> (NConn Source conn, NNet idx)) (netSources net) <>
    fmap (\conn -> (NNet idx, NConn Sink conn)) (netSinks net)

allNetEdges :: Cfg -> Seq Net -> Seq (NodeId, NodeId)
allNetEdges cfg nets = join $ S.mapWithIndex (netEdges cfg) nets

filterEdges :: Cfg -> Module -> Seq (NodeId, NodeId) -> Seq (NodeId, NodeId)
filterEdges cfg mod edges = edges'
  where
    edgeMap = foldl (\m (s, t) -> M.insertWith (<>) s (S.singleton t) m) M.empty $
        clearLogicPorts (not . logicShowsPorts mod) edges
    bypass = if cfgDedupEdges cfg then bypassNodesWithDedup else bypassNodes
    edgeMap' = flip bypass edgeMap $ \n -> case n of
        NBasicLogic _ -> not $ cfgDrawLogics cfg
        NNet i -> not $ drawNetNode cfg (mod `moduleNet` NetId i)
        _ -> False
    edges' = M.foldlWithKey (\es s ts -> es <> fmap (\t -> (s, t)) ts) S.empty edgeMap'



data Cfg = Cfg
    { cfgPrefix :: Text
    , cfgDrawNets :: Bool
    , cfgDrawOnesidedNets :: Bool
    , cfgDrawLogics :: Bool
    , cfgHideNamedNets :: Set Text
    , cfgDedupEdges :: Bool
    , cfgShortenNetNames :: Bool
    , cfgInstColor :: Int -> Maybe Color
    , cfgLogicColor :: Int -> Maybe Color
    , cfgNetColor :: NetId -> Maybe Color
    }

defaultCfg = Cfg
    { cfgPrefix = T.empty
    , cfgDrawNets = True
    , cfgDrawOnesidedNets = True
    , cfgDrawLogics = True
    , cfgHideNamedNets = Set.empty
    , cfgDedupEdges = False
    , cfgShortenNetNames = True
    , cfgInstColor = const Nothing
    , cfgLogicColor = const Nothing
    , cfgNetColor = const Nothing
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

portClusterId cfg side = joinGraphId [cfgPrefix cfg, T.pack "ext", sideKey side]

sideKey Source = T.pack "source"
sideKey Sink = T.pack "sink"

extKey cfg side idx =
    joinKey [cfgPrefix cfg, T.pack "ext", sideKey side, T.pack (show idx)]

netKey cfg idx =
    joinKey [cfgPrefix cfg, T.pack "net", T.pack (show idx)]

logicKey cfg side idx portIdx =
    joinKey [cfgPrefix cfg, T.pack "inst",
        T.pack (show idx), sideKey side, T.pack (show portIdx)]

basicLogicKey cfg idx =
    joinKey [cfgPrefix cfg, T.pack "logic", T.pack (show idx)]

nodeIdKey cfg (NConn side (ExtPort idx)) = extKey cfg side idx
nodeIdKey cfg (NConn side (LogicPort idx portIdx)) = logicKey cfg side idx portIdx
nodeIdKey cfg (NNet idx) = netKey cfg idx
nodeIdKey cfg (NBasicLogic idx) = basicLogicKey cfg idx

mkLabel name = Label $ StrLabel $ TL.fromStrict name
mkTooltip name = Tooltip $ TL.fromStrict name

attrStmt attrs = GA $ GraphAttrs attrs
labelStmt name = attrStmt [mkLabel name]

convColor :: Maybe Color -> [Attribute]
convColor Nothing = []
convColor (Just c) = [Color $ toColorList [c]]


portCluster :: Cfg -> Side -> Text -> Seq Port -> DotStatement Text
portCluster cfg side label ports =
    SG $ DotSG True (Just $ portClusterId cfg side) stmts
  where
    stmts =
        S.mapWithIndex go ports |> labelStmt label
    go idx (Port name _) =
        let key = extKey cfg side idx in
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
    let displayName = if cfgShortenNetNames cfg then shortName else name in
    let color = convColor $ cfgNetColor cfg $ NetId idx in

    DN $ DotNode (netKey cfg idx) ([mkLabel displayName, mkTooltip name] ++ color)

logicLabel LkOther = T.pack "(logic)"
logicLabel LkNetAlias = T.pack "(net alias)"
logicLabel (LkInst _) = T.pack "(mod inst)"

logicNode :: Design -> Cfg -> Int -> Logic -> Seq (DotStatement Text)
logicNode design cfg idx logic@Logic { logicKind = LkInst inst } =
    S.singleton $ SG $ DotSG True (Just clusterId) stmts
  where
    color = convColor $ cfgInstColor cfg idx
    clusterId = joinGraphId [cfgPrefix cfg, T.pack "inst", T.pack $ show idx]
    mod = design `designMod` instModId inst
    stmts =
        S.mapWithIndex (go Sink) (moduleInputs mod) <>
        S.mapWithIndex (go Source) (moduleOutputs mod) |>
        attrStmt 
            ([mkLabel $ moduleName mod <> T.singleton ' ' <> instName inst] ++ color)
    go side portIdx port =
        DN $ DotNode (logicKey cfg side idx portIdx) [mkLabel $ portName port]
logicNode design cfg idx logic =
    if cfgDrawLogics cfg then S.singleton stmt else S.empty
  where
    color = convColor $ cfgLogicColor cfg idx
    stmt = DN $ DotNode (basicLogicKey cfg idx)
        ([ mkLabel $ logicLabel $ logicKind logic, Shape BoxShape ] ++ color)


edgeStmt cfg n1 n2 = DE $ DotEdge (nodeIdKey cfg n1) (nodeIdKey cfg n2) []


graphModule' :: Design -> Cfg -> Module -> Seq (DotStatement Text)
graphModule' design cfg mod =
    portCluster cfg Source tInputs (moduleInputs mod) <|
    portCluster cfg Sink tOutputs (moduleOutputs mod) <|
    S.foldMapWithIndex (\idx net ->
        if drawNetNode cfg net then S.singleton $ netNode cfg idx net else S.empty)
        (moduleNets mod) <>
    S.foldMapWithIndex (logicNode design cfg) (moduleLogics mod) <>
    fmap (uncurry $ edgeStmt cfg)
        (filterEdges cfg mod $ allNetEdges cfg $ moduleNets mod)

graphModule :: Design -> Cfg -> Module -> DotGraph Text
graphModule design cfg mod =
    DotGraph False True (Just $ joinGraphId [cfgPrefix cfg])
        ( graphModule' design cfg mod
        |> labelStmt (moduleName mod)
        |> GA (GraphAttrs [RankDir FromLeft, RankSep [2.0]])
        )

printGraphviz :: DotGraph Text -> String
printGraphviz g = TL.unpack $ printDotGraph g
