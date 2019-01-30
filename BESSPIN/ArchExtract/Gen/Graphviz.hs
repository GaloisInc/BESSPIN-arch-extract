{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL

import Debug.Trace

import Data.GraphViz.Attributes.Complete hiding (portName)
import qualified Data.GraphViz.Attributes.HTML as H
import Data.GraphViz.Commands
import Data.GraphViz.Types
import Data.GraphViz.Types.Generalised

import BESSPIN.ArchExtract.Architecture


tSource = T.pack "source"
tSink = T.pack "sink"

tInputs = T.pack "Inputs"
tOutputs = T.pack "Outputs"


-- An ID of an endpoint of an edge.  "Port" is the name Graphviz uses for
-- places within a node where edges can begin/end.  `PConn`: a port
-- corresponding to a `Conn`.  `PNet`: the central node representing a `Net`.
-- `PBasicLogic`: the single-port node representing a basic logic element,
-- where the individual ports are not displayed.
data PortId = PConn Side Conn | PNet Int | PBasicLogic Int
    deriving (Show, Eq, Ord)

type EdgeMap = Map PortId (Seq PortId)

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
bypassNodes :: (PortId -> Bool) -> EdgeMap -> EdgeMap
bypassNodes f edges = M.mapMaybeWithKey go edges
  where
    go :: PortId -> Seq PortId -> Maybe (Seq PortId)
    go src _ | f src = Nothing
    go src tgts = Just $ join $ fmap (newTargets (Set.singleton src)) tgts

    -- Get the target(s) that replace `node` in the output.  If `not $ f node`,
    -- then `node` is kept unchanged; otherwise, it is replaced with its
    -- targets in `edges` (this creates the bypassing edges.  `seen` is used to
    -- prevent infinite loops when the graph contains a cycle.
    newTargets :: Set PortId -> PortId -> Seq PortId
    newTargets seen node | node `Set.member` seen =
        if not $ f node then S.singleton node else S.empty
    newTargets seen node | not $ f node = S.singleton node
    newTargets seen node =
        let tgts = fromMaybe S.empty $ M.lookup node edges in
        join $ fmap (newTargets (Set.insert node seen)) tgts

-- Like `bypassNodes`, but deduplicates edges internally for efficiency.
bypassNodesWithDedup :: (PortId -> Bool) -> EdgeMap -> EdgeMap
bypassNodesWithDedup f edges = M.mapMaybeWithKey go edges
  where
    go :: PortId -> Seq PortId -> Maybe (Seq PortId)
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

logicShowsPorts :: Logic a -> Bool
logicShowsPorts l = case logicKind l of
    LkInst _ -> True
    LkRegister _ -> True
    _ -> False

-- For all `PConn ... LogicPort` nodes where the logic index matches `f`,
-- switch to a `PBasicLogic` `PortId`.  This is used to get proper
-- deduplication of edges to/from logics that don't show individual ports.
clearLogicPorts :: (Int -> Bool) -> Seq (PortId, PortId) -> Seq (PortId, PortId)
clearLogicPorts f edges = fmap go edges
  where
    go (a, b) = (goNode a, goNode b)
    goNode (PConn _ (LogicPort i _)) | f i = PBasicLogic i
    goNode n = n

dedupEdges m = fmap go m
  where
    go xs = fst $ foldl (\(acc, seen) x ->
            if not $ x `Set.member` seen then (acc |> x, Set.insert x seen)
            else (acc, seen))
        (S.empty, Set.empty) xs

netEdges :: Cfg -> Int -> Net Ann -> Seq (PortId, PortId)
netEdges cfg idx net =
    fmap (\conn -> (PConn Source conn, PNet idx)) (netSources net) <>
    fmap (\conn -> (PNet idx, PConn Sink conn)) (netSinks net)

allNetEdges :: Cfg -> Seq (Net Ann) -> Seq (PortId, PortId)
allNetEdges cfg nets = join $ S.mapWithIndex (netEdges cfg) nets

filterEdges :: Cfg -> Module Ann -> Seq (PortId, PortId) -> Seq (PortId, PortId)
filterEdges cfg mod edges = edges'
  where
    edgeMap = foldl (\m (s, t) -> M.insertWith (<>) s (S.singleton t) m) M.empty $
        clearLogicPorts (\idx -> not $ logicShowsPorts $ mod `moduleLogic` idx) edges
    bypass = if cfgDedupEdges cfg then bypassNodesWithDedup else bypassNodes
    edgeMap' = flip bypass edgeMap $ \n -> case n of
        PBasicLogic _ -> not $ cfgDrawLogics cfg
        PNet i -> not $ drawNetNode cfg (mod `moduleNet` NetId i)
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
    , cfgPipelineStages :: Int
    }

defaultCfg = Cfg
    { cfgPrefix = T.empty
    , cfgDrawNets = True
    , cfgDrawOnesidedNets = True
    , cfgDrawLogics = True
    , cfgHideNamedNets = Set.empty
    , cfgDedupEdges = False
    , cfgShortenNetNames = True
    , cfgPipelineStages = 0
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


data Ann = Ann
    { annColor :: Maybe Color
    , annPipelineStage :: Maybe Int
    }


joinKey parts = T.intercalate (T.singleton '$') parts
joinGraphId parts = Str $ TL.fromStrict $ joinKey parts

portClusterId cfg side = joinGraphId [cfgPrefix cfg, T.pack "ext", sideKey side]

sideKey Source = T.pack "source"
sideKey Sink = T.pack "sink"

extKey cfg side idx =
    joinKey [cfgPrefix cfg, T.pack "ext", sideKey side, T.pack (show idx)]

netKey cfg idx =
    joinKey [cfgPrefix cfg, T.pack "net", T.pack (show idx)]

logicKey cfg idx =
    joinKey [cfgPrefix cfg, T.pack "logic", T.pack (show idx)]

stageStartKey cfg i =
    joinKey [cfgPrefix cfg,
        "stage" <> T.pack (show i),
        "start"]

stageChainKey cfg i j =
    joinKey [cfgPrefix cfg,
        "stage" <> T.pack (show i),
        "rank" <> T.pack (show j),
        "chain"]

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

portClusters :: Cfg -> Module a -> Seq (DotStatement Text)
portClusters cfg mod =
    S.empty |>
    portCluster cfg Source tInputs (moduleInputs mod) |>
    portCluster cfg Sink tOutputs (moduleOutputs mod)


netNode :: Cfg -> Int -> Net Ann -> Maybe (DotNode Text)
netNode cfg idx net | not $ drawNetNode cfg net = Nothing
netNode cfg idx net =
    let name = netName net in
    let names = T.lines name in
    let suffix =
            if length names > 1 then
                T.pack " (+" <> T.pack (show $ length names) <> T.pack " more)"
            else T.empty in
    let shortName = head (T.lines name) <> suffix in
    let displayName = if cfgShortenNetNames cfg then shortName else name in
    let color = convColor $ annColor $ netAnn net in

    Just $
        DotNode (netKey cfg idx) ([mkLabel displayName, mkTooltip name] ++ color)

netNode' cfg idx net = S.fromList $ toList $ netNode cfg idx net


logicLabel LkOther = T.pack "(logic)"
logicLabel (LkRegister name) = "(register " <> name <> ")"
logicLabel LkNetAlias = T.pack "(net alias)"
logicLabel (LkInst _) = T.pack "(mod inst)"

logicNode :: Design a -> Cfg -> Int -> Logic Ann -> Maybe (DotNode Text)
logicNode design cfg idx logic
    | logicShowsPorts logic = Just $
        logicTableNode cfg design idx logic
    | otherwise = logicBasicNode cfg idx logic

logicNode' design cfg idx logic =
    S.fromList $ toList $ logicNode design cfg idx logic

logicBasicNode :: Cfg -> Int -> Logic Ann -> Maybe (DotNode Text)
logicBasicNode cfg idx logic =
    if cfgDrawLogics cfg then Just node else Nothing
  where
    color = convColor $ annColor $ logicAnn logic
    node = DotNode (logicKey cfg idx)
        ([ mkLabel $ logicLabel $ logicKind logic, Shape BoxShape ] ++ color)

-- List of input port names and output port names for a logic item.  Uses the
-- module declaration's port names for `LkInst`, and numbers otherwise.
logicPortNames :: Design a -> Logic b -> (Seq Text, Seq Text)
logicPortNames d l@(Logic { logicKind = LkInst inst }) =
    (fmap portName $ moduleInputs mod,
        fmap portName $ moduleOutputs mod)
  where
    mod = d `designMod` instModId inst
logicPortNames d l =
    (S.fromList $ map (T.pack . show) [0 .. maxInput],
        S.fromList $ map (T.pack . show) [0 .. maxOutput])
  where
    maxInput = S.length (logicInputs l) - 1
    maxOutput = S.length (logicOutputs l) - 1

logicName :: Design a -> Logic b -> H.Text
logicName d l@(Logic { logicKind = LkInst inst }) =
    [H.Str $ TL.fromStrict $ moduleName $ d `designMod` instModId inst,
        H.Newline [],
        H.Str $ TL.fromStrict $ instName inst]
logicName d l@(Logic { logicKind = LkRegister name }) =
    [H.Str $ TL.fromStrict $ "(register " <> name <> ")"]
logicName _ _ = [H.Str $ TL.fromStrict "(logic)"]

logicTable :: Cfg -> Design a -> Int -> Logic Ann -> H.Label
logicTable cfg d idx l = H.Table $
    H.HTable Nothing [] $ nameRow : concat [[H.HorizontalRule, r] | r <- portRows]
  where
    nameRow = H.Cells [H.LabelCell [H.ColSpan 2] $ H.Text $ logicName d l]
    (ins, outs) = logicPortNames d l
    maxPort = max (S.length ins - 1) (S.length outs - 1)
    portCell side names portIdx = case S.lookup portIdx names of
        Nothing -> H.LabelCell [] $ H.Text [H.Str ""]
        Just name ->
            let port = joinKey [sideKey side, T.pack $ show portIdx] in
            H.LabelCell [H.Port $ PN $ TL.fromStrict port] $
                H.Text [H.Str $ TL.fromStrict name]
    portRow i = H.Cells [portCell Sink ins i, H.VerticalRule, portCell Source outs i]
    portRows = map portRow [0 .. maxPort]

logicTableNode :: Cfg -> Design a -> Int -> Logic Ann -> DotNode Text
logicTableNode cfg d idx l =
    DotNode (logicKey cfg idx)
        ([Label $ HtmlLabel $ logicTable cfg d idx l, Shape PlainText] ++
            convColor (annColor $ logicAnn l))


graphEdge :: Cfg -> PortId -> PortId -> DotEdge Text
graphEdge cfg n1 n2 = DotEdge end1 end2 attrs
  where
    go (PConn side (ExtPort idx)) = (extKey cfg side idx, Nothing)
    go (PConn side (LogicPort idx portIdx)) =
        (logicKey cfg idx, Just $ joinKey [sideKey side, T.pack $ show portIdx])
    go (PNet idx) = (netKey cfg idx, Nothing)
    go (PBasicLogic idx) = (logicKey cfg idx, Nothing)

    (end1, port1) = go n1
    (end2, port2) = go n2

    attrs =
        (case port1 of
            Nothing -> []
            Just port -> [TailPort $ LabelledPort (PN $ TL.fromStrict port) (Just East)])
        ++
        (case port2 of
            Nothing -> []
            Just port -> [HeadPort $ LabelledPort (PN $ TL.fromStrict port) (Just West)])


graphModuleUnstaged :: Design a -> Cfg -> Module Ann -> Seq (DotStatement Text)
graphModuleUnstaged design cfg mod = graphModuleStaged design cfg mod S.empty

graphModuleStaged :: Design a -> Cfg -> Module Ann -> StageLayout -> Seq (DotStatement Text)
graphModuleStaged design cfg mod layout =
    portClusters cfg mod <>
    stageStmts <>
    fmap DN unstagedNodes <>
    fmap (\(a,b) -> DE $ graphEdge cfg a b)
        (filterEdges cfg mod $ allNetEdges cfg $ moduleNets mod) <>
    chainEdges <>
    interChainEdges <>
    chainStartEdge <>
    chainEndEdge
  where
    stageStmts = flip S.foldMapWithIndex layout $ \i stage ->
        (DN (DotNode (stageStartKey cfg i) [invis]) <|) $
        flip S.foldMapWithIndex stage $ \j rank ->
            let gi = joinGraphId [cfgPrefix cfg,
                    "stage" <> T.pack (show i),
                    "rank" <> T.pack (show j)] in
            let label = T.pack (show i) <> "." <> T.pack (show j) in
            S.singleton $ SG $ DotSG False (Just gi) $
                GA (GraphAttrs [Rank SameRank]) <|
                DN (DotNode (stageChainKey cfg i j) [mkLabel label, invis]) <|
                foldMap (fmap DN . mkNode) rank

    --invis = Style [SItem Dotted []]
    invis = Style [SItem Invisible []]
    heavy = Weight $ Dbl 10.0

    -- We use the actual contents of `layout` instead of `annPipelineStage`, to
    -- handle the case where nodes are annotated with out-of-range stages:
    -- those nodes have `annPipelineStage` set, but don't appear in `layout`.
    stagedIds = foldMap (foldMap (foldMap Set.singleton)) layout

    unstagedNodes =
        S.foldMapWithIndex (\idx net ->
            if not $ Set.member (NNet $ NetId idx) stagedIds then
                netNode' cfg idx net else S.empty) (moduleNets mod) <>
        S.foldMapWithIndex (\idx logic ->
            if not $ Set.member (NLogic idx) stagedIds then
                logicNode' design cfg idx logic else S.empty) (moduleLogics mod)

    chainEdges = flip S.foldMapWithIndex layout $ \i stage ->
        flip foldMap [0 .. S.length stage - 1] $ \j ->
            let prev = if j > 0 then stageChainKey cfg i (j - 1)
                    else stageStartKey cfg i in
            S.singleton $ DE $ DotEdge prev (stageChainKey cfg i j) [invis, heavy]

    stageLastKey cfg i stage =
        if len > 0 then stageChainKey cfg i (len - 1)
            else stageStartKey cfg i
      where
        len = S.length stage

    interChainEdges = flip S.foldMapWithIndex layout $ \i stage ->
        if i < S.length layout - 1 then
            S.singleton $ DE $
                DotEdge (stageLastKey cfg i stage) (stageStartKey cfg (i + 1)) [invis, heavy]
        else S.empty

    chainStartEdge :: Seq (DotStatement Text)
    chainStartEdge = S.fromList $ maybeToList $ do
        guard $ not $ S.null layout
        guard $ not $ S.null $ moduleInputs mod
        return $ DE $ DotEdge (extKey cfg Source 0) (stageStartKey cfg 0) [invis, heavy]

    chainEndEdge :: Seq (DotStatement Text)
    chainEndEdge = S.fromList $ maybeToList $ do
        let i = S.length layout - 1
        stage <- layout S.!? i
        guard $ not $ S.null $ moduleOutputs mod
        return $ DE $ DotEdge (stageLastKey cfg i stage) (extKey cfg Sink 0) [invis, heavy]

    mkNode (NLogic idx) = logicNode' design cfg idx (mod `moduleLogic` idx)
    mkNode (NNet i) = netNode' cfg (unwrapNetId i) (mod `moduleNet` i)


-- Combined ID type for both logic and net nodes.  Used for tracking nodes
-- through the pipeline-stage layout process.
data NodeId = NLogic Int | NNet NetId
    deriving (Show, Eq, Ord)

nodePipelineStage mod (NLogic idx) =
    annPipelineStage $ logicAnn $ mod `moduleLogic` idx
nodePipelineStage mod (NNet i) =
    annPipelineStage $ netAnn $ mod `moduleNet` i

-- For each stage (0 .. cfgPipelineStages - 1), for each rank, a list of nodes
-- present in that rank of that stage.
type StageLayout = Seq (Seq (Seq NodeId))

-- Given a module, compute the pipeline stage and (intra-stage) node rank for
-- each net and logic node in the module's output graph.  The output map only
-- contains entries for those nodes that have a (non-`Nothing`) pipeline stage
-- annotation.
layoutModule :: Design a -> Cfg -> Module Ann -> IO StageLayout
layoutModule design cfg mod = graphvizWithHandle Dot g DotOutput $ \h -> do
    t <- T.hGetContents h
    return $ go $ parseDotGraphLiberally $ TL.fromStrict t
  where
    netNodes :: Seq (DotNode Text)
    netKeyMap :: Map Text NodeId
    (netNodes, netKeyMap) = S.foldMapWithIndex (\idx net ->
            case netNode cfg idx net of
                Nothing -> (S.empty, M.empty)
                Just n -> (S.singleton n, M.singleton (netKey cfg idx) (NNet $ NetId idx)))
        (moduleNets mod)

    (logicNodes, logicKeyMap) = S.foldMapWithIndex (\idx logic ->
            case logicNode design cfg idx logic of
                Nothing -> (S.empty, M.empty)
                Just n -> (S.singleton n, M.singleton (logicKey cfg idx) (NLogic idx)))
        (moduleLogics mod)

    stmts =
        portClusters cfg mod <>
        fmap DN netNodes <>
        fmap DN logicNodes <>
        fmap (\(a,b) -> DE $ graphEdge cfg a b)
            (filterEdges cfg mod $ allNetEdges cfg $ moduleNets mod)

    keyMap = netKeyMap <> logicKeyMap

    g = DotGraph False True (Just $ joinGraphId [cfgPrefix cfg])
        ( stmts
        |> GA (GraphAttrs [RankDir FromLeft])
        )

    go :: DotGraph Text -> StageLayout
    go g' = mkLayout $ mkPosMap $ collectPos g'

    -- Output is `[(stage index, x-position, node ID)]`.
    collectPos :: DotGraph Text -> [(Int, Double, NodeId)]
    collectPos g' = flip concatMap (toList $ graphStatements g') (\s -> case s of
        DN n -> maybeToList $ do
            i <- M.lookup (nodeID n) keyMap
            (x,y) <- graphNodePos n
            stage <- nodePipelineStage mod i
            return $ (stage, x, i)
        _ -> [])

    mkPosMap posList =
        M.unionsWith (M.unionWith (<>)) $
        map (\(s,x,i) -> M.singleton s (M.singleton x (S.singleton i))) $
        posList

    mkLayout :: Map Int (Map Double (Seq NodeId)) -> Seq (Seq (Seq NodeId))
    mkLayout posMap =
        S.fromList $
        map (\i -> maybe S.empty (S.fromList . M.elems) $ M.lookup i posMap) $
        [0 .. cfgPipelineStages cfg - 1]

graphNodePos :: DotNode a -> Maybe (Double, Double)
graphNodePos n = getFirst $ foldMap (\att -> case att of
    Pos (PointPos p) -> First $ Just (xCoord p, yCoord p)
    _ -> First $ Nothing) (nodeAttributes n)

graphModule :: Design a -> Cfg -> Module Ann -> IO (DotGraph Text)
graphModule design cfg mod = do
    layout <- layoutModule design cfg mod
    let stmts = graphModuleStaged design cfg mod layout
    return $ DotGraph False True (Just $ joinGraphId [cfgPrefix cfg])
        ( stmts
        |> labelStmt (moduleName mod)
        |> GA (GraphAttrs [RankDir FromLeft, RankSep [1.5]])
        )

printGraphviz :: DotGraph Text -> String
printGraphviz g = TL.unpack $ printDotGraph g
