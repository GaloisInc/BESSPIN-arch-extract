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
import qualified BESSPIN.ArchExtract.Config as Config


tSource = T.pack "source"
tSink = T.pack "sink"

tInputs = T.pack "Inputs"
tOutputs = T.pack "Outputs"


-- An ID of an endpoint of an edge.  "Port" is the name Graphviz uses for
-- places within a node where edges can begin/end.
data PortId =
    -- A port corresponding to a `Conn`.
      PConn Side Conn
    -- The central node representing a `Net`.
    | PNet Int
    -- The single-port node representing a basic logic element, where the
    -- individual ports are not displayed.
    | PBasicLogic Int
    -- The single-port node representing all external ports on the input/output
    -- side, where the individual ports are not displayed.
    | PBasicExt Side
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

logicShowsPorts :: Cfg -> Logic a -> Bool
logicShowsPorts cfg l =
    cfgDrawLogicPorts cfg &&
    case logicKind l of
        LkInst _ -> True
        LkRegister _ -> True
        LkDFlipFlop _ _ -> True
        LkRam _ _ _ _ _ -> True
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

clearExtPorts :: (Side -> Bool) -> Seq (PortId, PortId) -> Seq (PortId, PortId)
clearExtPorts f edges = fmap go edges
  where
    go (a, b) = (goNode a, goNode b)
    goNode (PConn side (ExtPort _)) | f side = PBasicExt side
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
        clearLogicPorts (\idx -> not $ logicShowsPorts cfg $ mod `moduleLogic` idx) $
        clearExtPorts (\_ -> not $ cfgDrawExtPorts cfg) $
        edges
    bypass = if cfgDedupEdges cfg then bypassNodesWithDedup else bypassNodes
    edgeMap' = flip bypass edgeMap $ \n -> case n of
        PBasicLogic i -> not $ drawLogicNode cfg (mod `moduleLogic` i)
        PNet i -> not $ drawNetNode cfg (mod `moduleNet` NetId i)
        _ -> False
    edges' = M.foldlWithKey (\es s ts -> es <> fmap (\t -> (s, t)) ts) S.empty edgeMap'



data Cfg = Cfg
    { cfgPrefix :: Text
    , cfgDrawNets :: Bool
    , cfgDrawOnesidedNets :: Bool
    , cfgDrawLogics :: Bool
    , cfgDrawLogicPorts :: Bool
    , cfgDrawExtPorts :: Bool
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
    , cfgDrawLogicPorts = True
    , cfgDrawExtPorts = True
    , cfgHideNamedNets = Set.empty
    , cfgDedupEdges = False
    , cfgShortenNetNames = True
    , cfgPipelineStages = 0
    }

fromConfig :: Config.Graphviz -> Cfg
fromConfig g = Cfg
    { cfgPrefix = T.empty
    , cfgDrawNets = Config.graphvizDrawNets g
    , cfgDrawOnesidedNets = Config.graphvizDrawOnesidedNets g
    , cfgDrawLogics = Config.graphvizDrawLogics g
    , cfgDrawLogicPorts = Config.graphvizDrawLogicPorts g
    , cfgDrawExtPorts = Config.graphvizDrawExtPorts g
    , cfgHideNamedNets = Set.empty
    , cfgDedupEdges = Config.graphvizDedupEdges g
    , cfgShortenNetNames = Config.graphvizShortenNetNames g
    , cfgPipelineStages = Config.graphvizNumPipelineStages g
    }

drawNetNode cfg net =
    -- Note that `netEdges` assumes that `drawNetNode` implies `drawNetEdges`
    drawNetEdges cfg net &&
    cfgDrawNets cfg &&
    (not onesided || cfgDrawOnesidedNets cfg)
  where
    onesided = S.null (netSources net) || S.null (netSinks net)

drawLogicNode cfg logic =
    cfgDrawLogics cfg ||
    case logicKind logic of
        LkInst _ -> True
        LkRegister _ -> True
        LkDFlipFlop _ _ -> True
        LkRam _ _ _ _ _ -> True
        _ -> False

drawNetEdges cfg net =
    (not $ last (T.splitOn (T.singleton '.') (netName net))
        `Set.member` cfgHideNamedNets cfg)


data Ann = Ann
    { annColor :: Maybe Color
    -- Which pipeline stage this node is in, if known.
    , annPipelineStage :: Maybe Int

    -- The rank of this node in its pipeline stage.  Populated by
    -- `layoutModule`.
    , annPipelineRank :: Maybe Int
    }
    deriving (Show)

defaultAnn = Ann
    { annColor = Nothing
    , annPipelineStage = Nothing
    , annPipelineRank = Nothing
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

outputKey cfg = joinKey [cfgPrefix cfg, "output"]

mkLabel name = Label $ StrLabel $ TL.fromStrict name
mkTooltip name = Tooltip $ TL.fromStrict name

attrStmt attrs = GA $ GraphAttrs attrs
labelStmt name = attrStmt [mkLabel name]

convColor :: Maybe Color -> [Attribute]
convColor Nothing = []
convColor (Just c) = [Color $ toColorList [c]]


portNode :: Cfg -> Side -> Int -> Port -> DotNode Text
portNode cfg side idx (Port name _ _) =
    let key = extKey cfg side idx in
    let label = name in
    DotNode key [mkLabel label]

basicPortNode :: Cfg -> Side -> DotNode Text
basicPortNode cfg side =
    let key = extKey cfg side 0 in
    DotNode key
        [ mkLabel $ if side == Source then "Inputs" else "Outputs"
        , Shape BoxShape
        ]


netNode :: Cfg -> Design a -> Module b -> Int -> Net Ann -> Maybe (DotNode Text)
netNode cfg d m idx net | not $ drawNetNode cfg net = Nothing
netNode cfg d m idx net =
    let name = netName net in
    let names = T.lines name in
    let suffix =
            if length names > 1 then
                T.pack " (+" <> T.pack (show $ length names) <> T.pack " more)"
            else T.empty in
    let shortName = head (T.lines name) <> suffix in
    let displayName = if cfgShortenNetNames cfg then shortName else name in
    let color = convColor $ annColor $ netAnn net in
    let tyStr = printTy (printVar d m) $ netTy net in

    Just $
        DotNode (netKey cfg idx) ([mkLabel $ T.unlines [displayName, tyStr],
            mkTooltip name] ++ color)


logicLabel LkOther = T.pack "(logic)"
logicLabel LkExpr = T.pack "(expr)"
logicLabel (LkRegister name) = "(register " <> name <> ")"
logicLabel (LkDFlipFlop name _) = "(dff " <> name <> ")"
logicLabel (LkRam name _ _ _ _) = "(ram " <> name <> ")"
logicLabel LkNetAlias = T.pack "(net alias)"
logicLabel (LkInst inst) = "(inst " <> instName inst <> ")"

logicNode :: Design a -> Cfg -> Module b -> Int -> Logic Ann -> Maybe (DotNode Text)
logicNode design cfg mod idx logic
    | not $ drawLogicNode cfg logic = Nothing
    | logicShowsPorts cfg logic = Just $
        logicTableNode cfg design mod idx logic
    | otherwise = Just $ logicBasicNode cfg idx logic

logicBasicNode :: Cfg -> Int -> Logic Ann -> DotNode Text
logicBasicNode cfg idx logic =
    DotNode (logicKey cfg idx)
        ([ mkLabel $ logicLabel $ logicKind logic, Shape BoxShape ] ++ color)
  where
    color = convColor $ annColor $ logicAnn logic

-- List of input port names and output port names for a logic item.  Uses the
-- module declaration's port names for `LkInst`, and numbers otherwise.
logicPortNames :: Design a -> Logic b -> (Seq Text, Seq Text)
logicPortNames d l@(Logic { logicKind = LkInst inst }) =
    (fmap portName $ moduleInputs mod,
        fmap portName $ moduleOutputs mod)
  where
    mod = d `designMod` instModId inst
logicPortNames d l@(Logic { logicKind = LkDFlipFlop _ numResets }) =
    ( "D" <| "clk" <| S.fromList ["rst" <> T.pack (show i) | i <- [0 .. numResets - 1]]
    , "Q" <| S.empty )
logicPortNames d l@(Logic { logicKind = LkRam _ _ resets readPorts writePorts }) =
    ( "RAM" <| "clk"
        <| S.fromList ["rst" <> T.pack (show i) | i <- [0 .. resets - 1]]
        <> S.fromList ["ra" <> T.pack (show i) | i <- [0 .. readPorts - 1]]
        <> S.fromList (concat [["wa" <> i, "wd" <> i, "we" <> i]
            | i <- map (T.pack . show) [0 .. writePorts - 1]])
    , "RAM"
        <| S.fromList ["rd" <> T.pack (show i) | i <- [0 .. readPorts - 1]]
        )
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
logicName d l@(Logic { logicKind = LkDFlipFlop name _ }) =
    [H.Str $ TL.fromStrict $ "(dff " <> name <> ")"]
logicName d l@(Logic { logicKind = LkRam name depth _ _ _ }) =
    [H.Str $ TL.fromStrict $ "(ram " <> name <> ")",
        H.Newline [],
        H.Str $ TL.fromStrict $ "x" <> printConstExpr (\_ _ -> "?") depth]
logicName _ _ = [H.Str $ TL.fromStrict "(logic)"]

printVar :: Design a -> Module b -> [Int] -> Int -> Text
printVar d m insts param | traceShow ("printvar", fmap logicKind $ moduleLogics m, moduleParams m, insts, param) False = undefined
printVar d m [] param = paramName $ m `moduleParam` param
printVar d m (instIdx : insts) param =
    --let LkInst inst = logicKind $ m `moduleLogic` instIdx in
    let inst = case logicKind $ m `moduleLogic` instIdx of
            LkInst inst -> inst
            lk -> error $ "bad logic kind " ++ show lk ++ " at " ++ show (instIdx, insts, param)
    in
    let instMod = d `designMod` instModId inst in
    instName inst <> "$" <> printVar d instMod insts param

printConstExpr :: ([Int] -> Int -> Text) -> ConstExpr -> Text
printConstExpr printVar e = go e
  where
    go (EIntLit _ i) = T.pack $ show i
    go (EParam _ i) = printVar [] i
    go (EInstParam _ insts i) = printVar insts i
    go (EUnArith _ op e) =
        "(" <> T.unwords [T.pack (show op), go e] <> ")"
    go (EBinArith _ op l r) =
        "(" <> T.unwords [go l, "`" <> T.pack (show op) <> "`", go r] <> ")"
    go (EBinCmp _ op l r) =
        "(" <> T.unwords [go l, "`" <> T.pack (show op) <> "`", go r] <> ")"
    go (ERangeSize _ l r) =
        "(" <> T.unwords ["rangeSize", go l, go r] <> ")"

printTy :: ([Int] -> Int -> Text) -> Ty -> Text
printTy printVar t = go t
  where
    go (TWire ws ds) = mconcat $
        ["wire"] ++ map (\e -> "[" <> goExpr e <> "]") (ws ++ ds)
    go (TEnum base) = "enum " <> go base
    go (TAlias name base) = "typedef(" <> name <> ") " <> go base
    go TSimVal = "sim"
    go TUnknown = "unknown"

    goExpr e = printConstExpr printVar e

logicParamLines :: Design a -> Module b -> Logic c -> [H.Text]
logicParamLines d m l@(Logic { logicKind = LkInst inst }) =
    toList $ S.mapWithIndex go $ instParams inst
  where
    instMod = d `designMod` instModId inst
    parentParamName i = printVar d m [] i
    instParamName i = "self$" <> printVar d instMod [] i

    go idx optExpr = [H.Str $ TL.fromStrict $
        instParamName idx <> " = " <> exprText idx optExpr]

    exprText idx (Just e) = printConstExpr (printVar d m) e
    exprText idx Nothing = case paramDefault $ instMod `moduleParam` idx of
        Just e -> printConstExpr (printVar d instMod) e
        Nothing -> "(unset??)"
logicParamLines d _ _ = []

logicTable :: Cfg -> Design a -> Module b -> Int -> Logic Ann -> H.Label
logicTable cfg d m idx l = H.Table $
    H.HTable Nothing [] $ nameRow : paramCells ++ concat [[H.HorizontalRule, r] | r <- portRows]
  where
    nameRow = H.Cells [H.LabelCell [H.ColSpan 2] $ H.Text $ logicName d l]
    params = logicParamLines d m l
    paramCells = map (\t -> H.Cells [H.LabelCell [H.ColSpan 2] $ H.Text t]) params
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

logicTableNode :: Cfg -> Design a -> Module b -> Int -> Logic Ann -> DotNode Text
logicTableNode cfg d m idx l =
    DotNode (logicKey cfg idx)
        ([Label $ HtmlLabel $ logicTable cfg d m idx l, Shape PlainText] ++
            convColor (annColor $ logicAnn l))


graphEdge :: Cfg -> Module a -> PortId -> PortId -> DotEdge Text
graphEdge cfg mod n1 n2 = DotEdge end1 end2 attrs
  where
    go (PConn side (ExtPort idx)) =
        (extKey cfg side idx,
            Nothing,
            Just $ portTy $ moduleSidePort mod side idx)
    go (PConn side (LogicPort idx portIdx)) =
        (logicKey cfg idx,
            Just $ joinKey [sideKey side, T.pack $ show portIdx],
            Just $ pinTy $ logicSidePin (mod `moduleLogic` idx) side portIdx)
    go (PNet idx) =
        (netKey cfg idx,
            Nothing,
            Just $ netTy $ mod `moduleNet` NetId idx)
    go (PBasicLogic idx) = (logicKey cfg idx, Nothing, Nothing)
    go (PBasicExt side) = (extKey cfg side 0, Nothing, Nothing)

    (end1, port1, ty1) = go n1
    (end2, port2, ty2) = go n2

    goPort portAttr portSide Nothing = []
    goPort portAttr portSide (Just port) =
        [portAttr $ LabelledPort (PN $ TL.fromStrict port) (Just portSide)]

    goTy labelAttr ty = case ty of
        Nothing -> []
        Just (TWire [] []) -> []
        Just (TWire w d) -> [labelAttr $ StrLabel $ TL.fromStrict $ busLabel w d, bold]
        Just (TEnum ty) -> goTy labelAttr (Just ty)
        Just (TAlias _ ty) -> goTy labelAttr (Just ty)
        Just TSimVal -> [gray]
        Just TUnknown -> []
      where
        -- TODO: show dimension expressions
        busLabel [] [] = ""
        busLabel _ [] = "*"
        busLabel [] _ = "1x*"
        busLabel _ _ = "*x*"
        bold = Style [SItem Bold []]
        gray = Color $ toColorList [RGB 150 150 150]

    attrs =
        goPort TailPort East port1 ++
        goPort HeadPort West port2 ++
        goTy TailLabel ty1 ++
        goTy HeadLabel ty2


-- Combined ID type for all nodes that appear in the generated graph.  Useful
-- for tracking nodes through the multi-stage layout process.
data NodeId =
    NExtPort Side Int |
    NLogic Int |
    NNet NetId |

    -- The rest of these nodes are invisible "rank markers", used to fix the
    -- ordering of other nodes in the final graphviz layout.  Specifically, we
    -- emit a chain of rank markers `A -> B -> C` with high edge weights, so
    -- they always end up on different ranks in the desired order, then force
    -- other nodes in the graph onto those ranks by putting them in a
    -- `rank=same` subgraph together with one of the rank markers.

    -- Rank marker for the start of a pipeline stage.
    NrmStageStart Int |
    -- Rank marker for a given rank within a pipeline stage.
    NrmStageRank Int Int |
    -- Rank marker for the output cluster.  Used to force the outputs to be to
    -- the right of the last pipeline stage.  (`NStageStart 0` serves a similar
    -- role for the inputs and the first stage.)
    NrmOutputRank
    deriving (Show, Eq, Ord)

-- During graph output, we divide the nodes into buckets, then output (most)
-- buckets together as clusters or subgraphs.
data Bucket =
    -- Emit the node at top level, not grouped with any other nodes.
    BFree |
    -- Emit the node as part of the external port cluster on the given side.
    BExtPort Side |
    -- Emit the node as part of the pipeline stage `rank=same` subgraph for the
    -- given stage (first `Int`) and rank (second `Int`).
    BPipeline Int Int |
    -- Emit the node as part of the pipeline start subgraph for the given
    -- stage.  Note that `BPipelineStart 0` and `BExtPort Source` are actually
    -- emitted together.
    BPipelineStart Int |
    -- Emit the node with the same rank as the outputs, but outside the output
    -- cluster.
    BOutputRank
    deriving (Show, Eq, Ord)

annBucket ann = case (annPipelineStage ann, annPipelineRank ann) of
    (Just stage, Just rank) -> BPipeline stage rank
    _ -> BFree

nodeBucket _ (NExtPort side _) = BExtPort side
nodeBucket mod (NLogic idx) =
    annBucket $ logicAnn $ mod `moduleLogic` idx
nodeBucket mod (NNet i) =
    annBucket $ netAnn $ mod `moduleNet` i
nodeBucket _ (NrmStageStart i) = BPipelineStart i
nodeBucket _ (NrmStageRank i j) = BPipeline i j
nodeBucket _ NrmOutputRank = BOutputRank

-- Short label, mainly used for debug displays of rank markers.
nodeShortLabel (NExtPort Source i) = "i" <> T.pack (show i)
nodeShortLabel (NExtPort Sink i) = "o" <> T.pack (show i)
nodeShortLabel (NLogic i) = "l" <> T.pack (show i)
nodeShortLabel (NNet (NetId i)) = "n" <> T.pack (show i)
nodeShortLabel (NrmStageStart i) = "s" <> T.pack (show i)
nodeShortLabel (NrmStageRank i j) = "s" <> T.pack (show i) <> "." <> T.pack (show j)
nodeShortLabel NrmOutputRank = "out"

nodeKey cfg (NExtPort side i) = extKey cfg side i
nodeKey cfg (NLogic idx) = logicKey cfg idx
nodeKey cfg (NNet (NetId idx)) = netKey cfg idx
nodeKey cfg (NrmStageStart i) = stageStartKey cfg i
nodeKey cfg (NrmStageRank i j) = stageChainKey cfg i j
nodeKey cfg NrmOutputRank = outputKey cfg

bucketKey cfg BFree = joinKey [cfgPrefix cfg, "free"]
bucketKey cfg (BExtPort side) = joinKey [cfgPrefix cfg, "ext", sideKey side]
bucketKey cfg (BPipeline i j) = joinKey
    [cfgPrefix cfg, "stage", T.pack $ show i, "rank", T.pack $ show j]
bucketKey cfg (BPipelineStart i) =
    joinKey [cfgPrefix cfg, "stage", T.pack $ show i, "start"]
bucketKey cfg BOutputRank = joinKey [cfgPrefix cfg, "output"]


bucketNodes :: Module Ann -> [(NodeId, DotNode Text)] -> Map Bucket (Seq (DotNode Text))
bucketNodes mod xs = M.unionsWith (<>) $ map (\(i,n) ->
    M.singleton (nodeBucket mod i) (S.singleton n)) xs

modulePortNodes :: Cfg -> Module Ann -> [(NodeId, DotNode Text)]
modulePortNodes cfg mod =
    go Source (moduleInputs mod) <>
    go Sink (moduleOutputs mod)
  where
    go side ports
      | cfgDrawExtPorts cfg = S.foldMapWithIndex (\idx port ->
        [(NExtPort side idx, portNode cfg side idx port)]) ports
      | otherwise = [(NExtPort side 0, basicPortNode cfg side)]


moduleLogicNodes :: Design a -> Cfg -> Module Ann -> [(NodeId, DotNode Text)]
moduleLogicNodes design cfg mod =
    S.foldMapWithIndex (\idx logic -> case logicNode design cfg mod idx logic of
            Nothing -> []
            Just n -> [(NLogic idx, n)])
        (moduleLogics mod)

moduleNetNodes :: Cfg -> Design a -> Module Ann -> [(NodeId, DotNode Text)]
moduleNetNodes cfg d mod =
    S.foldMapWithIndex (\idx net -> case netNode cfg d mod idx net of
            Nothing -> []
            Just n -> [(NNet $ NetId idx, n)])
        (moduleNets mod)

moduleEdges :: Cfg -> Module Ann -> Seq (DotEdge Text)
moduleEdges cfg mod =
    fmap (\(a, b) -> graphEdge cfg mod a b) $
        (filterEdges cfg mod $ allNetEdges cfg $ moduleNets mod)


invisNodeEntry :: Cfg -> NodeId -> (NodeId, DotNode Text)
invisNodeEntry cfg n =
    (n, DotNode (nodeKey cfg n) [mkLabel $ nodeShortLabel n, invis])

invis = Style [SItem Invisible []]
--invis = Style [SItem Dotted []]

countStageLens :: Module Ann -> [NodeId] -> [Int]
countStageLens mod ns = [fromMaybe 0 $ M.lookup i lenMap | i <- [0 .. maxStage]]
  where
    lenMap = M.unionsWith max $ map (\n -> case nodeBucket mod n of
        BPipeline i j -> M.singleton i (j + 1)
        _ -> M.empty) ns
    maxStage = maximum $ 0 : M.keys lenMap

-- Generate the rank-marker chain for pipeline stages, including the final
-- `NrmOutputRank` node.  `stageMax` gives the number of ranks in each stage.
stageChainNodes :: Cfg -> [Int] -> [(NodeId, DotNode Text)]
stageChainNodes cfg stageLens =
    concatMap go (zip [0..] stageLens) ++ [invisNodeEntry cfg NrmOutputRank]
  where
    go (stage, ranks) =
        [invisNodeEntry cfg $ NrmStageStart stage] ++
        [invisNodeEntry cfg $ NrmStageRank stage rank | rank <- [0 .. ranks - 1]]

stageChainEdges :: Cfg -> [Int] -> Seq (DotEdge Text)
stageChainEdges cfg stageLens =
    let ns = map fst $ stageChainNodes cfg stageLens in
    S.fromList $ zipWith (\a b ->
            DotEdge (nodeKey cfg a) (nodeKey cfg b) [invis, Weight $ Dbl 10.0])
        ns (tail ns)

mkConstraint :: [Text] -> Seq (DotStatement Text) -> DotStatement Text
mkConstraint idParts stmts =
    SG $ DotSG False (Just $ joinGraphId idParts)
        (GA (GraphAttrs [Rank SameRank]) <| stmts)

mkCluster :: [Text] -> Seq (DotStatement Text) -> DotStatement Text
mkCluster idParts stmts =
    SG $ DotSG True (Just $ joinGraphId idParts) stmts


-- Generate the Graphviz graph for a module.
moduleGraph design cfg mod =
    DotGraph False True (Just $ joinGraphId [cfgPrefix cfg])
        ( moduleGraph' design cfg mod
        |> labelStmt (moduleName mod)
        |> GA (GraphAttrs [RankDir FromLeft, RankSep [1.5],
            customAttribute "newrank" "true"])
        )

moduleGraph' :: Design a -> Cfg -> Module Ann -> Seq (DotStatement Text)
moduleGraph' design cfg mod =
    mkConstraint [bucketKey cfg $ BExtPort Source, "outer"]
        (mkCluster [bucketKey cfg $ BExtPort Source] (bucketStmts $ BExtPort Source) <|
            bucketStmts (BPipelineStart 0)) <|
    mkConstraint [bucketKey cfg $ BExtPort Sink, "outer"]
        (mkCluster [bucketKey cfg $ BExtPort Sink] (bucketStmts $ BExtPort Sink) <|
            bucketStmts BOutputRank) <|
    fmap (\b -> mkConstraint [bucketKey cfg b] $ bucketStmts b) pipelineBuckets <>
    bucketStmts BFree <>
    fmap DE allEdges
  where
    modNodes =
        modulePortNodes cfg mod ++
        moduleLogicNodes design cfg mod ++
        moduleNetNodes cfg design mod
    stageLens = countStageLens mod (map fst modNodes)
    allNodes = modNodes ++ stageChainNodes cfg stageLens

    allEdges = moduleEdges cfg mod <> stageChainEdges cfg stageLens

    buckets :: Map Bucket (Seq (DotNode Text))
    buckets = bucketNodes mod allNodes

    bucketStmts :: Bucket -> Seq (DotStatement Text)
    bucketStmts b = fmap DN $ fromMaybe S.empty $ M.lookup b buckets

    pre = cfgPrefix cfg

    -- `tail` skips the `BPipelineStart 0` bucket, which is included as part of
    -- the input-port rank.
    pipelineBuckets = S.fromList $ tail $ concat $
        zipWith (\i len -> BPipelineStart i : [BPipeline i j | j <- [0 .. len - 1]])
            [0..] stageLens


-- Given a module, compute the pipeline stage and (intra-stage) node rank for
-- each net and logic node in the module's output graph, and add it to the
-- `annPipelineRank` field of each node's `Ann`.
layoutModule :: Design a -> Cfg -> Module Ann -> IO (Module Ann)
layoutModule design cfg mod = graphvizWithHandle Dot g DotOutput $ \h -> do
    t <- T.hGetContents h
    return $ updateRanks cfg (parseDotGraphLiberally $ TL.fromStrict t) mod
  where
    g = moduleGraph design cfg mod

updateRanks :: Cfg -> DotGraph Text -> Module Ann -> Module Ann
updateRanks cfg g mod = mod
    { moduleLogics = S.mapWithIndex goLogic $ moduleLogics mod
    , moduleNets = S.mapWithIndex goNet $ moduleNets mod
    }
  where
    nodeAttrs :: Map Text [Attribute]
    nodeAttrs = fmap snd $ nodeInformation False g

    logicStageRanks :: [(Int, Double, NodeId)]
    logicStageRanks = flip S.foldMapWithIndex (moduleLogics mod) $ \idx l ->
        fromMaybe [] $ do
            stage <- annPipelineStage $ logicAnn l
            attrs <- M.lookup (logicKey cfg idx) nodeAttrs
            (x, y) <- attrPos attrs
            return $ [(stage, x, NLogic idx)]

    netStageRanks :: [(Int, Double, NodeId)]
    netStageRanks = flip S.foldMapWithIndex (moduleNets mod) $ \idx l ->
        fromMaybe [] $ do
            stage <- annPipelineStage $ netAnn l
            attrs <- M.lookup (netKey cfg idx) nodeAttrs
            (x, y) <- attrPos attrs
            return $ [(stage, x, NNet $ NetId idx)]

    srByStage :: Map Int [(Double, NodeId)]
    srByStage = M.fromListWith (<>) $
        map (\(s,x,i) -> (s, [(x, i)])) $
        logicStageRanks ++ netStageRanks

    indexRanks :: [(Double, NodeId)] -> Map NodeId Int
    indexRanks xs = M.fromList $ do
        (rank, is) <- zip [0..] $ M.elems $
            M.fromListWith (<>) $ map (\(x,i) -> (x, [i])) xs
        i <- is
        return (i, rank)

    nodeRank :: Map NodeId Int
    nodeRank = mconcat $ map indexRanks $ M.elems srByStage

    goLogic idx l = case M.lookup (NLogic idx) nodeRank of
        Just rank -> l { logicAnn = (logicAnn l) { annPipelineRank = Just rank } }
        Nothing -> l

    goNet idx l = case M.lookup (NNet $ NetId idx) nodeRank of
        Just rank -> l { netAnn = (netAnn l) { annPipelineRank = Just rank } }
        Nothing -> l


attrPos :: [Attribute] -> Maybe (Double, Double)
attrPos attrs = getFirst $ foldMap (\attr -> case attr of
    Pos (PointPos p) -> First $ Just (xCoord p, yCoord p)
    _ -> First $ Nothing) attrs


graphModule :: Design a -> Config.Graphviz -> Module Ann -> IO (DotGraph Text)
graphModule design cfg' mod = do
    let cfg = fromConfig cfg'
    mod' <- layoutModule design cfg mod
    return $ moduleGraph design cfg mod'

printGraphviz :: DotGraph Text -> String
printGraphviz g = TL.unpack $ printDotGraph g
