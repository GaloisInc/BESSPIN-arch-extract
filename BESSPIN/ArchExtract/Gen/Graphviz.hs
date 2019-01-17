module BESSPIN.ArchExtract.Gen.Graphviz where

import Control.Monad
import qualified Data.Map as M
import Data.Monoid
import Data.Sequence (Seq, (|>), (<|))
import qualified Data.Sequence as S
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

tLogic = T.pack "(logic)"


joinKey parts = T.intercalate (T.singleton '$') parts
joinGraphId parts = Str $ TL.fromStrict $ joinKey parts

connKey prefix side (ExtPort i) =
    joinKey [prefix, T.pack "ext", side, T.pack (show i)]
connKey prefix side (InstPort i j) =
    joinKey [prefix, T.pack "inst", T.pack (show i), side, T.pack (show j)]
connKey prefix side (LogicPort i _) =
    --joinKey [prefix, T.pack "logic", T.pack (show i), side, T.pack (show j)]
    logicKey prefix i

netKey prefix netId =
    joinKey [prefix, T.pack "net", T.pack (show netId)]

logicKey prefix idx =
    joinKey [prefix, T.pack "logic", T.pack (show idx)]

mkLabel name = Label $ StrLabel $ TL.fromStrict name

labelStmt name = GA $ GraphAttrs [mkLabel name]


portCluster :: Text -> Text -> Text -> Seq PortDecl -> DotStatement Text
portCluster prefix side label ports =
    SG $ DotSG True (Just $ joinGraphId [prefix, T.pack "ext", side]) stmts
  where
    stmts =
        S.mapWithIndex go ports |> labelStmt label
    go idx (PortDecl name _) =
        let key = connKey prefix side (ExtPort idx) in
        let label = name in
        DN $ DotNode key [mkLabel label]

netNode :: Text -> NetId -> Net -> DotStatement Text
netNode prefix netId net =
    DN $ DotNode (netKey prefix netId) [mkLabel $ netName net]

logicNode :: Text -> Int -> DotStatement Text
logicNode prefix idx =
    DN $ DotNode (logicKey prefix idx) [mkLabel tLogic]

instCluster :: Design -> Text -> Int -> ModInst -> Seq (DotStatement Text)
instCluster _ _ _ inst | modInstId inst == -1 = S.empty
instCluster design prefix instIdx inst =
    S.singleton $ SG $ DotSG True (Just clusterId) stmts
  where
    clusterId = joinGraphId [prefix, T.pack "inst", T.pack $ show instIdx]
    mod = designMods design `S.index` modInstId inst
    stmts =
        S.mapWithIndex (go tSink) (modDeclInputs mod) <>
        S.mapWithIndex (go tSource) (modDeclOutputs mod) |>
        labelStmt (modDeclName mod <> T.singleton ' ' <> modInstName inst)
    go side idx port =
        DN $ DotNode (connKey prefix side (InstPort instIdx idx))
            [mkLabel $ portDeclName port]


edgeStmt k1 k2 = DE $ DotEdge k1 k2 [] 

netEdges :: Text -> NetId -> Net -> Seq (DotStatement Text)
netEdges prefix netId net =
    fmap (\conn -> edgeStmt (connKey prefix tSource conn) (netKey prefix netId)) (netSources net) <>
    fmap (\conn -> edgeStmt (netKey prefix netId) (connKey prefix tSink conn)) (netSinks net)


graphModule' :: Design -> Text -> ModDecl -> Seq (DotStatement Text)
graphModule' design prefix mod =
    portCluster prefix tSource tInputs (modDeclInputs mod) <|
    portCluster prefix tSink tOutputs (modDeclOutputs mod) <|
    S.mapWithIndex (netNode prefix) (modDeclNets mod) <>
    S.fromList (map (logicNode prefix) [0 .. S.length (modDeclLogics mod) - 1]) <>
    join (S.mapWithIndex (netEdges prefix) (modDeclNets mod)) <>
    join (S.mapWithIndex (instCluster design prefix) (modDeclInsts mod))

graphModule :: Design -> Text -> ModDecl -> DotGraph Text
graphModule design prefix mod = traceShow mod $
    DotGraph False True Nothing (graphModule' design prefix mod |> labelStmt (modDeclName mod))

{-





extendKey prefix id = prefix <> T.singleton '$' <> T.pack (show id)

genGraphviz :: [ModuleDecl] -> DotGraph Key
genGraphviz v = DotGraph False True Nothing (S.fromList $ stmts)
  where
    rootModId = moduleId $ head [m | m <- v, T.unpack (moduleName m) == "leg"]
    modMap = M.fromList [(moduleId m, m) | m <- v]

    stmts = go T.empty $ Instance 0 rootModId (T.pack "root") [] []


    go :: Key -> ModItem -> [DotStatement Key]
    go prefix (mi@Instance {}) =
        case M.lookup (instanceModId mi) modMap of
            Nothing -> [ DN $ DotNode key [mkLabel name (T.pack "??")] ]
            Just instMod ->
                [ DN $ DotNode key [mkLabel name (moduleName instMod)] ] ++
                concatMap (go key) (moduleItems instMod) ++
                concatMap (goEdges key) (moduleItems instMod)
      where key = extendKey prefix (modItemId mi)
            name = instanceName mi

    -- Generate edges from `key` to each of its children in `mis`
    goEdges :: Key -> ModItem -> [DotStatement Key]
    goEdges prefix (mi@Instance {}) =
        [ DE $ DotEdge prefix (extendKey prefix (modItemId mi)) [] ]
-}

printGraphviz :: DotGraph Text -> String
printGraphviz g = TL.unpack $ printDotGraph g
