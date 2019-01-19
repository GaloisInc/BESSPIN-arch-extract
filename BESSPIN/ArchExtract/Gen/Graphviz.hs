module BESSPIN.ArchExtract.Gen.Graphviz where

import Control.Monad
import qualified Data.Map as M
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


data Cfg = Cfg
    { cfgPrefix :: Text
    , cfgDrawNets :: Bool
    , cfgDrawOnesidedNets :: Bool
    , cfgHideNamedNets :: Set Text
    }

defaultCfg = Cfg
    { cfgPrefix = T.empty
    , cfgDrawNets = True
    , cfgDrawOnesidedNets = True
    , cfgHideNamedNets = Set.empty
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

connKey cfg side (ExtPort i) =
    joinKey [cfgPrefix cfg, T.pack "ext", side, T.pack (show i)]
connKey cfg side (InstPort i j) =
    joinKey [cfgPrefix cfg, T.pack "inst", T.pack (show i), side, T.pack (show j)]
connKey cfg side (LogicPort i _) =
    --joinKey [cfgPrefix cfg, T.pack "logic", T.pack (show i), side, T.pack (show j)]
    logicKey cfg i

netKey cfg idx =
    joinKey [cfgPrefix cfg, T.pack "net", T.pack (show idx)]

logicKey cfg idx =
    joinKey [cfgPrefix cfg, T.pack "logic", T.pack (show idx)]

mkLabel name = Label $ StrLabel $ TL.fromStrict name

labelStmt name = GA $ GraphAttrs [mkLabel name]


portCluster :: Cfg -> Text -> Text -> Seq PortDecl -> DotStatement Text
portCluster cfg side label ports =
    SG $ DotSG True (Just $ joinGraphId [cfgPrefix cfg, T.pack "ext", side]) stmts
  where
    stmts =
        S.mapWithIndex go ports |> labelStmt label
    go idx (PortDecl name _) =
        let key = connKey cfg side (ExtPort idx) in
        let label = name in
        DN $ DotNode key [mkLabel label]

netNode :: Cfg -> Int -> Net -> DotStatement Text
netNode cfg idx net =
    DN $ DotNode (netKey cfg idx) [mkLabel $ netName net]

logicLabel LkOther = T.pack "(logic)"
logicLabel LkNetAlias = T.pack "(net alias)"

logicNode :: Cfg -> Int -> Logic -> DotStatement Text
logicNode cfg idx logic =
    DN $ DotNode (logicKey cfg idx) [mkLabel $ logicLabel $ logicKind logic]

instCluster :: Design -> Cfg -> Int -> ModInst -> Seq (DotStatement Text)
instCluster _ _ _ inst | modInstId inst == -1 = S.empty
instCluster design cfg instIdx inst =
    S.singleton $ SG $ DotSG True (Just clusterId) stmts
  where
    clusterId = joinGraphId [cfgPrefix cfg, T.pack "inst", T.pack $ show instIdx]
    mod = designMods design `S.index` modInstId inst
    stmts =
        S.mapWithIndex (go tSink) (modDeclInputs mod) <>
        S.mapWithIndex (go tSource) (modDeclOutputs mod) |>
        labelStmt (modDeclName mod <> T.singleton ' ' <> modInstName inst)
    go side idx port =
        DN $ DotNode (connKey cfg side (InstPort instIdx idx))
            [mkLabel $ portDeclName port]


edgeStmt k1 k2 = DE $ DotEdge k1 k2 []

netEdges :: Cfg -> Int -> Net -> Seq (DotStatement Text)
netEdges cfg idx net =
    if drawNetNode cfg net then drawDirect
    else if drawNetEdges cfg net then drawSkip
    else S.empty
  where
    drawDirect =
        fmap (\conn -> edgeStmt (connKey cfg tSource conn) (netKey cfg idx)) (netSources net) <>
        fmap (\conn -> edgeStmt (netKey cfg idx) (connKey cfg tSink conn)) (netSinks net)

    drawSkip =
        join $ fmap (\c1 ->
            fmap (\c2 -> edgeStmt (connKey cfg tSource c1) (connKey cfg tSink c2))
                (netSinks net))
            (netSources net)


graphModule' :: Design -> Cfg -> ModDecl -> Seq (DotStatement Text)
graphModule' design cfg mod =
    portCluster cfg tSource tInputs (modDeclInputs mod) <|
    portCluster cfg tSink tOutputs (modDeclOutputs mod) <|
    S.foldMapWithIndex (\idx net ->
        if drawNetNode cfg net then S.singleton $ netNode cfg idx net else S.empty)
        (modDeclNets mod) <>
    S.mapWithIndex (logicNode cfg) (modDeclLogics mod) <>
    join (S.mapWithIndex (netEdges cfg) (modDeclNets mod)) <>
    join (S.mapWithIndex (instCluster design cfg) (modDeclInsts mod))

graphModule :: Design -> Cfg -> ModDecl -> DotGraph Text
graphModule design cfg mod = traceShow mod $
    DotGraph False True (Just $ joinGraphId [cfgPrefix cfg])
        ( graphModule' design cfg mod
        |> labelStmt (modDeclName mod)
        |> GA (GraphAttrs [RankDir FromLeft, RankSep [2.0]])
        )

printGraphviz :: DotGraph Text -> String
printGraphviz g = TL.unpack $ printDotGraph g
