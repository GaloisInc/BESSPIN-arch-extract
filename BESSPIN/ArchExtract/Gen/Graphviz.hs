module BESSPIN.ArchExtract.Gen.Graphviz where

import qualified Data.Map as M
import Data.Monoid
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types
import Data.GraphViz.Types.Generalised

import BESSPIN.ArchExtract.Verilog


type Key = T.Text


extendKey prefix id = prefix <> T.singleton '$' <> T.pack (show id)

genGraphviz :: [ModuleDecl] -> DotGraph Key
genGraphviz v = DotGraph False True Nothing (S.fromList $ stmts)
  where
    rootModId = moduleId $ head [m | m <- v, T.unpack (moduleName m) == "leg"]
    modMap = M.fromList [(moduleId m, m) | m <- v]

    stmts = go T.empty $ Instance 0 rootModId (T.pack "root")

    mkLabel name ty = Label $ StrLabel $ TL.fromStrict $ name <> T.pack "\\n" <> ty

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

printGraphviz :: DotGraph Key -> String
printGraphviz g = TL.unpack $ printDotGraph g
