{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Config where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified TOML


data Config = Config
    { configInput :: Input
    , configGraphvizOutput :: Maybe Graphviz
    , configModuleTreeOutput :: Maybe ModuleTree
    , configClaferOutput :: Maybe Clafer
    , configSMTOutput :: Maybe SMT
    }
    deriving (Show)

defaultConfig = Config
    { configInput = defaultInput
    , configGraphvizOutput = Nothing
    , configModuleTreeOutput = Nothing
    , configClaferOutput = Nothing
    , configSMTOutput = Nothing
    }

defaultConfigWithClafer = defaultConfig { configClaferOutput = Just $ defaultClafer }

data Input =
    VerilogInput Verilog
    deriving (Show)

defaultInput = VerilogInput defaultVerilog

data Verilog = Verilog
    -- Names of modules to blackbox.  The contents of these modules are
    -- discarded early in processing, so it's okay for them to contain
    -- unsupported Verilog constructs.
    { verilogBlackboxModules :: [Text]
    -- Path to the CBOR file containing the exported Verilog source.
    , verilogSourceFile :: Text
    }
    deriving (Show)

defaultVerilog = Verilog
    { verilogBlackboxModules = []
    , verilogSourceFile = "out.cbor"
    }

data Graphviz = Graphviz
    { graphvizDrawNets :: Bool
    , graphvizDrawOnesidedNets :: Bool
    , graphvizDrawLogics :: Bool
    , graphvizDedupEdges :: Bool
    -- If `True`, names of merged nets will display as `foo (+2 more)` instead
    -- of listing the name of every net included in the merge.
    , graphvizShortenNetNames :: Bool
    , graphvizNumPipelineStages :: Int
    -- Names of modules to render.  If `Nothing`, render all modules.
    , graphvizRenderModules :: Maybe [Text]
    -- Directory to place generated `.dot` files.  Graphviz output generates
    -- one file per module in the input design.
    , graphvizOutDir :: Text
    }
    deriving (Show)

defaultGraphviz = Graphviz
    { graphvizDrawNets = True
    , graphvizDrawOnesidedNets = False
    , graphvizDrawLogics = True
    , graphvizDedupEdges = False
    , graphvizShortenNetNames = False
    , graphvizNumPipelineStages = 0
    , graphvizRenderModules = Nothing
    , graphvizOutDir = "out"
    }

data ModuleTree = ModuleTree
    { moduleTreeOutFile :: Text
    , moduleTreeRootModule :: Text
    }
    deriving (Show)

defaultModuleTree = ModuleTree
    { moduleTreeOutFile = "module-tree.dot"
    , moduleTreeRootModule = "top"
    }

data Clafer = Clafer
    -- Names of modules to instantiate at top level.  If this is empty, the
    -- output will contain (almost) no concrete clafers.
    { claferRootModules :: [Text]
    , claferOutFile :: Text
    -- Whether to emit non-`LkInst` logic nodes.
    , claferEmitLogics :: Bool
    -- Whether to emit `LkInst` logic nodes.
    , claferEmitInsts :: Bool
    , claferEmitNets :: Bool
    , claferEmitPorts :: Bool
    , claferEmitParams :: Bool
    }
    deriving (Show)

defaultClafer = Clafer
    { claferRootModules = []
    , claferOutFile = "out.cfr"
    , claferEmitLogics = True
    , claferEmitInsts = True
    , claferEmitNets = True
    , claferEmitPorts = True
    , claferEmitParams = True
    }

data SMT = SMT
    -- Names of modules to instantiate at top level.  If this is empty, the
    -- output will contain (almost) no concrete clafers.
    { smtRootModule :: Text
    , smtOutFile :: Text
    }
    deriving (Show)

defaultSMT = SMT
    { smtRootModule = "top"
    , smtOutFile = "out.smtlib2"
    }


listOf f (TOML.List xs) = map f xs
listOf _ x = error $ "expected list, but got " ++ show x

str (TOML.String s) = s
str x = error $ "expected string, but got " ++ show x

bool (TOML.Bool b) = b
bool x = error $ "expected bool, but got " ++ show x

int (TOML.Integer i)
    | toInteger (minBound :: Int) <= i &&
        i <= toInteger (maxBound :: Int) = fromInteger i
    | otherwise = error $ "integer out of bounds: " ++ show i
int x = error $ "expected integer, but got " ++ show x

tableKeys :: TOML.Value -> [Text]
tableKeys (TOML.Table kvs) = map fst kvs
tableKeys x = error $ "expected table, but got " ++ show x

tableFold :: a -> TOML.Value -> [(Text, a -> TOML.Value -> a)] -> a
tableFold z (TOML.Table kvs) fs = foldl go z kvs
  where
    fm = M.fromList fs
    go x (k, v) | Just f <- M.lookup k fm = f x v
    go _ (k, _) = error $ "unrecognized key " ++ show k ++
        "; expected one of " ++ show (M.keys fm)
tableFold _ x _ = error $ "expected table, but got " ++ show x


config :: TOML.Value -> Config
config x =
    if length inputKeys > 1 then
        error $ "expected at most one input section, but got " ++ show inputKeys
    else
        tableFold defaultConfig x
            [ ("verilog", \c x -> c { configInput = VerilogInput $ verilog x })
            , ("graphviz", \c x -> c { configGraphvizOutput = Just $ graphviz x })
            , ("module-tree", \c x -> c { configModuleTreeOutput = Just $ moduleTree x })
            , ("clafer", \c x -> c { configClaferOutput = Just $ clafer x })
            , ("smt", \c x -> c { configSMTOutput = Just $ smt x })
            ]
  where
    inputKeys = filter (\k -> k == "verilog") $ tableKeys x

verilog :: TOML.Value -> Verilog
verilog x = tableFold defaultVerilog x
    [ ("blackbox-modules", \c x -> c { verilogBlackboxModules = listOf str x })
    , ("source-file", \c x -> c { verilogSourceFile = str x })
    ]

graphviz :: TOML.Value -> Graphviz
graphviz x = tableFold defaultGraphviz x
    [ ("draw-nets", \c x -> c { graphvizDrawNets = bool x })
    , ("draw-onesided-nets", \c x -> c { graphvizDrawOnesidedNets = bool x })
    , ("draw-logics", \c x -> c { graphvizDrawLogics = bool x })
    , ("dedup-edges", \c x -> c { graphvizDedupEdges = bool x })
    , ("shorten-net-names", \c x -> c { graphvizShortenNetNames = bool x })
    , ("num-pipeline-stages", \c x -> c { graphvizNumPipelineStages = int x })
    , ("render-modules", \c x -> c { graphvizRenderModules = Just $ listOf str x })
    , ("out-dir", \c x -> c { graphvizOutDir = str x })
    ]

moduleTree :: TOML.Value -> ModuleTree
moduleTree x = tableFold defaultModuleTree x
    [ ("out-file", \c x -> c { moduleTreeOutFile = str x })
    , ("root-module", \c x -> c { moduleTreeRootModule = str x })
    ]

clafer :: TOML.Value -> Clafer
clafer x = tableFold defaultClafer x
    [ ("root-modules", \c x -> c { claferRootModules = listOf str x })
    , ("out-file", \c x -> c { claferOutFile = str x })
    , ("emit-logics", \c x -> c { claferEmitLogics = bool x })
    , ("emit-insts", \c x -> c { claferEmitInsts = bool x })
    , ("emit-nets", \c x -> c { claferEmitNets = bool x })
    , ("emit-ports", \c x -> c { claferEmitPorts = bool x })
    , ("emit-params", \c x -> c { claferEmitParams = bool x })
    ]

smt :: TOML.Value -> SMT
smt x = tableFold defaultSMT x
    [ ("root-module", \c x -> c { smtRootModule = str x })
    , ("out-file", \c x -> c { smtOutFile = str x })
    ]


parse :: Text -> Config
parse t = case TOML.parseTOML t of
    Left e -> error $ show e
    Right kvs -> config $ TOML.Table kvs
