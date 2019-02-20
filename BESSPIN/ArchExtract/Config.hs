{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Config where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified TOML


data Config = Config
    { configInput :: Input
    , configConstraints :: Constraints
    , configGraphvizOutput :: Maybe Graphviz
    , configModuleTreeOutput :: Maybe ModuleTree
    , configClaferOutput :: Maybe Clafer
    , configSMTOutput :: Maybe SMT
    }
    deriving (Show)

defaultConfig = Config
    { configInput = defaultInput
    , configConstraints = defaultConstraints
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

data Constraints = Constraints
    -- Derive constraints from parameters of module instantiations.
    { constraintsUseInstParams :: Bool
    -- Derive constraints from default expressions for uninitialized parameters
    -- in module instantiations.  Takes effect only if `use-inst-params` is
    -- also enabled.
    , constraintsUseInstParamDefaults :: Bool
    -- Derive constraints from the fact that the types of all ports connected
    -- to a net must match the type of the net.  For example, if a single net
    -- is connected to a port of type `logic[a:0]` and another port of type
    -- `logic[b:0]`, then add the constraint `a = b`.
    , constraintsUseNetTypes :: Bool
    -- Derive constraints from the fact that the internal and external types of
    -- a module instantiation's port must match.  For example, if the `pinTy`
    -- on the instantiation is `logic[a:0]` and the `portTy` on the module
    -- definition is `logic[b:0]`, then `a = b`.
    , constraintsUsePortTypes :: Bool
    -- For each module in this list, force its parameters (in every
    -- instantiation) to take on their default values.  Mainly useful for
    -- setting the parameters of the top-level module to their defaults.
    , constraintsForceModuleDefaults :: [Text]
    -- When set, all vectors are assumed to be big-endian (`[hi:lo]`).  This
    -- produces stricter constraints that rule out some undesirable solutions,
    -- such as setting `WIDTH` parameter to `-6` (so that `[WIDTH-1:0]` becomes
    -- `[-7:0]`) instead of `8` (`[7:0]`).
    , constraintsRequireBigEndianVectors :: Bool
    -- When set, all parameter values are required to be strictly positive.
    -- This is an alternative to `require-big-endian-vectors` for ruling out
    -- undesirable solutions.
    , constraintsRequirePositiveParams :: Bool
    }
    deriving (Show)

defaultConstraints = Constraints
    { constraintsUseInstParams = True
    , constraintsUseInstParamDefaults = True
    , constraintsUseNetTypes = True
    , constraintsUsePortTypes = True
    , constraintsForceModuleDefaults = []
    , constraintsRequireBigEndianVectors = False
    , constraintsRequirePositiveParams = False
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
    , smtGenUnsatCore :: Bool
    }
    deriving (Show)

defaultSMT = SMT
    { smtRootModule = "top"
    , smtOutFile = "out.smtlib2"
    , smtGenUnsatCore = False
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
            , ("constraints", \c x -> c { configConstraints = constraints x })
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

constraints :: TOML.Value -> Constraints
constraints x = tableFold defaultConstraints x
    [ ("use-inst-params", \c x -> c { constraintsUseInstParams = bool x })
    , ("use-inst-param-defaults", \c x -> c { constraintsUseInstParamDefaults = bool x })
    , ("use-net-types", \c x -> c { constraintsUseNetTypes = bool x })
    , ("use-port-types", \c x -> c { constraintsUsePortTypes = bool x })
    , ("force-module-defaults", \c x -> c { constraintsForceModuleDefaults = listOf str x })
    , ("require-big-endian-vectors", \c x -> c { constraintsRequireBigEndianVectors = bool x })
    , ("require-positive-params", \c x -> c { constraintsRequirePositiveParams = bool x })
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
    , ("gen-unsat-core", \c x -> c { smtGenUnsatCore = bool x })
    ]


parse :: Text -> Config
parse t = case TOML.parseTOML t of
    Left e -> error $ show e
    Right kvs -> config $ TOML.Table kvs
