{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module BESSPIN.ArchExtract.Config where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro.Platform
import qualified TOML

import BESSPIN.ArchExtract.Lens


data Config = Config
    { configSrcs :: Map Text Src
    , configDesign :: Design
    , configNameMap :: NameMap
    , configConstraints :: Constraints
    , configRewrite :: Maybe Rewrite
    , configGraphvizOutput :: Maybe Graphviz
    , configModuleTreeOutput :: Maybe ModuleTree
    , configClaferOutput :: Maybe Clafer
    , configSMTOutput :: Maybe SMT
    , configParamClaferOutput :: Maybe ParamClafer
    }
    deriving (Show)

defaultConfig = Config
    { configSrcs = M.empty
    , configDesign = defaultDesign
    , configNameMap = defaultNameMap
    , configConstraints = defaultConstraints
    , configRewrite = Nothing
    , configGraphvizOutput = Nothing
    , configModuleTreeOutput = Nothing
    , configClaferOutput = Nothing
    , configSMTOutput = Nothing
    , configParamClaferOutput = Nothing
    }

defaultConfigWithClafer = defaultConfig { configClaferOutput = Just $ defaultClafer }

data Src =
      VerilogSrc Verilog
    | BSVSrc BSV
    deriving (Show)

data Verilog = Verilog
    -- List of (System)Verilog source files to parse.  This can include shell
    -- globs.
    { verilogSrcFiles :: [Text]
    -- Path to the CBOR file where the serialized AST should be cached.  If
    -- this is `Nothing`, the AST is re-exported each time.
    , verilogAstFile :: Maybe Text
    -- Names of modules to blackbox.  The contents of these modules are
    -- discarded early in processing, so it's okay for them to contain
    -- unsupported Verilog constructs.
    , verilogBlackboxModules :: [Text]
    -- Names of nets to disconnect.  Useful for hiding clock and reset nets,
    -- which would otherwise wind up connected to nearly every part of the
    -- design.
    , verilogDisconnectNets :: [Text]
    }
    deriving (Show)

defaultVerilog = Verilog
    { verilogSrcFiles = []
    , verilogAstFile = Nothing
    , verilogBlackboxModules = []
    , verilogDisconnectNets = []
    }

data BSV = BSV
    -- List of BSV source files to parse.  This can include shell globs.
    { bsvSrcFiles :: [Text]
    -- Path to a directory where the CBOR files containing serialized package
    -- ASTs should be cached.  If this is `Nothing`, the AST is re-exported
    -- each time.  Setting this is recommended: many driver operations on BSV
    -- sources require access to the AST, and re-exporting it each time is
    -- fairly expensive.
    , bsvAstDir :: Maybe Text
    -- Names of packages that are considered "library packages".  These
    -- packages are processed as normal, except that modules found in library
    -- packages will have only ports (no internal structures) and will be
    -- marked as `MkExtern`
    , bsvLibraryPackages :: Set Text
    -- Qualified name (`Pkg.mkFoo`) of the root module of the design.  The
    -- package containing this module is always imported; other packages are
    -- imported only when required due to an `import` statement in the BSV.
    , bsvRootModule :: Text
    -- List of extra command-line options to pass to the Bluespec Compiler.
    , bsvBscFlags :: [Text]
    }
    deriving (Show)

defaultBSV = BSV
    { bsvSrcFiles = []
    , bsvAstDir = Nothing
    , bsvLibraryPackages = Set.empty
    , bsvRootModule = ""
    , bsvBscFlags = []
    }

data Design = Design
    -- The name of the root module of the design.  This setting provides a
    -- default value for `root-module` options in other sections, and is also
    -- used directly by some subcommands.
    { designRootModule :: Text
    }
    deriving (Show)

defaultDesign = Design
    { designRootModule = ""
    }

data NameMap = NameMap
    { nameMapFile :: Maybe Text
    , nameMapEntries :: [(Text, Text)]
    }
    deriving (Show)

defaultNameMap = NameMap
    { nameMapFile = Nothing
    , nameMapEntries = []
    }

data Constraints = Constraints
    -- Constraint generation toggles.  Typically all of these should be set to
    -- `true`, and `force-module-defaults` should be set to the root module of
    -- the design.

    -- Derive constraints from parameters of module instantiations.
    { constraintsUseInstParams :: Bool
    -- Derive constraints from default expressions of local parameters.
    , constraintsUseLocalDefaults :: Bool
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

    -- Override toggles.  These enable generation of override variables for
    -- certain parameters, essentially making them into features.

    -- Generate override variables for all module instantiation parameters.
    , constraintsOverrideInstParams :: Bool
    -- Generate override variables for all local parameters.
    , constraintsOverrideLocalParams :: Bool
    -- Generate override variables for the parameters affected by
    -- `force-module-defaults`.
    , constraintsOverrideForcedParams :: Bool
    -- Allow generating override variables for non-constant parameter
    -- expressions.  By default, only parameters that are set to constant
    -- values (`parameter foo = 16`) are overridable.  If this flag is enabled,
    -- all parameters become overridable instead (subject to the other override
    -- flags).
    , constraintsAllowOverrideNonConstant :: Bool

    -- Additional constraint generation options.

    -- When set, all vectors are required to be big-endian (`[hi:lo]`).  This
    -- produces stricter constraints that rule out some undesirable solutions,
    -- such as setting `WIDTH` parameter to `-6` (so that `[WIDTH-1:0]` becomes
    -- `[-7:0]`) instead of `8` (`[7:0]`).
    , constraintsRequireBigEndianVectors :: Bool
    -- When set, all parameter values are required to be strictly positive.
    -- This is an alternative to `require-big-endian-vectors` for ruling out
    -- undesirable solutions.
    , constraintsRequirePositiveParams :: Bool

    -- Map from module name to a list of custom constraints for that module.
    , constraintsCustom :: [(Text, [Text])]
    }
    deriving (Show)

defaultConstraints = Constraints
    { constraintsUseInstParams = True
    , constraintsUseLocalDefaults = True
    , constraintsUseNetTypes = True
    , constraintsUsePortTypes = True
    , constraintsForceModuleDefaults = []
    , constraintsOverrideInstParams = False
    , constraintsOverrideLocalParams = False
    , constraintsOverrideForcedParams = False
    , constraintsAllowOverrideNonConstant = False
    , constraintsRequireBigEndianVectors = False
    , constraintsRequirePositiveParams = False
    , constraintsCustom = []
    }

data Rewrite = Rewrite
    -- Base directory for the rewrite.  Paths recorded in the CBOR file will be
    -- interpreted relative to this directory.  Usually this should be the
    -- directory from which `exporter` was initially run.
    { rewriteBaseDir :: Text
    -- List of (name, value) pairs for overrides whose values should be
    -- rewritten.  For each pair, the expression corresponding to override
    -- `name` will be replaced with the `value` text.  This process is
    -- idempotent, so (for now) we simply apply these rewrites each time the
    -- tool is run.
    , rewriteOverrides :: [(Text, Text)]
    -- Root module.  TODO: unify the various root-module options
    , rewriteRootModule :: Text
    }
    deriving (Show)

defaultRewrite = Rewrite
    { rewriteBaseDir = "."
    , rewriteOverrides = []
    , rewriteRootModule = "top"
    }

data Graphviz = Graphviz
    { graphvizDrawNets :: Bool
    , graphvizDrawOnesidedNets :: Bool
    , graphvizDrawLogics :: Bool
    , graphvizDrawLogicPorts :: Bool
    , graphvizDrawExtPorts :: Bool
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
    , graphvizDrawLogicPorts = True
    , graphvizDrawExtPorts = True
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
    -- Whether modules should contain an `origin` subclafer giving the file
    -- where the module was defined.
    , claferEmitOrigin :: Bool
    -- Whether clafers should extend `component` and other clafers from the
    -- background theory.  If so, additional constraints will be added to set
    -- inherited fields such as `name`.
    , claferUseBackgroundTheory :: Bool
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
    , claferEmitOrigin = True
    , claferUseBackgroundTheory = True
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

data ParamClafer = ParamClafer
    -- Names of modules to instantiate at top level.  If this is empty, the
    -- output will contain (almost) no concrete clafers.
    { paramClaferRootModule :: Text
    , paramClaferOutFile :: Text
    }
    deriving (Show)

defaultParamClafer = ParamClafer
    { paramClaferRootModule = "top"
    , paramClaferOutFile = "out.p.cfr"
    }


listOf f (TOML.List xs) = map f xs
listOf _ x = error $ "expected list, but got " ++ show x

setOf f x = Set.fromList $ listOf f x

tableOf f (TOML.Table kvs) = map (\(k,v) -> (k, f v)) kvs
tableOf _ x = error $ "expected table, but got " ++ show x

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

tableMap :: TOML.Value -> (Text -> TOML.Value -> a) -> [a]
tableMap (TOML.Table kvs) f = map go kvs
  where
    go (k, v) = f k v
tableMap x _ = error $ "expected table, but got " ++ show x

tableDispatch :: TOML.Value -> Text -> [(Text, TOML.Value -> a)] -> a
tableDispatch x@(TOML.Table kvs) k fs = dispFunc x
  where
    dispVal = case lookup k kvs of
        Just (TOML.String s) -> s
        Just x -> error $ "expected `type` to be a string, but got " ++ show x
        Nothing -> error $ "missing `type` field"
    dispFunc = case lookup dispVal fs of
        Just f -> f
        Nothing -> error $ "unknown type " ++ show dispVal


config :: TOML.Value -> Config
config x =
    if length inputKeys > 1 then
        error $ "expected at most one input section, but got " ++ show inputKeys
    else
        tableFold defaultConfig x
            [ ("src", \c x -> c { configSrcs = srcs x })
            , ("design", \c x -> c { configDesign = design x })
            , ("name-map", \c x -> c { configNameMap = nameMap x })
            , ("constraints", \c x -> c { configConstraints = constraints x })
            , ("rewrite", \c x -> c { configRewrite = Just $ rewrite x })
            , ("graphviz", \c x -> c { configGraphvizOutput = Just $ graphviz x })
            , ("module-tree", \c x -> c { configModuleTreeOutput = Just $ moduleTree x })
            , ("clafer", \c x -> c { configClaferOutput = Just $ clafer x })
            , ("smt", \c x -> c { configSMTOutput = Just $ smt x })
            , ("param-clafer", \c x -> c { configParamClaferOutput = Just $ paramClafer x })
            , ("featuresynth", \c _ -> c)
            ]
  where
    inputKeys = filter (\k -> k == "verilog") $ tableKeys x

srcs :: TOML.Value -> Map Text Src
srcs x = M.fromList $ tableMap x $ \name x -> (name, src x)

src :: TOML.Value -> Src
src x = tableDispatch x "type"
    [ ("verilog", \x -> VerilogSrc $ verilog x)
    , ("bsv", \x -> BSVSrc $ bsv x)
    ]

verilog :: TOML.Value -> Verilog
verilog x = tableFold defaultVerilog x
    [ ("type", \c x -> c)
    , ("src-files", \c x -> c { verilogSrcFiles = listOf str x })
    , ("ast-file", \c x -> c { verilogAstFile = Just $ str x })
    , ("blackbox-modules", \c x -> c { verilogBlackboxModules = listOf str x })
    , ("disconnect-nets", \c x -> c { verilogDisconnectNets = listOf str x })
    ]

bsv :: TOML.Value -> BSV
bsv x = tableFold defaultBSV x
    [ ("type", \c x -> c)
    , ("src-files", \c x -> c { bsvSrcFiles = listOf str x })
    , ("ast-dir", \c x -> c { bsvAstDir = Just $ str x })
    , ("library-packages", \c x -> c { bsvLibraryPackages = setOf str x })
    , ("root-module", \c x -> c { bsvRootModule = str x })
    , ("bsc-flags", \c x -> c { bsvBscFlags = listOf str x })
    ]

design :: TOML.Value -> Design
design x = tableFold defaultDesign x
    [ ("root-module", \c x -> c { designRootModule = str x })
    ]

nameMap :: TOML.Value -> NameMap
nameMap x = tableFold defaultNameMap x
    [ ("file", \c x -> c { nameMapFile = Just $ str x })
    , ("entries", \c x -> c { nameMapEntries = tableOf str x })
    ]

constraints :: TOML.Value -> Constraints
constraints x = tableFold defaultConstraints x
    [ ("use-inst-params", \c x -> c { constraintsUseInstParams = bool x })
    , ("use-local-defaults", \c x -> c { constraintsUseLocalDefaults = bool x })
    , ("use-net-types", \c x -> c { constraintsUseNetTypes = bool x })
    , ("use-port-types", \c x -> c { constraintsUsePortTypes = bool x })
    , ("force-module-defaults", \c x -> c { constraintsForceModuleDefaults = listOf str x })
    , ("override-inst-params", \c x -> c { constraintsOverrideInstParams = bool x })
    , ("override-local-params", \c x -> c { constraintsOverrideLocalParams = bool x })
    , ("override-forced-params", \c x -> c { constraintsOverrideForcedParams = bool x })
    , ("allow-override-non-constant", \c x -> c { constraintsAllowOverrideNonConstant = bool x })
    , ("require-big-endian-vectors", \c x -> c { constraintsRequireBigEndianVectors = bool x })
    , ("require-positive-params", \c x -> c { constraintsRequirePositiveParams = bool x })
    , ("custom", \c x -> c { constraintsCustom = tableOf (listOf str) x })
    ]

rewrite :: TOML.Value -> Rewrite
rewrite x = tableFold defaultRewrite x
    [ ("base-dir", \c x -> c { rewriteBaseDir = str x })
    , ("overrides", \c x -> c { rewriteOverrides = tableOf str x })
    , ("root-module", \c x -> c { rewriteRootModule = str x })
    ]

graphviz :: TOML.Value -> Graphviz
graphviz x = tableFold defaultGraphviz x
    [ ("draw-nets", \c x -> c { graphvizDrawNets = bool x })
    , ("draw-onesided-nets", \c x -> c { graphvizDrawOnesidedNets = bool x })
    , ("draw-logics", \c x -> c { graphvizDrawLogics = bool x })
    , ("draw-logic-ports", \c x -> c { graphvizDrawLogicPorts = bool x })
    , ("draw-ext-ports", \c x -> c { graphvizDrawExtPorts = bool x })
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
    , ("emit-origin", \c x -> c { claferEmitOrigin = bool x })
    , ("use-background-theory", \c x -> c { claferUseBackgroundTheory = bool x })
    ]

smt :: TOML.Value -> SMT
smt x = tableFold defaultSMT x
    [ ("root-module", \c x -> c { smtRootModule = str x })
    , ("out-file", \c x -> c { smtOutFile = str x })
    , ("gen-unsat-core", \c x -> c { smtGenUnsatCore = bool x })
    ]

paramClafer :: TOML.Value -> ParamClafer
paramClafer x = tableFold defaultParamClafer x
    [ ("root-module", \c x -> c { paramClaferRootModule = str x })
    , ("out-file", \c x -> c { paramClaferOutFile = str x })
    ]


makeLenses' ''Config
makeLenses' ''Verilog
makeLenses' ''BSV
makeLenses' ''Design
makeLenses' ''NameMap
makeLenses' ''Constraints
makeLenses' ''Rewrite
makeLenses' ''Graphviz
makeLenses' ''ModuleTree
makeLenses' ''Clafer
makeLenses' ''SMT
makeLenses' ''ParamClafer

-- Lenses for `Src`

_VerilogSrc :: Traversal Src Src Verilog Verilog
_VerilogSrc f (VerilogSrc a) = VerilogSrc <$> f a
_VerilogSrc f x = pure x

_BSVSrc :: Traversal Src Src BSV BSV
_BSVSrc f (BSVSrc a) = BSVSrc <$> f a
_BSVSrc f x = pure x


postprocess :: Config -> Config
postprocess c = applyRootModule $ c

applyRootModule c = if T.null root then c else c'
  where
    root = c ^. _configDesign . _designRootModule

    maybeSetRoot s = if T.null s then root else s

    maybeSetRoots [] = [root]
    maybeSetRoots xs = xs

    c' = c
        & _configRewrite . traversed . _rewriteRootModule %~ maybeSetRoot
        & _configModuleTreeOutput . traversed . _moduleTreeRootModule %~ maybeSetRoot
        & _configClaferOutput . traversed . _claferRootModules %~ maybeSetRoots
        & _configSMTOutput . traversed . _smtRootModule %~ maybeSetRoot
        & _configParamClaferOutput . traversed . _paramClaferRootModule %~ maybeSetRoot
        & _configSrcs . traversed . _BSVSrc . _bsvRootModule %~ maybeSetRoot


parse :: Text -> Config
parse t = case TOML.parseTOML t of
    Left e -> error $ show e
    Right kvs -> postprocess $ config $ TOML.Table kvs
