{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, Rank2Types #-}
module BESSPIN.ArchExtract.Architecture
    ( module BESSPIN.ArchExtract.Architecture
    , Span(..)
    , HasSpan(..)
    , dummySpan
    ) where

import Control.Monad.State
import Data.Data
import Data.Ix
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Lens.Micro.Platform

import BESSPIN.ArchExtract.Lens
-- TODO: move span stuff out of verilog.ast
import BESSPIN.ArchExtract.Verilog.AST (Span(..), HasSpan(..), dummySpan)


-- The ID of a module is its position in `designMods`.
type ModId = Int
-- The ID of a net is its position in `moduleNets`.
newtype NetId = NetId { unwrapNetId :: Int }
    deriving (Show, Eq, Ord, Ix, Typeable, Data)

instance Enum NetId where
    toEnum = NetId
    fromEnum = unwrapNetId

data Design ann = Design
    { designMods :: Seq (Module ann)
    }
    deriving (Show, Typeable, Data)

data Module ann = Module
    { moduleName :: Text
    , moduleParams :: Seq Param
    , moduleInputs :: Seq Port
    , moduleOutputs :: Seq Port
    , moduleLogics :: Seq (Logic ann)
    , moduleNets :: Seq (Net ann)
    , moduleConstraints :: Seq Constraint
    }
    deriving (Show, Typeable, Data)


data Param = Param
    { paramName :: Text
    , paramDefault :: Maybe ConstExpr
    , paramKind :: ParamKind
    }
    deriving (Show, Typeable, Data)

data ParamKind =
    -- Ordinary global parameter, which can be set by module instantiations.
    PkNormal |
    -- Local parameter, which always takes on its default value.
    PkLocal |
    -- Enum variant.  Behaves like `PkLocal`, but it may not have an
    -- initializer.
    PkEnum
    deriving (Show, Eq, Data, Typeable)

data Port = Port
    { portName :: Text
    , portNet :: NetId
    , portTy :: Ty
    }
    deriving (Show, Typeable, Data)


data Logic ann = Logic
    { logicKind :: LogicKind
    , logicInputs :: Seq Pin
    , logicOutputs :: Seq Pin
    , logicAnn :: ann
    }
    deriving (Show, Typeable, Data)

data LogicKind =
    LkInst Inst |
    LkNetAlias |
    LkRegister Text |
    LkDFlipFlop
        { lkDffName :: Text
        , lkDffNumResets :: Int
        } |
    LkExpr |
    LkOther
    deriving (Show, Eq, Typeable, Data)

data Inst = Inst 
    { instModId :: ModId
    , instName :: Text
    , instParams :: Seq (Maybe ConstExpr)
    }
    deriving (Show, Eq, Typeable, Data)

data Pin = Pin
    { pinNet :: NetId
    , pinTy :: Ty
    }
    deriving (Show, Typeable, Data)


data Net ann = Net
    { netName :: Text
    -- Used to select a name to use when merging nets during net alias handling
    , netNamePriority :: Int
    , netSources :: Seq Conn
    , netSinks :: Seq Conn
    , netTy :: Ty
    , netAnn :: ann
    }
    deriving (Show, Typeable, Data)

-- For `ExtPort`, the `Int` is an index into the `moduleInputs`/`Outputs` list
-- (`Inputs` if this `Conn` is on the `netSinks` side of the `Net`, `Outputs`
-- if it's on the `netSources` side).  For `LogicPort`, the first `Int` is the
-- index of the `Logic` in `moduleLogics`, and the second `Int` indexes into
-- `logicInputs`/`Outputs`.
data Conn = ExtPort Int | LogicPort Int Int
    deriving (Show, Eq, Ord, Typeable, Data)


data Constraint = Constraint
    { constraintExpr :: ConstExpr
    , constraintOrigin :: ConstraintOrigin
    }
    deriving (Show, Typeable, Data)

data ConstraintOrigin =
    -- From the provided expression for parameter `j` on LkInst logic `i`.
    CoInstParam Int Int |
    -- From the default expression for parameter `j` of instance `i`.
    CoInstParamDefault Int Int |
    -- From the default expression for parameter `i` of the enclosing module.
    CoParamDefault Int |
    -- From connection `side, j` of net `i`.
    CoNetConn NetId Side Int |
    -- From the port connection for port `side, j` of LkInst logic `i`.
    CoPortConn Int Side Int |
    CoText Text
    deriving (Show, Eq, Typeable, Data)


data Ty =
    TWire
    { tWireWidths :: [ConstExpr]
    , tWireDepths :: [ConstExpr]
    } |
    TEnum
    { tEnumBase :: Ty
    } |
    TAlias
    { tAliasName :: Text
    , tAliasResolved :: Ty
    } |
    -- Integer value with no specific bit width.
    TUnsizedInt |
    -- Type for values used only in simulation (int, string, etc).
    TSimVal |
    TUnknown
    deriving (Show, Eq, Typeable, Data)


data ConstExpr =
      EIntLit Span Int
    -- A parameter of the enclosing module
    | EParam Span Int
    -- A parameter of a (nested) module instantiation.  `EInstParam [i, j] k`
    -- corresponds to `inst_i.inst_j.param_k`, relative to the enclosing
    -- module.
    | EInstParam Span [Int] Int
    -- `a -> a`
    | EUnArith Span UnArithOp ConstExpr
    -- `a -> a -> a`
    | EBinArith Span BinArithOp ConstExpr ConstExpr
    -- `a -> a -> Bool`
    | EBinCmp Span BinCmpOp ConstExpr ConstExpr
    -- Size of the range `e1:e2`.  Equal to `abs(e1 - e2) + 1`.
    | ERangeSize Span ConstExpr ConstExpr

    -- Special expression types, used only in constraint generation:

    -- If override `i` is enabled, use its value; otherwise, use `e`.
    | EOverride Int ConstExpr
    -- Like `EOverride`, but using a generated override associated with
    -- parameter `i` of the current module.
    | EOverrideLocalParam Int ConstExpr
    -- Like `EOverride`, but using a generated override associated with
    -- parameter `j` of instance `i` within the current module.
    | EOverrideInstParam Int Int ConstExpr
    deriving (Show, Eq, Typeable, Data)

data UnArithOp = UClog2
    deriving (Show, Eq, Typeable, Data)

data BinArithOp = BAdd | BSub | BMul
    deriving (Show, Eq, Typeable, Data)

data BinCmpOp = BEq | BNe | BLt | BLe | BGt | BGe
    deriving (Show, Eq, Typeable, Data)


-- Enum for indicating a side of a net, logic, or module.  For clarity, these
-- are named `Source` and `Sink` instead of `Input` and `Output` because
-- `Input`/`Output` have opposite meanings on external ports vs. logic.
data Side = Source | Sink
    deriving (Show, Eq, Ord, Typeable, Data)

flipSide Source = Sink
flipSide Sink = Source


instance HasSpan ConstExpr where
    spanOf (EIntLit sp _) = sp
    spanOf (EParam sp _) = sp
    spanOf (EInstParam sp _ _) = sp
    spanOf (EUnArith sp _ _) = sp
    spanOf (EBinArith sp _ _ _) = sp
    spanOf (EBinCmp sp _ _ _) = sp
    spanOf (ERangeSize sp _ _) = sp


-- Item-lookup functions.  For each `fooBars :: Foo -> Seq Bar` field above,
-- we define a singular `fooBar :: Foo -> Int -> Bar` that combines the field
-- access and sequence indexing.

designLookup what i mx = case mx of
    Nothing -> error $ "design has no " ++ what ++ " with index " ++ show i
    Just x -> x

designMod :: Design ann -> ModId -> Module ann
designMod d i = designLookup "mod" i $ designMods d S.!? i

moduleLookup m what i mx = case mx of
    Nothing -> error $ "module " ++ show (moduleName m) ++
        " has no " ++ what ++ " with index " ++ show i
    Just x -> x

moduleParam :: Module ann -> Int -> Param
moduleParam m i = moduleLookup m "param" i $ moduleParams m S.!? i

moduleInput :: Module ann -> Int -> Port
moduleInput m i = moduleLookup m "input" i $ moduleInputs m S.!? i

moduleOutput :: Module ann -> Int -> Port
moduleOutput m i = moduleLookup m "output" i $ moduleOutputs m S.!? i

moduleSidePort :: Module ann -> Side -> Int -> Port
moduleSidePort m Source i = moduleInput m i
moduleSidePort m Sink i = moduleOutput m i

moduleLogic :: Module ann -> Int -> Logic ann
moduleLogic m i = moduleLookup m "logic" i $ moduleLogics m S.!? i

moduleNet :: Module ann -> NetId -> Net ann
moduleNet m (NetId i) = moduleLookup m "net" i $ moduleNets m S.!? i

logicLookup l what i mx = case mx of
    Nothing ->
        let name = case logicKind l of
                LkInst inst -> "logic " ++ show (instName inst)
                _ -> "logic"
        in error $ name ++ " has no " ++ what ++ " with index " ++ show i
    Just x -> x

logicInput :: Logic ann -> Int -> Pin
logicInput l i = logicLookup l "input" i $ logicInputs l S.!? i

logicOutput :: Logic ann -> Int -> Pin
logicOutput l i = logicLookup l "output" i $ logicOutputs l S.!? i

logicSidePin :: Logic ann -> Side -> Int -> Pin
logicSidePin l Source i = logicOutput l i
logicSidePin l Sink i = logicInput l i

netLookup m what i mx = case mx of
    Nothing -> error $ "net " ++ show (head $ T.lines $ netName m) ++
        " has no " ++ what ++ " with index " ++ show i
    Just x -> x

netSource :: Net ann -> Int -> Conn
netSource n i = netLookup n "source" i $ netSources n S.!? i

netSink :: Net ann -> Int -> Conn
netSink n i = netLookup n "sink" i $ netSinks n S.!? i

netSideConn :: Net ann -> Side -> Int -> Conn
netSideConn n Source i = netSource n i
netSideConn n Sink i = netSink n i



class Annotated t where
    gatherAnn :: t ann -> [ann]
    scatterAnn' :: t ann' -> State [ann] (t ann)

nextAnn :: State [a] a
nextAnn = state $ \xs -> case xs of
    [] -> error $ "too few annotations in scatter"
    x : xs -> (x, xs)

instance Annotated Design where
    gatherAnn (Design mods) = foldMap gatherAnn mods
    scatterAnn' (Design mods) = Design <$> mapM scatterAnn' mods

instance Annotated Module where
    gatherAnn (Module _ _ _ _ logics nets _) =
        foldMap gatherAnn logics ++
        foldMap gatherAnn nets
    scatterAnn' (Module name params ins outs logics nets cons) = Module
        <$> pure name
        <*> pure params
        <*> pure ins
        <*> pure outs
        <*> mapM scatterAnn' logics
        <*> mapM scatterAnn' nets
        <*> pure cons

instance Annotated Logic where
    gatherAnn (Logic _ _ _ ann) = [ann]
    scatterAnn' (Logic kind ins outs _) = Logic
        <$> pure kind
        <*> pure ins
        <*> pure outs
        <*> nextAnn

instance Annotated Net where
    gatherAnn (Net _ _ _ _ _ ann) = [ann]
    scatterAnn' (Net name prio sources sinks ty _) = Net
        <$> pure name
        <*> pure prio
        <*> pure sources
        <*> pure sinks
        <*> pure ty
        <*> nextAnn

scatterAnn anns x = case runState (scatterAnn' x) anns of
    (x', []) -> x'
    (_, _ : _) -> error $ "too many annotations in scatter"


constAnn :: Annotated t => ann -> t ann' -> t ann
constAnn ann x = fst $ runState (scatterAnn' x) (repeat ann)

mapAnn :: Annotated t => (ann -> ann') -> t ann -> t ann'
mapAnn f x = scatterAnn (map f $ gatherAnn x) x

zipAnn :: Annotated t => t ann1 -> t ann2 -> t (ann1, ann2)
zipAnn x y = scatterAnn (zip (gatherAnn x) (gatherAnn y)) x

zipAnnWith :: Annotated t => (a -> b -> c) -> t a -> t b -> t c
zipAnnWith f x y = scatterAnn (zipWith f (gatherAnn x) (gatherAnn y)) x



-- Lenses

makeLenses' ''Design
makeLenses' ''Module
makeLenses' ''Param
makeLenses' ''Port
makeLenses' ''Logic
makeLenses' ''Net
makeLenses' ''Inst
makeLenses' ''Pin
makeLenses' ''Ty

_designMod :: Int -> Lens' (Design ann) (Module ann)
_designMod i = _designMods . singular (ix i)

_moduleParam :: Int -> Lens' (Module ann) Param
_moduleParam i = _moduleParams . singular (ix i)
_moduleInput :: Int -> Lens' (Module ann) Port
_moduleInput i = _moduleInputs . singular (ix i)
_moduleOutput :: Int -> Lens' (Module ann) Port
_moduleOutput i = _moduleOutputs . singular (ix i)
_moduleSidePort Source i = _moduleInputs i
_moduleSidePort Sink i = _moduleOutputs i
_moduleLogic :: Int -> Lens' (Module ann) (Logic ann)
_moduleLogic i = _moduleLogics . singular (ix i)
_moduleNet :: NetId -> Lens' (Module ann) (Net ann)
_moduleNet i = _moduleNets . singular (ix $ unwrapNetId i)

_logicInput :: Int -> Lens' (Logic ann) Pin
_logicInput i = _logicInputs . singular (ix i)
_logicOutput :: Int -> Lens' (Logic ann) Pin
_logicOutput i = _logicOutputs . singular (ix i)
_logicSidePin Source i = _logicOutput i
_logicSidePin Sink i = _logicInput i

_netSource :: Int -> Lens' (Net ann) Conn
_netSource i = _netSources . singular (ix i)
_netSink :: Int -> Lens' (Net ann) Conn
_netSink i = _netSinks . singular (ix i)
_netSideConn Source i = _netSource i
_netSideConn Sink i = _netSink i
