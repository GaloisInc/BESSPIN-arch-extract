{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, Rank2Types #-}
module BESSPIN.ArchExtract.Architecture where

import Control.Monad.State
import Data.Data
import Data.Ix
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import Data.Typeable
import Lens.Micro.Platform

import BESSPIN.ArchExtract.Lens


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
    , moduleConstraints :: Seq ConstExpr
    }
    deriving (Show, Typeable, Data)


data Param = Param
    { paramName :: Text
    , paramDefault :: Maybe ConstExpr
    }
    deriving (Show, Typeable, Data)


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
    -- Type for values used only in simulation (int, string, etc).
    TSimVal |
    TUnknown
    deriving (Show, Eq, Typeable, Data)


data ConstExpr =
      EIntLit Int
    -- A parameter of the enclosing module
    | EParam Int
    -- A parameter of a (nested) module instantiation.  `EInstParam [i, j] k`
    -- corresponds to `inst_i.inst_j.param_k`, relative to the enclosing
    -- module.
    | EInstParam [Int] Int
    -- `a -> a`
    | EUnArith UnArithOp ConstExpr
    -- `a -> a -> a`
    | EBinArith BinArithOp ConstExpr ConstExpr
    -- `a -> a -> Bool`
    | EBinCmp BinCmpOp ConstExpr ConstExpr
    -- Size of the range `e1:e2`.  Equal to `abs(e1 - e2) + 1`.
    | ERangeSize ConstExpr ConstExpr
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


-- Item-lookup functions.  For each `fooBars :: Foo -> Seq Bar` field above,
-- we define a singular `fooBar :: Foo -> Int -> Bar` that combines the field
-- access and sequence indexing.

designMod :: Design ann -> ModId -> Module ann
designMod d i = designMods d `S.index` i

moduleParam :: Module ann -> Int -> Param
moduleParam m i = moduleParams m `S.index` i

moduleInput :: Module ann -> Int -> Port
moduleInput m i = moduleInputs m `S.index` i

moduleOutput :: Module ann -> Int -> Port
moduleOutput m i = moduleOutputs m `S.index` i

moduleSidePort :: Module ann -> Side -> Int -> Port
moduleSidePort m Source i = moduleInput m i
moduleSidePort m Sink i = moduleOutput m i

moduleLogic :: Module ann -> Int -> Logic ann
moduleLogic m i = moduleLogics m `S.index` i

moduleNet :: Module ann -> NetId -> Net ann
moduleNet m (NetId i) = moduleNets m `S.index` i

logicInput :: Logic ann -> Int -> Pin
logicInput l i = logicInputs l `S.index` i

logicOutput :: Logic ann -> Int -> Pin
logicOutput l i = logicOutputs l `S.index` i

logicSidePin :: Logic ann -> Side -> Int -> Pin
logicSidePin l Source i = logicOutput l i
logicSidePin l Sink i = logicInput l i

netSource :: Net ann -> Int -> Conn
netSource n i = netSources n `S.index` i

netSink :: Net ann -> Int -> Conn
netSink n i = netSinks n `S.index` i

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
