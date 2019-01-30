{-# LANGUAGE DeriveDataTypeable #-}
module BESSPIN.ArchExtract.Architecture where

import Control.Monad.State
import Data.Data
import Data.Ix
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import Data.Typeable


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
    , moduleInputs :: Seq Port
    , moduleOutputs :: Seq Port
    , moduleLogics :: Seq (Logic ann)
    , moduleNets :: Seq (Net ann)
    }
    deriving (Show, Typeable, Data)

-- A blob of anonymous logic.
data Logic ann = Logic
    { logicKind :: LogicKind
    , logicInputs :: Seq NetId
    , logicOutputs :: Seq NetId
    , logicAnn :: ann
    }
    deriving (Show, Typeable, Data)

data LogicKind =
    LkInst Inst |
    LkNetAlias |
    LkRegister Text |
    LkOther
    deriving (Show, Eq, Typeable, Data)

data Inst = Inst 
    { instModId :: ModId
    , instName :: Text
    }
    deriving (Show, Eq, Typeable, Data)

data Port = Port
    { portName :: Text
    , portNet :: NetId
    }
    deriving (Show, Typeable, Data)

data Net ann = Net
    { netName :: Text
    -- Used to select a name to use when merging nets during net alias handling
    , netNamePriority :: Int
    , netSources :: Seq Conn
    , netSinks :: Seq Conn
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

moduleInput :: Module ann -> Int -> Port
moduleInput m i = moduleInputs m `S.index` i

moduleOutput :: Module ann -> Int -> Port
moduleOutput m i = moduleOutputs m `S.index` i

moduleLogic :: Module ann -> Int -> Logic ann
moduleLogic m i = moduleLogics m `S.index` i

moduleNet :: Module ann -> NetId -> Net ann
moduleNet m (NetId i) = moduleNets m `S.index` i

logicInput :: Logic ann -> Int -> NetId
logicInput l i = logicInputs l `S.index` i

logicOutput :: Logic ann -> Int -> NetId
logicOutput l i = logicOutputs l `S.index` i

netSource :: Net ann -> Int -> Conn
netSource n i = netSources n `S.index` i

netSink :: Net ann -> Int -> Conn
netSink n i = netSinks n `S.index` i



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
    gatherAnn (Module _ _ _ logics nets) =
        foldMap gatherAnn logics ++
        foldMap gatherAnn nets
    scatterAnn' (Module name ins outs logics nets) = Module
        <$> pure name
        <*> pure ins
        <*> pure outs
        <*> mapM scatterAnn' logics
        <*> mapM scatterAnn' nets

instance Annotated Logic where
    gatherAnn (Logic _ _ _ ann) = [ann]
    scatterAnn' (Logic kind ins outs _) = Logic
        <$> pure kind
        <*> pure ins
        <*> pure outs
        <*> nextAnn

instance Annotated Net where
    gatherAnn (Net _ _ _ _ ann) = [ann]
    scatterAnn' (Net name prio sources sinks _) = Net
        <$> pure name
        <*> pure prio
        <*> pure sources
        <*> pure sinks
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
