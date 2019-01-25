{-# LANGUAGE DeriveDataTypeable #-}
module BESSPIN.ArchExtract.Architecture where

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

data Design = Design
    { designMods :: Seq Module
    }
    deriving (Show, Typeable, Data)

data Module = Module
    { moduleName :: Text
    , moduleInputs :: Seq Port
    , moduleOutputs :: Seq Port
    , moduleLogics :: Seq Logic
    , moduleNets :: Seq Net
    }
    deriving (Show, Typeable, Data)

-- A blob of anonymous logic.
data Logic = Logic
    { logicKind :: LogicKind
    , logicInputs :: Seq NetId
    , logicOutputs :: Seq NetId
    }
    deriving (Show, Typeable, Data)

data LogicKind =
    LkInst Inst |
    LkNetAlias |
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

data Net = Net
    { netName :: Text
    -- Used to select a name to use when merging nets during net alias handling
    , netNamePriority :: Int
    , netSources :: Seq Conn
    , netSinks :: Seq Conn
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

designMod :: Design -> ModId -> Module
designMod d i = designMods d `S.index` i

moduleInput :: Module -> Int -> Port
moduleInput m i = moduleInputs m `S.index` i

moduleOutput :: Module -> Int -> Port
moduleOutput m i = moduleOutputs m `S.index` i

moduleLogic :: Module -> Int -> Logic
moduleLogic m i = moduleLogics m `S.index` i

moduleNet :: Module -> NetId -> Net
moduleNet m (NetId i) = moduleNets m `S.index` i

logicInput :: Logic -> Int -> NetId
logicInput l i = logicInputs l `S.index` i

logicOutput :: Logic -> Int -> NetId
logicOutput l i = logicOutputs l `S.index` i

netSource :: Net -> Int -> Conn
netSource n i = netSources n `S.index` i

netSink :: Net -> Int -> Conn
netSink n i = netSinks n `S.index` i
