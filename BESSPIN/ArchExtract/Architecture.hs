{-# LANGUAGE DeriveDataTypeable #-}
module BESSPIN.ArchExtract.Architecture where

import Data.Data
import Data.Ix
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Typeable


-- The ID of a module is its position in `designMods`.
type ModId = Int
-- The ID of a net is its position in `modDeclNets`.
newtype NetId = NetId { unwrapNetId :: Int }
    deriving (Show, Eq, Ord, Ix, Typeable, Data)

instance Enum NetId where
    toEnum = NetId
    fromEnum = unwrapNetId

data Design = Design
    { designMods :: Seq ModDecl
    }
    deriving (Show, Typeable, Data)

data ModDecl = ModDecl
    { modDeclName :: Text
    , modDeclInputs :: Seq PortDecl
    , modDeclOutputs :: Seq PortDecl
    , modDeclInsts :: Seq ModInst
    , modDeclLogics :: Seq Logic
    , modDeclNets :: Seq Net
    }
    deriving (Show, Typeable, Data)

-- An instance of a module.
data ModInst = ModInst
    { modInstId :: ModId
    , modInstName :: Text
    , modInstInputs :: Seq NetId
    , modInstOutputs :: Seq NetId
    }
    deriving (Show, Typeable, Data)

-- A blob of anonymous logic.
data Logic = Logic
    { logicKind :: LogicKind
    , logicInputs :: Seq NetId
    , logicOutputs :: Seq NetId
    }
    deriving (Show, Typeable, Data)

data LogicKind = LkNetAlias | LkOther
    deriving (Show, Eq, Typeable, Data)

data PortDecl = PortDecl
    { portDeclName :: Text
    , portDeclNet :: NetId
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

-- The first `Int` is an index into `Insts`/`Logics` (`ExtPort` doesn't have
-- one of these, since there is only one containing module).  The second is the
-- index of the corresponding entry in the `Inputs`/`Outputs` list (`Inputs` if
-- this `Conn` is on the `netSinks` side of the `Net`, `Outputs` if it's on the
-- `netSources` side.)
data Conn = ExtPort Int | InstPort Int Int | LogicPort Int Int
    deriving (Show, Eq, Ord, Typeable, Data)

-- Enum for indicating a side of a net, inst, or logic.  For clarity, these are
-- named `Source` and `Sink` instead of `Input` and `Output` because
-- `Input`/`Output` have opposite meanings on external ports vs. logic/insts.
data Side = Source | Sink
    deriving (Show, Eq, Ord, Typeable, Data)
