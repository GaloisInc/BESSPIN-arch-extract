module BESSPIN.ArchExtract.Architecture where

import Data.Sequence (Seq)
import Data.Text (Text)


-- The ID of a module is its position in `designMods`.
type ModId = Int
-- The ID of a net is its position in `modDeclNets`.
type NetId = Int

data Design = Design
    { designMods :: Seq ModDecl
    }
    deriving (Show)

data ModDecl = ModDecl
    { modDeclName :: Text
    , modDeclInputs :: Seq PortDecl
    , modDeclOutputs :: Seq PortDecl
    , modDeclInsts :: Seq ModInst
    , modDeclLogics :: Seq Logic
    , modDeclNets :: Seq Net
    }
    deriving (Show)

-- An instance of a module.
data ModInst = ModInst
    { modInstId :: ModId
    , modInstName :: Text
    , modInstInputs :: Seq NetId
    , modInstOutputs :: Seq NetId
    }
    deriving (Show)

-- A blob of anonymous logic.
data Logic = Logic
    { logicInputs :: Seq NetId
    , logicOutputs :: Seq NetId
    }
    deriving (Show)

data PortDecl = PortDecl
    { portDeclName :: Text
    , portDeclNet :: NetId
    }
    deriving (Show)

data Net = Net
    { netName :: Text
    , netSources :: Seq Conn
    , netSinks :: Seq Conn
    }
    deriving (Show)

-- The first `Int` is an index into `Insts`/`Logics` (`ExtPort` doesn't have
-- one of these, since there is only one containing module).  The second is the
-- index of the corresponding entry in the `Inputs`/`Outputs` list (`Inputs` if
-- this `Conn` is on the `netSinks` side of the `Net`, `Outputs` if it's on the
-- `netSources` side.)
data Conn = ExtPort Int | InstPort Int Int | LogicPort Int Int
    deriving (Show)
