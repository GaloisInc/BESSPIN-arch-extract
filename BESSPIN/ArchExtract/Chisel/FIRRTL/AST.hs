{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass,
    StandaloneDeriving, OverloadedStrings #-}
module BESSPIN.ArchExtract.Chisel.FIRRTL.AST where

import Control.DeepSeq
import Control.Monad
import Data.Data
import Data.Generics hiding (Generic)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics

import qualified Codec.CBOR.Term as CBOR


data Circuit = Circuit
    { circuitInfo :: SourceInfo
    , circuitModules :: [Module]
    , circuitMain :: Text
    }
    deriving (Show, Typeable, Data, Generic, NFData)

data SourceInfo = SourceInfo [Text]
    deriving (Show, Typeable, Data, Generic, NFData)

instance Semigroup SourceInfo where
    SourceInfo a <> SourceInfo b = SourceInfo $ a <> b

instance Monoid SourceInfo where
    mempty = SourceInfo []

data Module = Module
    { moduleInfo :: SourceInfo
    , moduleName :: Text
    , modulePorts :: [Port]
    , moduleKind :: ModuleKind
    }
    deriving (Show, Typeable, Data, Generic, NFData)

data ModuleKind =
    MkNormal
    { mkNormalBody :: Stmt
    } |
    MkExt
    { mkExtDefName :: Text
    --, mkExtParams :: [?]
    }
    deriving (Show, Typeable, Data, Generic, NFData)

data Direction = Input | Output
    deriving (Show, Typeable, Data, Generic, NFData)

data Port = Port
    { portInfo :: SourceInfo
    , portName :: Text
    , portDir :: Direction
    , portTy :: Ty
    }
    deriving (Show, Typeable, Data, Generic, NFData)


data Ty =
      TUInt (Maybe Int)
    | TSInt (Maybe Int)
    -- Width + point position
    | TFixed (Maybe Int) (Maybe Int)
    | TBundle [Field]
    | TVector Ty Int
    | TClock
    | TAnalog (Maybe Int)
    | TUnknown
    deriving (Show, Typeable, Data, Generic, NFData)

data Field = Field
    { fieldName :: Text
    , fieldTy :: Ty
    , fieldFlip :: Bool
    }
    deriving (Show, Typeable, Data, Generic, NFData)

data Stmt =
      SDef SourceInfo Def
    | SCond SourceInfo Expr Stmt Stmt
    | SBlock [Stmt]
    | SPartialConnect SourceInfo Expr Expr
    | SConnect SourceInfo Expr Expr
    | SIsInvalid SourceInfo Expr
    | SAttach SourceInfo [Expr]
    | SStop SourceInfo Int Expr Expr
    | SPrint SourceInfo Text [Expr] Expr Expr
    | SEmpty
    deriving (Show, Typeable, Data, Generic, NFData)

data Def =
      DWire Text Ty
    | DReg Text Ty Expr Expr Expr
    | DInst Text Text
    | DMemory Text Ty Int [Text] [Text] [Text]
    | DNode Text Expr
    | DCMem Text Ty Int Bool
    | DCMemPort Text Ty Text [Expr] MemPortDir
    deriving (Show, Typeable, Data, Generic, NFData)

data MemPortDir = MpdInfer | MpdRead | MpdWrite | MpdReadWrite
    deriving (Show, Typeable, Data, Generic, NFData)

data Expr =
      ELit Lit
    | ERef Text Ty
    | EField Expr Text Ty
    | EIndex Expr Expr Ty
    -- Index by a constant
    | EIndexC Expr Int Ty
    | EMux Expr Expr Expr Ty
    | EValidIf Expr Expr Ty
    | EPrim Text [Expr] [Integer] Ty
    deriving (Show, Typeable, Data, Generic, NFData)

data Lit =
      LUInt Integer (Maybe Int)
    | LSInt Integer (Maybe Int)
    | LFixed Integer (Maybe Int) (Maybe Int)
    deriving (Show, Typeable, Data, Generic, NFData)



