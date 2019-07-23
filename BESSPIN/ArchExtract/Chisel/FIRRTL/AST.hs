{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass,
    StandaloneDeriving, OverloadedStrings, TemplateHaskell #-}
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

import BESSPIN.ArchExtract.Lens


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
    MkExtern
    { mkExternDefName :: Text
    --, mkExtParams :: [?]
    }
    deriving (Show, Typeable, Data, Generic, NFData)

data Direction = Input | Output
    deriving (Show, Eq, Typeable, Data, Generic, NFData)

data Port = Port
    { portInfo :: SourceInfo
    , portName :: Text
    , portDir :: Direction
    , portTy :: Ty
    }
    deriving (Show, Typeable, Data, Generic, NFData)


data Width = WInt Int | WUnknown
    deriving (Show, Eq, Typeable, Data, Generic, NFData)

data Ty =
      TUInt Width
    | TSInt Width
    -- Width + point position
    | TFixed Width Width
    | TBundle [Field]
    | TVector Ty Int
    | TClock
    | TAnalog Width
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
    deriving (Show, Eq, Typeable, Data, Generic, NFData)

-- `Just True` if this field access flips direction, `Just False` if it does
-- not, `Nothing` if unknown
type FieldFlip = Maybe Bool

data Expr =
      ELit Lit
    | ERef Text Ty
    | EField Expr Text FieldFlip Ty
    | EIndex Expr Expr Ty
    -- Index by a constant
    | EIndexC Expr Int Ty
    | EMux Expr Expr Expr Ty
    | EValidIf Expr Expr Ty
    | EPrim Text [Expr] [Integer] Ty
    deriving (Show, Typeable, Data, Generic, NFData)

data Lit =
      LUInt Integer Width
    | LSInt Integer Width
    | LFixed Integer Width Width
    deriving (Show, Typeable, Data, Generic, NFData)


isGroundTy ty = case ty of
    TBundle _ -> False
    TVector _ _ -> False
    _ -> True

isPassiveTy ty = case ty of
    TBundle fs -> all (\f -> not (fieldFlip f) && isPassiveTy (fieldTy f)) fs
    TVector ty _ -> isPassiveTy ty
    _ -> True



makeLenses' ''Circuit
makeLenses' ''Module
makeLenses' ''Port
makeLenses' ''Field
