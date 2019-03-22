{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module BESSPIN.ArchExtract.BSV.Raw where

import Control.Monad
import Data.Data
import Data.Sequence (Seq)
import Data.Text (Text)

import qualified Codec.CBOR.Term as CBOR


data Package = Package
    { packageId :: Id
    , packageDefs :: Seq Def
    }
    deriving (Show, Data, Typeable)

data Def = Def
    { defId :: Id
    , defTy :: Ty
    , defClauses :: [Clause]
    }
    deriving (Show, Data, Typeable)

data Clause = Clause
    { clausePats :: [Pat]
    , clauseBody :: Expr
    }
    deriving (Show, Data, Typeable)

data Expr =
    -- Standard functional constructs
      EVar Id
    | ELam [Pat] Expr
    | EApp Expr [Ty] [Expr]
    | ELet Def Expr
    | ELetRec [Def] Expr

    -- BSV-specific primitives
    | ELit Lit
    | ERules [Rule]
    | EStatic Id Id     -- parent name, field name
    | EStruct Ty [(Id, Expr)]

    -- Raised expressions
    | EPrim Prim
    | EDo [Stmt] Expr
    -- Raised form of `ERules`.  The `Text` is the name of the rule.
    | EAddRules [(Maybe Text, Expr)]
    | ETcDict   -- Replacement for elided `_tcdict` `EVar`s
    | ERegRead Expr
    | ERegWrite Expr Expr  -- reg, value
    | EBinOp Text Expr Expr

    | EUnknown CBOR.Term
    deriving (Show, Data, Typeable)

data Prim =
      PReturn -- a -> m a
    | PMkRegU -- forall ty width. m ?  -- width is the total width in bits
    | PPack   -- bits -> a
    | PUnpack -- a -> bits
    deriving (Show, Data, Typeable)

data Rule =
      RRule (Maybe Expr) Expr
    | RUnknown CBOR.Term
    deriving (Show, Data, Typeable)

data Lit =
      LStr Text
    | LChar Char
    | LInt Integer
    | LDouble Double
    deriving (Show, Data, Typeable)

data Stmt =
      SBind Pat Expr
    | SBind' Expr
    deriving (Show, Data, Typeable)

data Pat =
      PVar Id
    | PUnknown CBOR.Term
    deriving (Show, Data, Typeable)

data Ty =
      TVar Id
    | TCon Id
    | TNat Int
    | TApp Ty [Ty]

    | TArrow Ty Ty

    | TUnknown CBOR.Term
    deriving (Show, Data, Typeable)

data Id = Id Text Int Int
    deriving (Show, Eq, Ord, Data, Typeable)

deriving instance Data CBOR.Term
