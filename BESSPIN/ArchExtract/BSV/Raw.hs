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
    , packageStructs :: Seq Struct
    }
    deriving (Show, Data, Typeable)

data Struct = Struct
    { structId :: Id
    , structTyParams :: [Id]
    , structFields :: [Field]
    }
    deriving (Show, Data, Typeable)

data Field = Field Id Ty
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
    | ERules [RawRule]
    | EStatic Id Id     -- parent name, field name
    | EStruct Ty [(Id, Expr)]

    -- Raised expressions
    | EPrim Prim
    | EDo [Stmt] Expr
    -- Raised form of `ERules`.  The `Text` is the name of the rule.
    | EAddRules [Rule]
    | ETcDict   -- Replacement for elided `_tcdict` `EVar`s
    | ERegRead Expr
    | ERegWrite Expr Expr  -- reg, value
    | EUnOp Text Expr
    | EBinOp Text Expr Expr

    | EUnknown CBOR.Term
    deriving (Show, Data, Typeable)

data Prim =
      PReturn -- a -> m a
    | PBind   -- m a -> (a -> m b) -> m b
    | PMkReg  -- forall ty width. ty -> m Reg
    | PMkRegU -- forall ty width. m Reg  -- width is the total width in bits
    | PPack   -- bits -> a
    | PUnpack -- a -> bits
    | PTruncate
    | PIndex
    | PRegRead  -- reg -> value
    | PRegWrite -- reg -> value -> Action
    | PUnOp Text
    | PBinOp Text
    | PIf       -- forall a. Bool -> a -> a -> a
    | PSetName Text -- Module a -> Module a
    deriving (Show, Data, Typeable)

data RawRule =
      RrRule (Maybe Expr) [Guard] Expr
    | RrUnknown CBOR.Term
    deriving (Show, Data, Typeable)

data Rule = Rule
    { ruleName :: Maybe Text
    , ruleConds :: [Expr]
    , ruleBody :: Expr
    }
    deriving (Show, Data, Typeable)

data Guard =
      GCond Expr
    | GPat Pat Ty Expr
    deriving (Show, Data, Typeable)

data Lit =
      LStr Text
    | LChar Char
    | LInt Integer
    | LDouble Double
    deriving (Show, Data, Typeable)

data Stmt =
      SBind Pat Ty Expr
    | SBind' Expr
    deriving (Show, Data, Typeable)

data Pat =
      PWild
    | PVar Id
    | PTcDict   -- Replacement for elided `_tcdict` `PVar`s
    | PUnknown CBOR.Term
    deriving (Show, Data, Typeable)

data Ty =
      TVar Id
    | TCon Id
    | TNat Int
    | TApp Ty [Ty]
    | TForall [Id] Ty

    | TArrow Ty Ty
    | TReg Ty
    | TUnit
    | TBool
    | TBit Ty
    | TModule Ty
    -- The Action monad.  In BSV this monad is actually called `ActionValue`,
    -- and `Action` is an alias for `ActionValue ()`.
    | TAction Ty
    | TIsModule Ty Ty

    | TUnknown CBOR.Term
    deriving (Show, Data, Typeable)

data Id = Id Text Int Int
    deriving (Show, Eq, Ord, Data, Typeable)

idName (Id name _ _) = name

deriving instance Data CBOR.Term
