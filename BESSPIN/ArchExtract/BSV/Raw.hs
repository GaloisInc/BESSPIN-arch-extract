{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module BESSPIN.ArchExtract.BSV.Raw where

import Control.Monad
import Data.Data
import Data.Sequence (Seq)
import Data.Text (Text)

import qualified Codec.CBOR.Term as CBOR


-- Some nodes have unique IDs assigned so we can associate errors and other
-- messages with those nodes.
type NodeId = Int

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
    , structIsIfc :: Bool
    }
    deriving (Show, Data, Typeable)

-- A field that contains a function may have a list of default arg names
-- associated with it.
data Field = Field Id Ty (Maybe [Id])
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
    -- Let bindings have NodeIds and messages associated with the `Def`'s RHS.
    | ELet Def Expr NodeId [Text]
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
    | EConst Text   -- always evaluates to a constant

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
    | PIndex    -- args: array, index
    | PSlice    -- args: array, high index, low index
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
    -- Final `Int` is a unique ID, used for attaching comments/error messages
    -- (as `SNote`s) to particular statements.
      SBind Pat Ty Expr NodeId
    | SBind' Expr NodeId
    | SNote Text
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
    | TIfc Id
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


-- Split a function type into a list of type variables, a list of argument
-- types, and a return type.
splitFnTy :: Ty -> ([Id], [Ty], Ty)
splitFnTy (TForall tyVars ty') =
    let (vars', args', ret') = splitFnTy ty' in
    (tyVars ++ vars', args', ret')
splitFnTy (TArrow argTy ty') =
    let (vars', args', ret') = splitFnTy ty' in
    (vars', argTy : args', ret')
splitFnTy ty = ([], [], ty)

buildFnTy :: [Id] -> [Ty] -> Ty -> Ty
buildFnTy [] [] retTy = retTy
buildFnTy [] (argTy : argTys) retTy = TArrow argTy $ buildFnTy [] argTys retTy
buildFnTy tyVars argTys retTy = TForall tyVars $ buildFnTy [] argTys retTy

splitLambda :: Expr -> ([Pat], Expr)
splitLambda (ELam ps e') =
    let (pats', body') = splitLambda e' in
    (ps ++ pats', body')
splitLambda e = ([], e)

buildLambda :: [Pat] -> Expr -> Expr
buildLambda [] e = e
buildLambda ps e = ELam ps e

splitLet :: Expr -> ([Def], Expr)
splitLet (ELet d e' _ _) =
    let (defs', body') = splitLet e' in
    (d : defs', body')
splitLet e = ([], e)

-- Split an application type into the base type constructor and a list of
-- arguments.
splitAppTy :: Ty -> (Ty, [Ty])
splitAppTy t = go [] t
  where
    go args (TApp a b) = go (b ++ args) a
    go args t = (t, args)
