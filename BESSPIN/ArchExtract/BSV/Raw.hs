{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass,
   StandaloneDeriving #-}
module BESSPIN.ArchExtract.BSV.Raw where

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


-- Some nodes have unique IDs assigned so we can associate errors and other
-- messages with those nodes.
type NodeId = Int

data Package = Package
    { packageId :: Id
    , packageImports :: [Id]
    , packageDefs :: Seq Def
    , packageStructs :: Seq Struct
    , packageTypedefs :: Seq Typedef
    }
    deriving (Show, Data, Typeable, Generic, NFData)

data Struct = Struct
    { structId :: Id
    , structTyParams :: [Id]
    , structFields :: [Field]
    , structIsIfc :: Bool
    }
    deriving (Show, Data, Typeable, Generic, NFData)

-- A field that contains a function may have a list of default arg names
-- associated with it.
data Field = Field Id Ty (Maybe [Id])
    deriving (Show, Data, Typeable, Generic, NFData)

data Typedef = Typedef
    { typedefId :: Id
    , typedefTyParams :: [Id]
    , typedefTy :: Ty
    }
    deriving (Show, Data, Typeable, Generic, NFData)

data Def = Def
    { defId :: Id
    , defTy :: Ty
    , defClauses :: [Clause]
    }
    deriving (Show, Data, Typeable, Generic, NFData)

data Clause = Clause
    { clausePats :: [Pat]
    , clauseBody :: Expr
    }
    deriving (Show, Data, Typeable, Generic, NFData)

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

    | EUndef    -- Generate an undefined value of appropriate type

    | EUnknown CBOR.Term
    deriving (Show, Data, Typeable, Generic, NFData)

data Prim =
      PReturn -- a -> m a
    | PBind   -- m a -> (a -> m b) -> m b
    | PMkReg  -- forall ty width. ty -> m Reg
    | PMkRegU -- forall ty width. m Reg  -- width is the total width in bits
    | PPack   -- bits -> a
    | PUnpack -- a -> bits
    | PIndex    -- args: array, index
    | PSlice    -- args: array, high index, low index
    | PRegRead  -- reg -> value
    | PRegWrite -- reg -> value -> Action
    | PUnOp Text
    | PBinOp Text
    | PResize Text  -- Takes a `TNat` parameter to indicate output width
    | PIf       -- forall a. Bool -> a -> a -> a
    | PSetName Text -- Module a -> Module a
    deriving (Show, Data, Typeable, Generic, NFData)

data RawRule =
      RrRule (Maybe Expr) [Guard] Expr
    | RrUnknown CBOR.Term
    deriving (Show, Data, Typeable, Generic, NFData)

data Rule = Rule
    { ruleName :: Maybe Text
    , ruleConds :: [Expr]
    , ruleBody :: Expr
    }
    deriving (Show, Data, Typeable, Generic, NFData)

data Guard =
      GCond Expr
    | GPat Pat Ty Expr
    deriving (Show, Data, Typeable, Generic, NFData)

data Lit =
      LStr Text
    | LChar Char
    | LInt Integer
    | LDouble Double
    deriving (Show, Data, Typeable, Generic, NFData)

data Stmt =
    -- Final `Int` is a unique ID, used for attaching comments/error messages
    -- (as `SNote`s) to particular statements.
      SBind Pat Ty Expr NodeId
    | SBind' Expr NodeId
    | SNote Text
    deriving (Show, Data, Typeable, Generic, NFData)

data Pat =
      PWild
    | PVar Id
    | PTcDict   -- Replacement for elided `_tcdict` `PVar`s
    | PUnknown CBOR.Term
    deriving (Show, Data, Typeable, Generic, NFData)

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

    -- A reference to a type alias, which resolves to the second field.
    | TAlias Id Ty
    -- A type-level lambda.  These appear when resolving typedefs that have
    -- type parameters.
    --
    -- NB: Correctness of `betaReduceTy` below depends on the body of `TLam`
    -- having no free variables beyond those listed in the first argument.
    -- That is, `\x -> x` is okay, but `\x -> y` is not.
    | TLam [Id] Ty

    | TUnknown CBOR.Term
    deriving (Show, Data, Typeable, Generic, NFData)

data Id = Id Text Int Int
    deriving (Show, Eq, Ord, Data, Typeable, Generic, NFData)

idName (Id name _ _) = name

deriving instance Data CBOR.Term
deriving instance Generic CBOR.Term
deriving instance NFData CBOR.Term


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


-- Compute the most-resolved version of a type, by expanding all type aliases
-- and beta-reducing.
resolveTy :: Ty -> Ty
resolveTy t = betaReduceTy $ everywhere (mkT go) t
  where
    go (TAlias _ t) = t
    go t = t

betaReduceTy :: Ty -> Ty
betaReduceTy t = everywhere (mkT go) t
  where
    go (TApp (TLam is body) ts) | length is == length ts =
        let m = M.fromList (zip (map idName is) ts) in
        substTy m body
    go t = t

-- Substitute according to `m` in `t`.
--
-- NB: It's okay if there are free variables in the RHSs of `m`, and it's okay
-- if there are free variables in `t` (beyond the vars in the LHSs of `m`), but
-- it is NOT okay if both are true at the same time.  Consider:
--      m: [a -> x]
--      t: \x y -> a
-- The two `x`s are distinct, but will be mixed up if you perform this
-- substitution.
--
-- We further assume that `TLam` bodies don't have free variables (beyond the
-- lambda's arguments), which means we don't need to do anything special at all
-- for binders.
substTy :: Map Text Ty -> Ty -> Ty
substTy m t = everywhere (mkT go) t
  where
    go (TVar i) = fromMaybe (error $ "no binding for ty var " ++ show i) $
        M.lookup (idName i) m
    go t = t
