module BESSPIN.ArchExtract.Verilog.TypeCheck where

import Data.Foldable
import Data.Generics
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro.Platform

import Debug.Trace

import BESSPIN.ArchExtract.Verilog.AST hiding (Ty(..), BaseType(..))
import qualified BESSPIN.ArchExtract.Verilog.AST as V
import BESSPIN.ArchExtract.Architecture (Ty(..))
import qualified BESSPIN.ArchExtract.Architecture as A


resolve :: Ty -> Ty
resolve (TWire ws ds) = TWire ws ds
resolve (TEnum base) = resolve base
resolve (TAlias _ t) = resolve t
resolve TSimVal = TSimVal
resolve TUnknown = TUnknown

exprType :: (Int -> Ty) -> Expr -> Ty
exprType varType e = go e
  where
    go (Var i) = varType i
    go (Param i) = TWire 1 0    -- TODO
    go (Index vec (ISingle _)) = case resolve $ go vec of
        TWire ws 0 -> TWire (max 0 $ ws - 1) 0
        t@(TWire _ _) -> warn "index (single)" t
        t -> warn "index (single)" t
    go (Index vec (IRange _ _)) = case resolve $ go vec of
        TWire ws 0 -> TWire ws 0
        t@(TWire _ _) -> warn "index (range)" t
        t -> warn "index (range)" t
    -- `MemIndex` indexes should always be `ISingle`.
    go (MemIndex mem ixs) = case resolve $ go mem of
        TWire ws ds -> TWire ws (max 0 $ ds - 1)
        t -> warn "memindex (single)" t
    -- This is just a bad guess at a possible type.  TODO: actually track types
    -- for constants.
    go (Const _) = TWire 1 0
    go (ConstBool _ _) = TWire 0 0
    go (Concat es) = TWire 1 0
    go (MultiConcat rep es) = TWire 1 0
    go (IfExpr _ t e) = commonTy "ifexpr branch mismatch" (go t) (go e)
    go (Unary op e) = case op of
        UNeg -> go e
        UNot -> go e
        ULogNot -> TWire 0 0
        UReduce _ -> TWire 0 0
        UReduceNot _ -> TWire 0 0
        UOther -> TUnknown
    go (Binary op l r) = case op of
        BAdd -> commonTy "binop operand mismatch" (go l) (go r)
        BSub -> commonTy "binop operand mismatch" (go l) (go r)
        BMul -> commonTy "binop operand mismatch" (go l) (go r)
        BDiv -> commonTy "binop operand mismatch" (go l) (go r)
        BMod -> commonTy "binop operand mismatch" (go l) (go r)
        BAnd -> commonTy "binop operand mismatch" (go l) (go r)
        BOr -> commonTy "binop operand mismatch" (go l) (go r)
        BXor -> commonTy "binop operand mismatch" (go l) (go r)
        BLogAnd -> TWire 0 0
        BLogOr -> TWire 0 0
        BEq -> TWire 0 0
        BNe -> TWire 0 0
        BLt -> TWire 0 0
        BLe -> TWire 0 0
        BGt -> TWire 0 0
        BGe -> TWire 0 0
        BShl -> go l
        BShr -> go l
        BOther -> TUnknown
    go (Field _ _) = TUnknown
    go (AssignPat _ _) = TUnknown
    go UnknownExpr = TUnknown

    commonTy msg t1 t2 = if t1 /= t2 then warn msg (t1, t2) else t1

    warn msg t = trace ("type error: " ++ msg ++ " on type " ++ show t) TUnknown

