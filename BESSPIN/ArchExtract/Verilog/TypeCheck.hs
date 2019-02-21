{-# LANGUAGE OverloadedStrings #-}
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
import Text.Read

import Debug.Trace

import BESSPIN.ArchExtract.Verilog.AST hiding (Ty(..), TTy, TEnum, TRef, BaseType(..))
import qualified BESSPIN.ArchExtract.Verilog.AST as V
import BESSPIN.ArchExtract.Architecture (Ty(..), ConstExpr(..))
import qualified BESSPIN.ArchExtract.Architecture as A


resolve :: Ty -> Ty
resolve (TWire ws ds) = TWire ws ds
resolve (TEnum base) = resolve base
resolve (TAlias _ t) = resolve t
resolve TUnsizedInt = TUnsizedInt
resolve TSimVal = TSimVal
resolve TUnknown = TUnknown

exprType :: Monad m => (Int -> m Ty) -> (Expr -> m ConstExpr) -> Expr -> m Ty
exprType varType convExpr e = go e
  where
    -- TODO: check orders of nested dimensions / indices in Index and MemIndex
    -- cases.  It's likely backwards in at least a few places.
    -- go :: Expr -> m Ty
    go (Var i) = resolve <$> varType i
    go (Param i) = return $ TWire [EIntLit 32] []    -- TODO
    go (Index vec (ISingle _)) = (resolve <$> go vec) >>= \ty -> case ty of
        TWire (w : ws) [] -> return $ TWire ws []
        t -> return $ warn "index (single)" t
    go (Index vec (IRange l r)) = (resolve <$> go vec) >>= \ty -> case ty of
        TWire (w : ws) [] -> do
            l <- convExpr l
            r <- convExpr r
            return $ TWire [ERangeSize l r] []
        t -> return $ warn "index (range)" t
    -- `MemIndex` indexes should always be `ISingle`.
    go (MemIndex mem []) = (resolve <$> go mem) >>= \ty -> case ty of
        TWire ws (d : ds) -> return $ TWire ws ds
        t -> return $ warn "memindex (single)" t
    go (Const t) | Just width <- bitConstSize t = return $ TWire [EIntLit width] []
    go (Const _) = return TUnknown
    -- This is just a bad guess at a possible type.  TODO: actually track types
    -- for constants.
    go (ConstInt _ _) = return $ TUnsizedInt
    go (ConstBool _ _) = return $ TWire [] []
    go (Concat es) = sumWidths es >>= \w -> case w of
        Nothing -> return TUnknown
        Just w -> return $ TWire [w] []
    go (MultiConcat rep es) = sumWidths es >>= \w -> case w of
        Nothing -> return TUnknown
        Just w -> do
            r <- convExpr rep
            return $ TWire [EBinArith A.BMul r w] []
    go (IfExpr _ t e) = commonTy "ifexpr branch mismatch" <$> go t <*> go e
    go (Unary op e) = case op of
        UNeg -> go e
        UNot -> go e
        ULogNot -> return $ TWire [] []
        UReduce _ -> return $ TWire [] []
        UReduceNot _ -> return $ TWire [] []
        UOther -> return $ TUnknown
    go (Binary op l r) = case op of
        BAdd -> commonTy "binop operand mismatch" <$> go l <*> go r
        BSub -> commonTy "binop operand mismatch" <$> go l <*> go r
        BMul -> commonTy "binop operand mismatch" <$> go l <*> go r
        BDiv -> commonTy "binop operand mismatch" <$> go l <*> go r
        BMod -> commonTy "binop operand mismatch" <$> go l <*> go r
        BAnd -> commonTy "binop operand mismatch" <$> go l <*> go r
        BOr -> commonTy "binop operand mismatch" <$> go l <*> go r
        BXor -> commonTy "binop operand mismatch" <$> go l <*> go r
        BLogAnd -> return $ TWire [] []
        BLogOr -> return $ TWire [] []
        BEq -> return $ TWire [] []
        BNe -> return $ TWire [] []
        BLt -> return $ TWire [] []
        BLe -> return $ TWire [] []
        BGt -> return $ TWire [] []
        BGe -> return $ TWire [] []
        BShl -> go l
        BShr -> go l
        BOther -> return $ TUnknown
    go (Builtin op es) = case op of
        BkClog2 -> return $ TSimVal
        BkSize -> return $ TSimVal
    go (Field _ _) = return $ TUnknown
    go (AssignPat _ _) = return $ TUnknown
    go UnknownExpr = return $ TUnknown

    commonTy _ t1@(TWire _ []) TUnsizedInt = t1
    commonTy _ TUnsizedInt t2@(TWire _ []) = t2
    commonTy msg t1 t2 = if t1 /= t2 then warn msg (t1, t2) else t1

    tyWidth :: Ty -> Maybe ConstExpr
    tyWidth (TWire [] []) = Just $ EIntLit 1
    tyWidth (TWire ws []) = Just $ foldl1 (EBinArith A.BMul) ws
    tyWidth _ = Nothing

    -- sumWidths :: [Expr] -> m (Maybe ConstExpr)
    sumWidths es = do
        tys <- mapM go es
        return $ foldl1 (EBinArith A.BAdd) <$> mapM (tyWidth . resolve) tys

    warn msg t = trace ("type error: " ++ msg ++ " on type " ++ show t) TUnknown


bitConstSize :: Text -> Maybe Int
bitConstSize t = readMaybe $ T.unpack width
  where
    (width, rest) = T.breakOn "'" t
