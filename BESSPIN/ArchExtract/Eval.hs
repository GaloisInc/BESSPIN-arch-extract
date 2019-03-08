module BESSPIN.ArchExtract.Eval where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Writer
import Data.Array.ST
import Data.Bits
import Data.Foldable
import Data.Generics
import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro
import Lens.Micro.Platform
import qualified Data.UnionFind.ST as UF

import Debug.Trace

import BESSPIN.ArchExtract.Architecture

data Value = VInt Int | VBool Bool

-- Try to evaluate a constant expression.  Calls `getVar` to evaluate `EParam`
-- and `EInstParam` exprs, and `getOverride` for `EOverride`s.  The other
-- override variants are not yet supported.
eval :: ([Int] -> Int -> Maybe Value) -> (Int -> Value -> Maybe Value) ->
    ConstExpr -> Maybe Value
eval getVar getOverride e = go e
  where
    go (EIntLit _ n) = Just $ VInt n
    go (EParam _ p) = getVar [] p
    go (EInstParam _ is p) = getVar is p
    go (EUnArith _ UClog2 e) = go e >>= evalClog2
    go (EUnArith _ UIsPow2 e) = go e >>= evalIsPow2
    go (EBinArith _ op l r) = do { x <- go l; y <- go r; evalBinArith op x y }
    go (EBinCmp _ op l r) = do { x <- go l; y <- go r; evalBinCmp op x y }
    go (ERangeSize _ l r) = do { x <- go l; y <- go r; evalRangeSize x y }
    go (EOverride i e) = go e >>= getOverride i
    go (EOverrideLocalParam _ _) = Nothing
    go (EOverrideInstParam _ _ _) = Nothing

evalConst :: ConstExpr -> Maybe Value
evalConst e = eval (\_ _ -> Nothing) (\_ _ -> Nothing) e

evalClog2 :: Value -> Maybe Value
evalClog2 (VInt n)
  | n > 0 = Just $ VInt $ finiteBitSize n - countLeadingZeros (n - 1)
  | n == 0 = Just $ VInt 0
evalClog2 _ = Nothing

evalIsPow2 :: Value -> Maybe Value
evalIsPow2 (VInt n)
  | n > 0 = Just $ VBool $ n .&. (n - 1) == 0
  | n == 0 = Just $ VBool True
evalIsPow2 _ = Nothing

evalBinArith op (VInt l) (VInt r) = Just $ VInt $ case op of
    BAdd -> l + r
    BSub -> l - r
    BMul -> l * r

evalBinCmp op (VInt l) (VInt r) = Just $ VBool $ case op of
    BEq -> l == r
    BNe -> l /= r
    BLt -> l < r
    BLe -> l <= r
    BGt -> l > r
    BGe -> l >= r

evalRangeSize (VInt l) (VInt r) = Just $ VInt $ l - r + 1


