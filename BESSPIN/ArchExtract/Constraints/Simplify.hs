{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Constraints.Simplify where

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
import BESSPIN.ArchExtract.Eval
import BESSPIN.ArchExtract.Print
import BESSPIN.ArchExtract.Constraints.Flat


isValid e = everything (&&) (True `mkQ` go) e
  where
    -- Explicit enumeration of supported expression types
    go (EIntLit _ _) = True
    go (EParam _ _) = True
    -- EInstParam unsupported: callers should run simplification only on
    -- flattened constraints.
    go (EUnArith _ _ _) = True
    go (EBinArith _ _ _ _) = True
    go (EBinCmp _ _ _ _) = True
    -- ERangeSize is only partially supported.  Callers should replace
    -- ERangeSize with explicit subtraction, if they're okay with the
    -- require-big-endian-vectors option.  Otherwise, we can only handle
    -- constant ERangeSize expressions - anything involving a variable is
    -- non-invertible, due to the `abs` inside `range-size`
    go (ERangeSize _ _ _) = True
    go (EOverride _ _) = True
    go _ = False

containsVar v e = everything (||) (False `mkQ` go) e
  where
    go (EParam _ idx) | idx == v = True
    go _ = False

isConstant e = everything (&&) (True `mkQ` go) e
  where
    go (EParam _ _) = False
    go (EOverride _ _) = False
    go _ = True

hasNoVars e = everything (&&) (True `mkQ` go) e
  where
    go (EParam _ _) = False
    go _ = True

findVars e = everything (<>) (Set.empty `mkQ` go) e
  where
    go (EParam _ v) = Set.singleton v
    go _ = Set.empty

findOverrides e = everything (<>) (Set.empty `mkQ` go) e
  where
    go (EOverride i _) = Set.singleton i
    go _ = Set.empty


mkParam i = EParam dummySpan i
mkAdd l r = EBinArith dummySpan BAdd l r
mkSub l r = EBinArith dummySpan BSub l r
mkMul l r = EBinArith dummySpan BMul l r
mkIntLit n = EIntLit dummySpan n
mkEq l r = EBinCmp dummySpan BEq l r
mkClog2 e = EUnArith dummySpan UClog2 e


-- A polynomial is a sum of terms, each multiplied by an integer constant.
-- Empty terms are allowed, to indicate adding a constant into the sum.
type Poly = Map Term Int
-- A term is a product of factors, each raised to an integer power.
type Term = Map Factor Int
-- A factor is any other kind of expression.
type Factor = ConstExpr

constPoly :: Int -> Poly
constPoly n = M.singleton M.empty n

polyNeg :: Poly -> Poly
polyNeg a = fmap negate a

polyAdd :: Poly -> Poly -> Poly
polyAdd a b = M.filter (/= 0) $ M.unionWith (+) a b

polySub :: Poly -> Poly -> Poly
polySub a b = polyAdd a (polyNeg b)

termMul :: Term -> Term -> Term
termMul a b = M.filter (/= 0) $ M.unionWith (+) a b

polyMul :: Poly -> Poly -> Poly
polyMul a b = M.filter (/= 0) $ M.fromListWith (+) $ do
    (at, ac) <- M.toList a
    (bt, bc) <- M.toList b
    return (termMul at bt, ac * bc)



termSingleFactor :: Term -> Maybe Factor
termSingleFactor t
  | M.size t == 1, (f, 1) <- M.findMin t = Just f
  | otherwise = Nothing

polySingleTerm :: Poly -> Maybe Term
polySingleTerm p
  | M.size p == 1, (t, 1) <- M.findMin p = Just t
  | otherwise = Nothing

polySingleFactor :: Poly -> Maybe Factor
polySingleFactor p = polySingleTerm p >>= termSingleFactor

polySingleConstant :: Poly -> Maybe Int
polySingleConstant p
  | M.size p == 1
  , (t, n) <- M.findMin p
  , M.size t == 0 = Just n
  | otherwise = Nothing

mapPolyTerms :: (Term -> Term) -> Poly -> Poly
mapPolyTerms f p = M.filter (/= 0) $ M.mapKeysWith (+) f p

mapTermFactors :: (Factor -> Factor) -> Term -> Term
mapTermFactors f t = M.filter (/= 0) $ M.mapKeysWith (+) f t

_polyTerms = sets mapPolyTerms
_termFactors = sets mapTermFactors

exprToPoly e = go e
  where
    go (EIntLit _ n) = constPoly n
    go (EBinArith _ BAdd l r) = polyAdd (go l) (go r)
    go (EBinArith _ BSub l r) = polySub (go l) (go r)
    go (EBinArith _ BMul l r) = polyMul (go l) (go r)
    go e = M.singleton (M.singleton (unspan e) 1) 1

    -- TODO: copied from Verilog.Match
    unspan e = everywhere (mkT $ \_ -> Span 0 0) e

-- Convert factor + exponent to an expr
factorToExpr f c
  | c == 0 = error "factors with zero exponent should have been filtered out?"
  | c < 0 = error "negative exponents not supported"
  | otherwise = foldl1 mkMul $ replicate c f

-- Convert term + coefficient to an expr
termToExpr t n
  | M.size t == 0 = mkIntLit n
  | n == 1 = factorProd
  | otherwise = mkMul (mkIntLit n) factorProd
  where factorProd = foldl1 mkMul $ map (uncurry factorToExpr) (M.toList t)

-- Convert polynomial to an expr
polyToExpr p
  | M.size p == 0 = mkIntLit 0
  | otherwise = foldl1 mkAdd $ map (uncurry termToExpr) (M.toList p)

simplifyFactor e = go e
  where
    -- TODO: Implement specialized (non-polynomial-based) simplification rules
    -- here.  For example, const eval of `UClog2`, or cancellation on both
    -- sides of a `BEq`.
    --go (EUnArith _ UClog2 e')
    --  | Just n <- polySingleConstant $ simplifyPoly e'
    --  , Just (VInt n') <- evalClog2 (VInt n)
    --  = EIntLit dummySpan n'
    go e = over _subexprs simplify e

simplifyPoly e = over (_polyTerms . _termFactors) simplifyFactor $ exprToPoly e

simplify e = polyToExpr $ simplifyPoly e


-- Inline the `require-big-endian-vectors` implementation of `range-size`,
-- replacing all `ERangeSize` exprs with basic arithmetic.
inlineRangeSizeBE e = everywhere (mkT go) e
  where
    go (ERangeSize sp l r) = EBinArith sp BAdd (EIntLit sp 1) (EBinArith sp BSub l r)
    go e = e


-- Check if a constraint has the form "1 = 1".  Returns `True` if it is
-- trivially true, raises an error if it is trivially false, and return `False`
-- otherwise.
isTriviallyTrue c
  | EBinCmp _ BEq l r <- constraintExpr c, l == r = True
  | Just (VBool b) <- evalConst $ constraintExpr c =
    if b then True else error $ "constraint is trivially false: " ++ show c
  | otherwise = False

substVars :: Seq Int -> ConstExpr -> ConstExpr
substVars varMap e = everywhere (mkT go) e
  where
    go (EParam sp i) = EParam sp $ varMap `S.index` i
    go e = e

substVars' :: (Int -> Maybe ConstExpr) -> ConstExpr -> ConstExpr
substVars' f e = everywhere (mkT go) e
  where
    go (EParam _ i) | Just e <- f i = e
    go e = e

substOverrides :: Seq Int -> ConstExpr -> ConstExpr
substOverrides overrideMap e = everywhere (mkT go) e
  where
    go (EOverride i e) = EOverride (overrideMap `S.index` i) e
    go e = e

unwrapOverrides :: (Int -> Bool) -> ConstExpr -> ConstExpr
unwrapOverrides f e = everywhere (mkT go) e
  where
    go (EOverride i e) | f i = e
    go e = e

-- Eliminate all constraints of the form `var1 = var2`, by substituting `var2`
-- for `var1` in all other constraints.
elimVarVar :: FlatConstraints -> FlatConstraints
elimVarVar fc = runST $ do
    arr <- newArray_ (0, S.length vars - 1) :: ST s (STArray s Int (UF.Point s Int))
    (lo, hi) <- getBounds arr

    forM_ [lo .. hi] $ \idx -> do
        p <- UF.fresh idx
        writeArray arr idx p

    -- Filter out `var1 = var2` constraints, and merge variable equivalence
    -- classes as we go.
    cons <- flip filterM (toList $ fcConstraints fc) $ \c -> case constraintExpr c of
        EBinCmp _ BEq (EParam _ i) (EParam _ j) -> do
            ip <- readArray arr i
            jp <- readArray arr j
            -- Keep the index of the higher-numbered var, which is normally the
            -- one that's closer to the top level of the design.
            UF.union' ip jp (\a b -> return $ max a b)
            return False
        _ -> return True

    -- Build substitution map.  This maps each var's index to its replacement
    -- (which may be itself).
    -- substMap :: Seq Int
    substMap <- liftM S.fromList $ forM [lo .. hi] $ \idx ->
        readArray arr idx >>= UF.descriptor

    return $ fc
        { fcConstraints =
            over (traversed . _constraintExpr) (substVars substMap) (S.fromList cons)
        }

  where
    vars = fcVars fc

-- Eliminate all constraints of the form `var = expr` that match `filt`, by
-- substituting `expr` for `var` in all other constraints.
elimVarExpr :: (ConstExpr -> Bool) -> FlatConstraints -> FlatConstraints
elimVarExpr filt fc = fc { fcConstraints = cons' <> extraCons }
  where
    record :: Int -> ConstExpr -> State (Map Int (Set ConstExpr)) ()
    record v e = modify $ \m ->
        M.alter (\s -> Just $ maybe (Set.singleton e) (Set.insert e) s) v m

    (cons, constMap) = flip runState M.empty $ liftM S.fromList $
        flip filterM (toList $ fcConstraints fc) $ \c -> case constraintExpr c of
            e | not $ filt e -> return True
            EBinCmp _ BEq (EParam _ i) e | hasNoVars e -> record i e >> return False
            EBinCmp _ BEq e (EParam _ i) | hasNoVars e -> record i e >> return False
            _ -> return True

    cons' = fmap (over _constraintExpr (substVars' $ \i ->
        Set.findMin <$> M.lookup i constMap)) cons

    -- If we see both `var = const1` and `var = const2`, we replace `var` with
    -- `const1`, but also emit `const1 = const2` to ensure the output
    -- constraints are equivalent to the input.
    extraCons = flip foldMap constMap $ \exprSet -> S.fromList $
        let (e, es) = Set.deleteFindMin exprSet in
        fmap (mkExtraCon e) (Set.toList es)

    mkExtraCon e e' = Constraint (mkEq e e') (CoText "elimVarConst")

-- Remove overrides that appear in constraints of the form `override = const`.
-- All instances of such overrides are replaced with their inner expressions.
elimOverrideConst :: FlatConstraints -> FlatConstraints
elimOverrideConst fc =
    filterOverrides (\i -> not $ i `Set.member` constantOverrides) $
    fc { fcConstraints = cons' }
  where
    record :: Int -> ConstExpr -> State (Map Int (Set ConstExpr)) ()
    record v e = modify $ \m ->
        M.alter (\s -> Just $ maybe (Set.singleton e) (Set.insert e) s) v m

    constantOverrides = flip foldMap (fcConstraints fc) $ \c -> case constraintExpr c of
        EBinCmp _ BEq (EOverride i _) e | isConstant e -> Set.singleton i
        EBinCmp _ BEq e (EOverride i _) | isConstant e -> Set.singleton i
        _ -> Set.empty

    cons' = fmap (over _constraintExpr (unwrapOverrides $
        flip Set.member constantOverrides)) $ fcConstraints fc


deleteUnusedVars fc = fc { fcVars = vars', fcConstraints = cons' }
  where
    usedVars = foldMap (findVars . constraintExpr) $ fcConstraints fc

    (varMap, vars') = flip runState S.empty $ liftM S.fromList $
        forM [0 .. S.length (fcVars fc) - 1] $ \idx ->
            if Set.member idx usedVars then do
                idx' <- gets S.length
                modify (|> fcVars fc `S.index` idx)
                return idx'
            else
                return $ error "unused var was actually used?"

    cons' = fmap (over _constraintExpr (substVars varMap)) $ fcConstraints fc

filterOverrides :: (Int -> Bool) -> FlatConstraints -> FlatConstraints
filterOverrides f fc = fc { fcOverrides = overrides', fcConstraints = cons' }
  where
    (overrideMap, overrides') = flip runState S.empty $ liftM S.fromList $
        forM [0 .. S.length (fcOverrides fc) - 1] $ \idx ->
            if f idx then do
                idx' <- gets S.length
                modify (|> fcOverrides fc `S.index` idx)
                return idx'
            else
                return $ error "unused override was actually used?"

    cons' = fmap (over _constraintExpr (substOverrides overrideMap)) $ fcConstraints fc


dedupConstraints fc = fc
    { fcConstraints = S.fromList $ M.elems $ M.fromList $
        [(constraintExpr c, c) | c <- toList $ fcConstraints fc]
    }


liftClog2Calls :: FlatConstraints -> FlatConstraints
liftClog2Calls fc = fc
    & _fcVars %~ (<> newVars)
    & _fcConstraints .~ (oldConstraints <> newConstraints)
  where
    args :: Set ConstExpr
    args = everything (<>) (Set.empty `mkQ` go) (fcConstraints fc)
      where
        go (EUnArith _ UClog2 e) = Set.singleton e
        go _ = Set.empty

    varBase = S.length $ fcVars fc

    exprName _ (EParam _ i) = fcVars fc `S.index` i
    exprName _ (EOverride i _) = overrideName $ fcOverrides fc `S.index` i
    exprName idx _ = "expr$" <> T.pack (show idx)

    newVars = S.mapWithIndex (\idx e -> "clog2$" <> exprName idx e)
        (S.fromList $ Set.toList args)
    newConstraints = S.mapWithIndex (\idx e -> Constraint
            (mkEq (mkParam $ varBase + idx) (mkClog2 e))
            (CoText $ newVars `S.index` idx))
        (S.fromList $ Set.toList args)

    argVar :: ConstExpr -> Int
    argVar e = varBase + Set.findIndex e args

    oldConstraints = fmap (everywhere (mkT go)) (fcConstraints fc)
      where
        go (EUnArith sp UClog2 e) = EParam sp $ argVar e
        go e = e

isClog2Constraint (EBinCmp _ BEq (EParam _ _) (EUnArith _ UClog2 _)) = True
isClog2Constraint _ = False


simplifyConstraints :: FlatConstraints -> FlatConstraints
simplifyConstraints fc = fc
    & traceConstraints "orig"
    & deleteUnusedVars
    & _fcConstraints %~ S.filter (not . isTriviallyTrue)
    & _fcConstraints . traversed . _constraintExpr %~ simplify . inlineRangeSizeBE
    & dedupConstraints
    & go (const True)
    & traceConstraints "before clog2 lifting"
    & liftClog2Calls
    & go (not . isClog2Constraint)
    & traceConstraints "before override cleanup"
    & elimOverrideConst
    & traceConstraints "after override cleanup"
    & _fcConstraints %~ S.filter (not . isTriviallyTrue)
    & traceConstraints "before dedup"
    & dedupConstraints
    & traceConstraints "final"
  where
    go varExprFilt fc = fc
        & traceConstraints "begin"
        & elimVarVar
        & traceConstraints "after var-var"
        & elimVarExpr varExprFilt
        & traceConstraints "after var-const"
        & _fcConstraints %~ S.filter (not . isTriviallyTrue)
        & traceConstraints "after trivial"
        & deleteUnusedVars
        & maybeLoop
      where
        maybeLoop fc' =
            if S.length (fcVars fc') < S.length (fcVars fc) then go varExprFilt fc' else fc'


traceConstraints desc fc = flip trace fc $ unlines $
    [" ========== " ++ desc ++ ": " ++ show (S.length $ fcConstraints fc) ++ " constraints =========="]
    ++ ["  " ++ show (S.length $ fcVars fc) ++ " vars"]
    ++ ["  " ++ show (Set.size $ mconcat $ map (findVars . constraintExpr) $ toList $ fcConstraints fc) ++ " vars in use"]
    ++ ["  " ++ show (S.length $ fcOverrides fc) ++ " overrides"]
    ++ toList (flip S.mapWithIndex (fcVars fc) $ \idx n ->
        "var " ++ show idx ++ ": " ++ T.unpack n)
    ++ toList (flip S.mapWithIndex (fcOverrides fc) $ \idx o ->
        "override " ++ show idx ++ ": " ++ T.unpack (overrideName o))
    ++ map (\c -> "constraint: " ++ T.unpack (showPretty $ constraintExpr c)) (toList $ fcConstraints fc)
    ++ [" ========== end =========="]

testSimplify :: ConstExpr -> ConstExpr
testSimplify e =
    let e' = simplify $ inlineRangeSizeBE e in
    trace (unlines $ ["test simplify:", "  old -> " ++ T.unpack (showPretty e), "  new -> " ++ T.unpack (showPretty e')]) e
