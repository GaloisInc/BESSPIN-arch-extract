{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Gen.ParamSMT where

import Control.Monad
import Data.Foldable
import Data.Generics
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UnionFind.IO as UF

import Debug.Trace

import qualified SimpleSMT as SMT

import BESSPIN.ArchExtract.Architecture
import qualified BESSPIN.ArchExtract.Config as Config
import BESSPIN.ArchExtract.Constraints


data SExpr = Atom Text | App [SExpr]
    deriving (Show)

printSExpr :: SExpr -> Text
printSExpr (Atom t) = t
printSExpr (App ts) = "(" <> T.unwords (map printSExpr ts) <> ")"

call name args = App (Atom name : args)
assertEq a b = call "assert" [call "=" [a, b]]

intLit :: (Show a, Integral a) => a -> SExpr
intLit i = Atom $ T.pack $ show i


overrideValName (n, _) = "over-val-" <> n
overrideEnableName (n, _) = "over-en-" <> n

convConstExpr :: FlatConstraints -> ConstExpr -> SExpr
convConstExpr fc e = go e
  where
    go (EIntLit _ i) = intLit i
    go (EParam _ idx) = Atom $ fcVars fc `S.index` idx
    go (EInstParam _ insts idx) = error $ "unexpected EInstParam in flattened constraint"
    go (EUnArith _ UClog2 e) = call "clog2" [go e]
    go (EUnArith _ UIsPow2 e) = call "is-pow2" [go e]
    go (EBinArith _ BAdd l r) = call "+" [go l, go r]
    go (EBinArith _ BSub l r) = call "-" [go l, go r]
    go (EBinArith _ BMul l r) = call "*" [go l, go r]
    go (EBinCmp _ BEq l r) = call "=" [go l, go r]
    go (EBinCmp _ BNe l r) = call "not" [call "=" [go l, go r]]
    go (EBinCmp _ BLt l r) = call "<" [go l, go r]
    go (EBinCmp _ BLe l r) = call "<=" [go l, go r]
    go (EBinCmp _ BGt l r) = call ">" [go l, go r]
    go (EBinCmp _ BGe l r) = call ">=" [go l, go r]
    go (ERangeSize _ l r) = call "range-size" [go l, go r]
    go (EOverride idx e) = call "ite"
        [ Atom $ overrideEnableName $ fcOverrides fc `S.index` idx
        , Atom $ overrideValName $ fcOverrides fc `S.index` idx
        , go e ]
    go (EOverrideLocalParam _ _) = error "unexpected OverrideLocalParam in flattened constraint"
    go (EOverrideInstParam _ _ _) = error "unexpected OverrideInstParam in flattened constraint"

convConstraint :: Bool -> FlatConstraints -> Int -> Constraint -> SExpr
convConstraint False fc _ (Constraint e _) =
    call "assert" [convConstExpr fc e]
convConstraint True fc i (Constraint e o) =
    let CoText name = o in
    call "assert" [call "!" [convConstExpr fc e, Atom ":named",
        Atom $ name <> "-" <> T.pack (show i)]]



mkCase :: [(SExpr, SExpr)] -> SExpr -> SExpr
mkCase [] dfl = dfl
mkCase ((cond, val) : rest) dfl = call "if" [cond, val, mkCase rest dfl]

defineFun name args retTy body = call "define-fun"
    [ Atom name
    , App [App [Atom name, ty] | (name, ty) <- args]
    , retTy
    , body]
funArg name ty = App [Atom name, ty]
tInt = Atom "Int"
tBool = Atom "Bool"

defineClog2 = defineFun "clog2" [("x", tInt)] tInt body
  where
    body = mkCase cases (intLit maxBits)
    maxBits = 64
    cases = [(call "<=" [Atom "x", intLit (2 ^ i)], intLit i) | i <- [0 .. maxBits - 1]]

defineIsPow2 = defineFun "is-pow2" [("x", tInt)] tBool body
  where
    body = call "or" [call "=" [Atom "x", intLit $ 2 ^ i] | i <- [0 .. maxBits - 1]]
    maxBits = 64

--defineRangeSize = defineFun "range-size" [("l", tInt), ("r", tInt)] tInt $
--    call "+" [Atom "1", call "abs" [call "-" [Atom "l", Atom "r"]]]
defineRangeSize = defineFun "range-size" [("l", tInt), ("r", tInt)] tInt $
    call "+" [Atom "1", call "-" [Atom "l", Atom "r"]]


flattenConstraintsForDesign :: Config.SMT -> Design a -> FlatConstraints
flattenConstraintsForDesign cfg d = simplifyConstraints $ flattenConstraints d rootId
  where
    rootName = Config.smtRootModule cfg
    optRootId = S.findIndexL (\m -> moduleName m == rootName) (designMods d)
    rootId = fromMaybe (error $ "root module not found: " ++ show rootName) optRootId


genSmt :: Config.SMT -> FlatConstraints -> [SExpr]
genSmt cfg fc = cmds'
  where
    cmds = toList $ initCommands' unsatCore fc
    cmds' =
        (if unsatCore then
            [call "set-option" [Atom ":produce-unsat-cores", Atom "true"]]
        else [])
        <> cmds
        <> [call "check-sat" []]
        <> [if unsatCore then call "get-unsat-core" [] else call "get-model" []]

    unsatCore = Config.smtGenUnsatCore cfg

genSmt' :: Config.SMT -> Design a -> Text
genSmt' cfg d = T.unlines $ map printSExpr $ genSmt cfg fc
  where
    fc = flattenConstraintsForDesign cfg d


initCommands :: FlatConstraints -> Seq SExpr
initCommands fc = initCommands' False fc

initCommands' :: Bool -> FlatConstraints -> Seq SExpr
initCommands' unsatCore fc =
    defineClog2 <|
    defineIsPow2 <|
    defineRangeSize <|
    fmap (\v -> call "declare-const" [Atom v, tInt]) (fcVars fc) <>
    foldMap (\o -> S.fromList
        [ call "declare-const" [Atom $ overrideValName o, tInt]
        , call "declare-const" [Atom $ overrideEnableName  o, tBool]
        ]) (fcOverrides fc) <>
    S.mapWithIndex (convConstraint unsatCore fc) (fcConstraints fc)

convExpr :: SExpr -> SMT.SExpr
convExpr (Atom t) = SMT.Atom $ T.unpack t
convExpr (App es) = SMT.List $ map convExpr es

initSolver :: FlatConstraints -> IO SMT.Solver
initSolver fc = do
    logger <- SMT.newLogger 0
    solver <- SMT.newSolver "z3" ["-smt2", "-in"] Nothing

    forM_ (initCommands fc) $ \expr -> SMT.ackCommand solver $ convExpr expr
    r <- SMT.check solver
    guard $ r == SMT.Sat

    return solver

{-
-- Build a set of related-variable tests to perform.  Two variables might be
-- related if they appear in the same constraint.  The output will contain a
-- test between each pair of potentially-related variables.  (The relatedness
-- test is assumed to be symmetric, so only one of `(i,j)` and `(j,i)` can
-- appear in the output.)
relatedVarTests :: Seq Constraint -> Set (Int, Int)
relatedVarTests cons = Set.unions $ map relSetPairs $ toList relSets
  where
    relSets :: Seq (Set Int)
    relSets = fmap varSet cons

    relSetPairs :: Set Int -> Set (Int, Int)
    relSetPairs rs = Set.filter (\(a, b) -> a < b) $ Set.cartesianProduct rs rs

    varSet c = everything (<>) (Set.empty `mkQ` grabVar) (constraintExpr c)
    grabVar (EParam _ i) = Set.singleton i
    grabVar _ = Set.empty

-- Check if two variables are related.  `a` and `b` are related if changing `a`
-- forces `b` to change or vice versa.  (This definition handles cases such as
-- `a = clog2(b)`, where changing `b` from 5 to 6 doesn't change `a`.)
--
-- This function assumes `solver` has already been provided with the model's
-- base constraints.
checkRelated :: SMT.Solver -> Text -> SMT.Value -> Text -> SMT.Value -> IO Bool
checkRelated solver name1 val1 name2 val2 = do
    let name1' = convExpr $ Atom name1
    let name2' = convExpr $ Atom name2

    -- If var1 changes, can var2 stay the same?
    SMT.push solver
    SMT.assert solver $ SMT.not $ SMT.eq name1' (SMT.value val1)
    SMT.assert solver $ SMT.eq name2' (SMT.value val2)
    -- Unsat means no, changing var1 forces var2 to change.
    r1 <- (== SMT.Unsat) <$> SMT.check solver
    SMT.pop solver

    -- If var2 changes, can var1 stay the same?
    SMT.push solver
    SMT.assert solver $ SMT.eq name1' (SMT.value val1)
    SMT.assert solver $ SMT.not $ SMT.eq name2' (SMT.value val2)
    r2 <- (== SMT.Unsat) <$> SMT.check solver
    SMT.pop solver

    return $ r1 || r2
-}

-- Count the number of different values `e` can take on, up to a limit of `n`.
countValues :: SMT.Solver -> SMT.SExpr -> Int -> IO Int
countValues _ _ n | n <= 0 = return 0
countValues solver e n = do
    r <- SMT.check solver
    case r of
        SMT.Sat -> do
            val <- SMT.getExpr solver e
            SMT.assert solver $ SMT.not $ SMT.eq e (SMT.value val)
            (+ 1) <$> countValues solver e (n - 1)
        SMT.Unsat -> return 0

-- Check if an override is fixed, meaning it can't take on more than one value.
checkFixed :: SMT.Solver -> FlatConstraints -> Int -> IO Bool
checkFixed solver fc idx = do
    let ov = fcOverrides fc `S.index` idx
    SMT.push solver
    SMT.assert solver $ SMT.Atom $ T.unpack $ overrideEnableName ov
    count <- countValues solver (SMT.Atom $ T.unpack $ overrideValName ov) 2
    SMT.pop solver
    return $ count <= 1

groupParameters :: Config.SMT -> Design a -> IO ()
groupParameters cfg d = do
    solver <- initSolver fc

    let ovIdxs = [0 .. S.length (fcOverrides fc) - 1]
    fixedIdxs <- filterM (checkFixed solver fc) ovIdxs

    print (length ovIdxs, "total idxs")
    print (length fixedIdxs, "fixed idxs")

    let goodIdxs = Set.toList $ Set.fromList ovIdxs Set.\\ Set.fromList fixedIdxs
    forM_ goodIdxs $ \i -> print $ fcOverrides fc `S.index` i

{-
    rawModel <- SMT.getExprs solver $ map (SMT.Atom . T.unpack) $ toList varNames
    let model = S.fromList $ map snd rawModel

    fixedVars <- liftM Set.fromList $ flip filterM [0 .. S.length varNames - 1] $ \i -> do
        let name = varNames `S.index` i
        let val = model `S.index` i
        checkFixed solver name val

    pts <- S.fromList <$> mapM UF.fresh [0 .. S.length varNames - 1]

    let relTests' = filter (\(a,b) ->
            not $ Set.member a fixedVars || Set.member b fixedVars) relTests

    forM_ relTests' $ \(a, b) -> do
        equiv <- UF.equivalent (pts `S.index` a) (pts `S.index` b)
        when (not equiv) $ do
            let aName = varNames `S.index` a
            let aVal = model `S.index` a
            let bName = varNames `S.index` b
            let bVal = model `S.index` b
            rel <- checkRelated solver aName aVal bName bVal
            when rel $ do
                print ("relate", a, aName, b, bName)
                UF.union' (pts `S.index` a) (pts `S.index` b) (\a b -> return $ min a b)

    putStrLn "--- distinct parameters: ---"

    uniquePts <- liftM mconcat $ liftM toList $ forM pts $ \p -> do
        r <- UF.redundant p
        idx <- UF.descriptor p
        if not r && not (Set.member idx fixedVars) then do
            print $ varNames `S.index` idx
            return [idx]
        else return []

    print uniquePts
    print (length uniquePts, "distinct parameters")
    print (Set.size fixedVars, "fixed parameters")
-}

  where
    fc = flattenConstraintsForDesign cfg d
    --varNames = fcVars fc
    --relTests = Set.toList $ relatedVarTests $ fcConstraints fc



