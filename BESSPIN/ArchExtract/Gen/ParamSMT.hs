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


convConstExpr :: Seq Text -> ConstExpr -> SExpr
convConstExpr varNames e = go e
  where
    go (EIntLit i) = intLit i
    go (EParam idx) = Atom $ varNames `S.index` idx
    go (EInstParam insts idx) = error $ "unexpected EInstParam in flattened constraint"
    go (EUnArith UClog2 e) = call "clog2" [go e]
    go (EBinArith BAdd l r) = call "+" [go l, go r]
    go (EBinArith BSub l r) = call "-" [go l, go r]
    go (EBinArith BMul l r) = call "*" [go l, go r]
    go (EBinCmp BEq l r) = call "=" [go l, go r]
    go (EBinCmp BNe l r) = call "not" [call "=" [go l, go r]]
    go (EBinCmp BLt l r) = call "<" [go l, go r]
    go (EBinCmp BLe l r) = call "<=" [go l, go r]
    go (EBinCmp BGt l r) = call ">" [go l, go r]
    go (EBinCmp BGe l r) = call ">=" [go l, go r]
    go (ERangeSize l r) = call "range-size" [go l, go r]

convConstraint :: Bool -> Seq Text -> Int -> Constraint -> SExpr
convConstraint False varNames _ (Constraint e _) =
    call "assert" [convConstExpr varNames e]
convConstraint True varNames i (Constraint e o) =
    let CoText name = o in
    call "assert" [call "!" [convConstExpr varNames e, Atom ":named",
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

defineClog2 = defineFun "clog2" [("x", tInt)] tInt body
  where
    body = mkCase cases (intLit maxBits)
    maxBits = 64
    cases = [(call "<=" [Atom "x", intLit (2 ^ i)], intLit i) | i <- [0 .. maxBits - 1]]

--defineRangeSize = defineFun "range-size" [("l", tInt), ("r", tInt)] tInt $
--    call "+" [Atom "1", call "abs" [call "-" [Atom "l", Atom "r"]]]
defineRangeSize = defineFun "range-size" [("l", tInt), ("r", tInt)] tInt $
    call "+" [Atom "1", call "-" [Atom "l", Atom "r"]]


flattenConstraintsForDesign :: Config.SMT -> Design a -> (Seq Text, Seq Constraint)
flattenConstraintsForDesign cfg d = (varNames, cons)
  where
    rootName = Config.smtRootModule cfg
    optRootId = S.findIndexL (\m -> moduleName m == rootName) (designMods d)
    rootId = fromMaybe (error $ "root module not found: " ++ show rootName) optRootId

    (varNames, cons) = flattenConstraints d rootId


genSmt :: Config.SMT -> Seq Text -> Seq Constraint -> [SExpr]
genSmt cfg varNames cons = prefix ++ varDecls ++ conExprs ++ suffix
  where
    unsatCore = Config.smtGenUnsatCore cfg

    prefix =
        (if unsatCore then
            [call "set-option" [Atom ":produce-unsat-cores", Atom "true"]]
        else []) ++
        [ defineClog2
        , defineRangeSize
        ]
    varDecls = map (\v -> call "declare-const" [Atom v, tInt]) $ toList varNames
    conExprs = zipWith (convConstraint unsatCore varNames) [0..] $ toList cons
    suffix =
        [ call "check-sat" []
        ] ++
        (if unsatCore then [call "get-unsat-core" []] else [call "get-model" []])

genSmt' :: Config.SMT -> Design a -> Text
genSmt' cfg d = T.unlines $ map printSExpr $ genSmt cfg varNames cons
  where
    (varNames, cons) = flattenConstraintsForDesign cfg d


convExpr :: SExpr -> SMT.SExpr
convExpr (Atom t) = SMT.Atom $ T.unpack t
convExpr (App es) = SMT.List $ map convExpr es

initSolver :: Seq Text -> Seq Constraint -> IO SMT.Solver
initSolver varNames cons = do
    logger <- SMT.newLogger 0
    solver <- SMT.newSolver "z3" ["-smt2", "-in"] (Just logger)

    forM_ exprs $ \expr -> SMT.ackCommand solver $ convExpr expr
    r <- SMT.check solver
    guard $ r == SMT.Sat

    return solver
  where
    exprs =
        defineClog2 <|
        defineRangeSize <|
        fmap (\v -> call "declare-const" [Atom v, tInt]) varNames <>
        fmap (convConstraint False varNames 0) cons

findUnconstrainedParameters :: Config.SMT -> Design a -> IO ()
findUnconstrainedParameters cfg d = do
    solver <- initSolver varNames cons

    initVals <- SMT.getExprs solver (map (SMT.Atom . T.unpack) $ toList varNames)

    unconParams <- liftM mconcat $ forM initVals $ \(name, val) -> do
        SMT.push solver
        SMT.assert solver $ SMT.not $ name `SMT.eq` SMT.value val
        r <- SMT.check solver
        SMT.pop solver
        return $ case r of
            SMT.Sat -> [name]
            _ -> []

    putStrLn "--- unconstrained parameters: ---"
    mapM_ print unconParams

    putStrLn "--- constrained parameters: ---"
    mapM_ print $ filter (\(n,v) -> not $ Set.member n $ Set.fromList unconParams) initVals

    print (length unconParams, "unconstrained", S.length varNames, "total")

  where
    (varNames, cons) = flattenConstraintsForDesign cfg d


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
    grabVar (EParam i) = Set.singleton i
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

-- Check if a variable is fixed, meaning it cannot take on any other value than
-- the one in the current model.
checkFixed :: SMT.Solver -> Text -> SMT.Value -> IO Bool
checkFixed solver name val = do
    let name' = convExpr $ Atom name
    SMT.push solver
    SMT.assert solver $ SMT.not $ SMT.eq name' (SMT.value val)
    r <- (== SMT.Unsat) <$> SMT.check solver
    SMT.pop solver
    return r

groupParameters :: Config.SMT -> Design a -> IO ()
groupParameters cfg d = do
    solver <- initSolver varNames cons

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

  where
    (varNames, cons) = flattenConstraintsForDesign cfg d
    relTests = Set.toList $ relatedVarTests cons



