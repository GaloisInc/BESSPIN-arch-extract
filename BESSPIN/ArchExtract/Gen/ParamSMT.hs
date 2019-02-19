{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Gen.ParamSMT where

import Control.Monad
import Data.Foldable
import Data.Generics
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Debug.Trace

import qualified SimpleSMT as SMT

import Language.Clafer hiding (Module)
import Language.Clafer.Common
import Language.Clafer.Front.AbsClafer hiding (Module)
import qualified Language.Clafer.Front.AbsClafer as C
import Language.Clafer.Front.PrintClafer
import qualified Language.Clafer.ClaferArgs as Args

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

convConstraint :: Seq Text -> ConstExpr -> SExpr
convConstraint varNames e = call "assert" [convConstExpr varNames e]



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

defineRangeSize = defineFun "range-size" [("l", tInt), ("r", tInt)] tInt $
    call "+" [Atom "1", call "abs" [call "-" [Atom "l", Atom "r"]]]


nameAssertion i (App [Atom "assert", e]) =
    call "assert" [call "!" [e, Atom ":named", Atom $ "a" <> T.pack (show i)]]
nameAssertion _ e = e

genSmt :: Config.SMT -> Seq Text -> Seq ConstExpr -> [SExpr]
genSmt cfg varNames cons = prefix ++ varDecls ++ conExprs' ++ suffix
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
    conExprs = map (convConstraint varNames) $ toList cons
    conExprs' = if unsatCore then zipWith nameAssertion [0..] conExprs else conExprs
    suffix =
        [ call "check-sat" []
        ] ++
        (if unsatCore then [call "get-unsat-core" []] else [call "get-model" []])

genSmt' :: Config.SMT -> Design a -> Text
genSmt' cfg d = T.unlines $ map printSExpr $ genSmt cfg varNames cons
  where
    rootName = Config.smtRootModule cfg
    optRootId = S.findIndexL (\m -> moduleName m == rootName) (designMods d)
    rootId = fromMaybe (error $ "root module not found: " ++ show rootName) optRootId

    d' = d --addRootDefaultConstraints d rootId
    (varNames, cons) = flattenConstraints d' rootId
    

convExpr :: SExpr -> SMT.SExpr
convExpr (Atom t) = SMT.Atom $ T.unpack t
convExpr (App es) = SMT.List $ map convExpr es

findUnconstrainedParameters :: Config.SMT -> Design a -> IO ()
findUnconstrainedParameters cfg d = do
    logger <- SMT.newLogger 0
    solver <- SMT.newSolver "z3" ["-smt2", "-in"] (Just logger)

    forM_ exprs $ \expr -> SMT.ackCommand solver $ convExpr expr
    SMT.command solver $ convExpr $ call "check-sat" []

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
    rootName = Config.smtRootModule cfg
    optRootId = S.findIndexL (\m -> moduleName m == rootName) (designMods d)
    rootId = fromMaybe (error $ "root module not found: " ++ show rootName) optRootId

    d' = d --addRootDefaultConstraints d rootId
    (varNames, cons) = flattenConstraints d' rootId

    exprs =
        defineClog2 <|
        defineRangeSize <|
        fmap (\v -> call "declare-const" [Atom v, tInt]) varNames <>
        fmap (convConstraint varNames) cons
