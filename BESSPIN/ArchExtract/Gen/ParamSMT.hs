{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Gen.ParamSMT where

import Control.Monad
import Data.Foldable
import Data.Generics
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T

import Debug.Trace

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


convModule :: Design a -> Int -> Text -> [SExpr]
convModule d modId name =
    map goParam (toList $ moduleParams mod) <>
    foldMap goLogic (moduleLogics mod) <>
    map (convConstraint convVar) (toList $ moduleConstraints mod)
  where
    mod = d `designMod` modId

    goParam p = App [Atom "declare-const", Atom $ name <> "$" <> paramName p, Atom "Int"]

    goLogic (Logic { logicKind = LkInst inst }) =
        convModule d (instModId inst) (name <> "$" <> instName inst)
    goLogic _ = []

    convVar is p = Atom $ name <> "$" <> varName d mod is p

varName d m [] p = paramName (m `moduleParam` p)
varName d m (i:is) p =
    let LkInst inst = logicKind $ m `moduleLogic` i in
    instName inst <> "$" <> varName d (d `designMod` instModId inst) is p

convConstExpr :: ([Int] -> Int -> SExpr) -> ConstExpr -> SExpr
convConstExpr convVar e = go e
  where
    go (EIntLit i) = Atom $ T.pack $ show i
    go (EParam idx) = convVar [] idx
    go (EInstParam insts idx) = convVar insts idx
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

convConstraint :: ([Int] -> Int -> SExpr) -> ConstExpr -> SExpr
convConstraint convVar e = call "assert" [convConstExpr convVar e]



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

genSmt :: Config.SMT -> Design a -> [SExpr]
genSmt cfg d = prefix ++ base' ++ suffix
  where
    unsatCore = Config.smtGenUnsatCore cfg

    prefix =
        (if unsatCore then
            [call "set-option" [Atom ":produce-unsat-cores", Atom "true"]]
        else []) ++
        [ defineClog2
        , defineRangeSize
        ]
    base =
        convModule d rootId "root" <>
        map (convConstraint convVar) (toList topCons)
    base' = if unsatCore then zipWith nameAssertion [0..] base else base
    suffix =
        [ call "check-sat" []
        ] ++
        (if unsatCore then [call "get-unsat-core" []] else [call "get-model" []])

    convVar is p = Atom $ "root$" <> varName d (d `designMod` rootId) is p

    topCons = defaultConstraints $ d `designMod` rootId

    rootName = Config.smtRootModule cfg
    optRootId = S.findIndexL (\m -> moduleName m == rootName) (designMods d)
    rootId = fromMaybe (error $ "root module not found: " ++ show rootName) optRootId

genSmt' :: Config.SMT -> Design a -> Text
genSmt' cfg d = T.unlines $ map printSExpr $ genSmt cfg d
