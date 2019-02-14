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
    foldMap goLogic (moduleLogics mod)
  where
    mod = d `designMod` modId

    goParam p = App [Atom "declare-const", Atom $ name <> "$" <> paramName p, Atom "Int"]

    goLogic (Logic { logicKind = LkInst inst }) = convInst d name modId inst
    goLogic _ = []

convInst :: Design a -> Text -> Int -> Inst -> [SExpr]
convInst d parentName parentModId inst = paramDecls <> constraints
  where
    paramDecls = convModule d (instModId inst) instFullName
    instFullName = parentName <> "$" <> instName inst
    instMod = d `designMod` instModId inst
    parentMod = d `designMod` parentModId

    parentVar idx = Atom $
        parentName <> "$" <> paramName (moduleParams parentMod `S.index` idx)
    childVar idx = Atom $
        instFullName <> "$" <> paramName (moduleParams instMod `S.index` idx)

    paramConstraint idx = case join $ S.lookup idx (instParams inst) of
        Just expr -> assertEq (childVar idx) (convConstExpr parentVar expr)
        Nothing -> case paramDefault $ instMod `moduleParam` idx of
            Just expr -> assertEq (childVar idx) (convConstExpr childVar expr)
            Nothing -> traceShow ("no default available for", instFullName, idx) $
                assertEq (childVar idx) (childVar idx)

    constraints = map paramConstraint [0 .. S.length (moduleParams instMod) - 1]


convConstExpr :: (Int -> SExpr) -> ConstExpr -> SExpr
convConstExpr convVar e = go e
  where
    go (EIntLit i) = Atom $ T.pack $ show i
    go (EParam idx) = convVar idx
    go (EBinOp CbAdd l r) = call "+" [go l, go r]
    go (EBinOp CbSub l r) = call "-" [go l, go r]
    go (EBinOp CbMul l r) = call "*" [go l, go r]
    go (ELog2 e) = call "clog2" [go e]



mkCase :: [(SExpr, SExpr)] -> SExpr -> SExpr
mkCase [] dfl = dfl
mkCase ((cond, val) : rest) dfl = call "if" [cond, val, mkCase rest dfl]

defineClog2 = call "define-fun" [Atom "clog2", argTys, retTy, body]
  where
    argTys = App [App [Atom "x", Atom "Int"]]
    retTy = Atom "Int"
    body = mkCase cases (intLit maxBits)
    maxBits = 64
    cases = [(call "<=" [Atom "x", intLit (2 ^ i)], intLit i) | i <- [0 .. maxBits - 1]]



genSmt :: Config.SMT -> Design a -> [SExpr]
genSmt cfg d = prefix ++ base ++ suffix
  where
    prefix =
        [ defineClog2
        ]
    base = convInst d "" (error $ "top module has params with no defaults")
        (Inst rootId "root" S.empty)
    suffix =
        [ call "check-sat" []
        , call "get-model" []
        ]

    rootName = Config.smtRootModule cfg
    optRootId = S.findIndexL (\m -> moduleName m == rootName) (designMods d)
    rootId = fromMaybe (error $ "root module not found: " ++ show rootName) optRootId

genSmt' :: Config.SMT -> Design a -> Text
genSmt' cfg d = T.unlines $ map printSExpr $ genSmt cfg d
