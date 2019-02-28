{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Gen.ParamClafer where

import Control.Monad
import Control.Monad.Writer
import Data.Char
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

import BESSPIN.ArchExtract.Architecture hiding (Span)
import qualified BESSPIN.ArchExtract.Config as Config
import BESSPIN.ArchExtract.Constraints


flattenConstraintsForDesign :: Config.ParamClafer -> Design a -> FlatConstraints
flattenConstraintsForDesign cfg d = simplifyConstraints $ flattenConstraints d rootId
  where
    rootName = Config.paramClaferRootModule cfg
    optRootId = S.findIndexL (\m -> moduleName m == rootName) (designMods d)
    rootId = fromMaybe (error $ "root module not found: " ++ show rootName) optRootId

cleanIdent t = T.pack $ "v_" ++ foldr go [] (T.unpack t)
  where
    go c s | isAlphaNum c = c : s
    go _ s = '_' : s

mkPath :: [Text] -> Exp
mkPath (name : names) = foldl (\e n -> EJoin noSpan e (go n)) (go name) names
  where
    go :: Text -> Exp
    go n = ClaferId noSpan $ Path noSpan [ModIdIdent noSpan $ mkIdent $ T.unpack n]

mkIntLit :: Int -> Exp
mkIntLit i = EInt noSpan $ PosInteger ((0, 0), show i)

genVarDecl :: Text -> C.Element
genVarDecl name = Subclafer noSpan $ Clafer noSpan
    (AbstractEmpty noSpan)
    []
    (GCardEmpty noSpan)
    (mkIdent $ T.unpack $ cleanIdent name)
    (SuperEmpty noSpan)
    (ReferenceSet noSpan $ mkPath ["integer"])
    (CardEmpty noSpan)
    (InitEmpty noSpan)
    (TransitionEmpty noSpan)
    (ElementsEmpty noSpan)

genConstraint :: Seq Text -> ConstExpr -> [C.Element]
genConstraint varNames c
  | Just e <- convExpr varNames c =
    [Subconstraint noSpan $ C.Constraint noSpan [e]]
  | otherwise = []


convExpr :: Seq Text -> ConstExpr -> Maybe Exp
convExpr varNames e = go e
  where
    -- TODO: warn when dropping a constraint due to unsupported expressions
    go (EIntLit _ i) = Just $ mkIntLit i
    go (EParam _ idx) = Just $ mkPath [cleanIdent $ varNames `S.index` idx]
    go (EInstParam _ [] idx) = Just $ mkPath [cleanIdent $ varNames `S.index` idx]
    go (EInstParam _ _ _) = Nothing
    go (EUnArith _ UClog2 _) = Nothing
    go (EUnArith _ UIsPow2 _) = Nothing
    go (EBinArith _ BAdd l r) = EAdd noSpan <$> go l <*> go r
    go (EBinArith _ BSub l r) = mkSub noSpan <$> go l <*> go r
    go (EBinArith _ BMul l r) = EMul noSpan <$> go l <*> go r
    go (EBinCmp _ BEq l r) = EEq noSpan <$> go l <*> go r
    go (EBinCmp _ BNe l r) = ENeq noSpan <$> go l <*> go r
    go (EBinCmp _ BLt l r) = ELt noSpan <$> go l <*> go r
    go (EBinCmp _ BLe l r) = ELte noSpan <$> go l <*> go r
    go (EBinCmp _ BGt l r) = EGt noSpan <$> go l <*> go r
    go (EBinCmp _ BGe l r) = EGte noSpan <$> go l <*> go r
    go (ERangeSize _ l r) =
        EAdd noSpan (mkIntLit 1) <$> (mkSub noSpan <$> go l <*> go r)
    go (EOverride _ e) = go e
    go (EOverrideLocalParam _ e) = go e
    go (EOverrideInstParam _ _ e) = go e

    -- Clafer doesn't parse `a - b` correctly, so we generate `a + -b` instead.
    mkSub sp a b = EAdd sp a $ EMinExp sp b

genParamClafer :: Config.ParamClafer -> FlatConstraints -> C.Module
genParamClafer cfg fc = C.Module noSpan $ map (ElementDecl noSpan) elts
  where
    elts =
        (toList $ fmap genVarDecl $ fcVars fc)
        ++ (foldMap (genConstraint (fcVars fc) . constraintExpr) $ fcConstraints fc)

genParamClafer' :: Config.ParamClafer -> Design a -> C.Module
genParamClafer' cfg d = genParamClafer cfg $ flattenConstraintsForDesign cfg d
