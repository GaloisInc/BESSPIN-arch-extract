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
import BESSPIN.ArchExtract.Eval


flattenConstraintsForDesign :: Config.ParamClafer -> Design a -> FlatConstraints
flattenConstraintsForDesign cfg d = simplifyConstraints $ flattenConstraints d rootId
  where
    rootName = Config.paramClaferRootModule cfg
    optRootId = S.findIndexL (\m -> moduleName m == rootName) (designMods d)
    rootId = fromMaybe (error $ "root module not found: " ++ show rootName) optRootId

cleanIdent prefix t = prefix <> (T.pack $ foldr go [] $ T.unpack t)
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

genVarDecl :: Text -> Text -> C.Element
genVarDecl prefix name = Subclafer noSpan $ Clafer noSpan
    (AbstractEmpty noSpan)
    []
    (GCardEmpty noSpan)
    (mkIdent $ T.unpack $ cleanIdent prefix name)
    (SuperEmpty noSpan)
    (ReferenceSet noSpan $ mkPath ["integer"])
    (CardEmpty noSpan)
    (InitEmpty noSpan)
    (TransitionEmpty noSpan)
    (ElementsEmpty noSpan)

genConstraint :: Ctx -> ConstExpr -> [C.Element]
genConstraint ctx c
  | Just e <- convConstraintExpr ctx c =
    [Subconstraint noSpan $ C.Constraint noSpan [cleanupRedundantEqs e]]
  | otherwise = []


data Ctx = Ctx
    { ctxVarNames :: Seq Text
    , ctxOverrideNames :: Seq Text
    }

paramPrefix = "p_"
overridePrefix = "f_"

buildClog2Constraint bitWidth e e' =
    foldr (\(cond, val) rest -> EImpliesElse noSpan cond (EEq noSpan e val) rest) false checks
  where
    -- If the bit width is 4, then only integers in the range -8 .. 7 are
    -- valid.  -8..1 (inclusive) map to 0, 2 -> 1, 3..4 -> 2, and 5..7 -> 3.
    -- Note that the last case has a truncated input range due to the bit width
    -- limit.
    checks =
        [(ELte noSpan e' (mkIntLit $ 2^n), mkIntLit n)  | n <- [0 .. bitWidth - 2]] ++
        [(ELte noSpan e' (mkIntLit $ 2^n - 1), mkIntLit n) | n <- [bitWidth - 1]]

    -- Clafer has no boolean literals, so we write `0 != 0` to mean false.
    false = ENeq noSpan (mkIntLit 0) (mkIntLit 0)

buildIsPow2Constraint bitWidth e =
    -- If the bit width is 4, then only integers in the range -8 .. 7 are
    -- valid.  The supported powers of two are 1, 2, and 4.
    foldl1 (EOr noSpan) [EEq noSpan e (mkIntLit $ 2^n) | n <- [0 .. bitWidth - 2]]

-- TODO: make bit width configurable
bitWidth = 9

convConstraintExpr :: Ctx -> ConstExpr -> Maybe Exp
convConstraintExpr ctx (EBinCmp _ BEq e (EUnArith _ UClog2 e')) =
    buildClog2Constraint bitWidth <$> convExpr ctx e <*> convExpr ctx e'
convConstraintExpr ctx (EUnArith _ UIsPow2 e) =
    buildIsPow2Constraint bitWidth <$> convExpr ctx e
convConstraintExpr ctx e = convExpr ctx e

convExpr :: Ctx -> ConstExpr -> Maybe Exp
convExpr ctx e = go e
  where
    -- TODO: warn when dropping a constraint due to unsupported expressions
    go (EIntLit _ i) = Just $ mkIntLit i
    go (EParam _ idx) = Just $
        mkPath [cleanIdent paramPrefix $ ctxVarNames ctx `S.index` idx]
    go (EInstParam _ [] idx) = Just $
        mkPath [cleanIdent paramPrefix $ ctxVarNames ctx `S.index` idx]
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
    go (EOverride i _) = Just $
        mkPath [cleanIdent overridePrefix $ ctxOverrideNames ctx `S.index` i]
    go (EOverrideLocalParam _ e) = go e
    go (EOverrideInstParam _ _ e) = go e

    -- Clafer doesn't parse `a - b` correctly, so we generate `a + -b` instead.
    mkSub sp a b = EAdd sp a $ EMinExp sp b

cleanupRedundantEqs :: Exp -> Exp
cleanupRedundantEqs e = everywhere (mkT go) e
  where
    go (EEq _ l r) | l `syntaxEq` r =
        -- Always true.  Replace with `1 < 2`, which clafer and alloy won't
        -- complain about.
        ELt noSpan (mkIntLit 1) (mkIntLit 2)
    go (ENeq _ l r) | l `syntaxEq` r =
        -- Always false.  Replace with `2 < 1`.
        ELt noSpan (mkIntLit 2) (mkIntLit 1)
    go e = e

    syntaxEq a b = unspan a == unspan b
    unspan e = everywhere (mkT go) e
      where go (C.Span _ _) = noSpan


wrapElements :: Text -> [C.Element] -> C.Element
wrapElements name elts = Subclafer noSpan $ Clafer noSpan
    (AbstractEmpty noSpan)
    []
    (GCardEmpty noSpan)
    (mkIdent $ T.unpack name)
    (SuperEmpty noSpan)
    (ReferenceEmpty noSpan)
    (CardEmpty noSpan)
    (InitEmpty noSpan)
    (TransitionEmpty noSpan)
    (ElementsList noSpan elts)

genParamClafer :: Config.ParamClafer -> FlatConstraints -> C.Module
genParamClafer cfg fc = C.Module noSpan [ElementDecl noSpan $ wrapElements "device" elts]
  where
    elts =
        (toList $ fmap (genVarDecl paramPrefix) $ fcVars fc)
        ++ (toList $ fmap (genVarDecl overridePrefix . overrideName) $ fcOverrides fc)
        ++ (foldMap (genConstraint ctx . constraintExpr) $ fcConstraints fc)
    ctx = Ctx (fcVars fc) (fmap overrideName $ fcOverrides fc)

genParamClafer' :: Config.ParamClafer -> Design a -> C.Module
genParamClafer' cfg d = genParamClafer cfg $ flattenConstraintsForDesign cfg d
