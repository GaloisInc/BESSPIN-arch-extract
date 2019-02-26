{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Constraints.Resolve where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as P
import Data.Char
import Data.Generics
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Lens.Micro.Platform

import BESSPIN.ArchExtract.Architecture (dummySpan)
import qualified BESSPIN.ArchExtract.Architecture as A
import qualified BESSPIN.ArchExtract.Constraints.Parser as P


-- TODO: duplicated from Constraints
shiftExpr idx e = everywhere (mkT go) e
  where
    go (A.EParam sp p) = A.EInstParam sp [idx] p
    go (A.EInstParam sp is p) = A.EInstParam sp (idx : is) p
    go e = e

-- TODO: duplicated from Verilog.TypeCheck
tyWidth :: A.Ty -> Either Text A.ConstExpr
tyWidth (A.TWire [] []) = Right $ A.EIntLit dummySpan 1
tyWidth (A.TWire ws []) = Right $ foldl1 (A.EBinArith dummySpan A.BMul) ws
tyWidth t = Left $ "can't compute size of net of type " <> T.pack (show t)


data ItemNames = ItemNames
    { inParams :: Map Text Int
    , inInsts :: Map Text Int
    , inNets :: Map Text Int
    }

data Context a = Context
    { cxNames :: Seq ItemNames
    , cxDesign :: A.Design a
    , cxCurMod :: Int
    }

getParam :: Context a -> Text -> Either Text Int
getParam cx name = case M.lookup name $ inParams $ cxNames cx `S.index` cxCurMod cx of
    Nothing -> Left $ "no parameter " <> name <> " in module " <> A.moduleName m
    Just idx -> Right idx
  where
    m = cxDesign cx `A.designMod` cxCurMod cx

getNet :: Context a -> Text -> Either Text (A.Net a)
getNet cx name = case M.lookup name $ inNets $ cxNames cx `S.index` cxCurMod cx of
    Nothing -> Left $ "no net " <> name <> " in module " <> A.moduleName m
    Just idx -> Right $ m `A.moduleNet` A.NetId idx
  where
    m = cxDesign cx `A.designMod` cxCurMod cx

enterInst :: Context a -> Text -> Either Text (Int, Context a)
enterInst cx name = case M.lookup name $ inInsts $ cxNames cx `S.index` cxCurMod cx of
    Nothing -> Left $ "no instance " <> name <> " in module " <> A.moduleName m
    Just idx ->
        let A.LkInst inst = A.logicKind $ m `A.moduleLogic` idx in
        Right (idx, cx { cxCurMod = A.instModId inst })
  where
    m = cxDesign cx `A.designMod` cxCurMod cx


resolveParam :: Context a -> P.Dotted -> Either Text A.ConstExpr
resolveParam cx (P.Dotted insts param) = go cx insts param
  where
    go cx [] param = A.EParam dummySpan <$> getParam cx param
    go cx (inst : insts) param = do
        (idx, cx') <- enterInst cx inst
        shiftExpr idx <$> go cx' insts param

resolveNetSize :: Context a -> P.Dotted -> Either Text A.ConstExpr
resolveNetSize cx (P.Dotted insts param) = go cx insts param
  where
    go cx [] net = (A.netTy <$> getNet cx net) >>= tyWidth
    go cx (inst : insts) net = do
        (idx, cx') <- enterInst cx inst
        shiftExpr idx <$> go cx' insts net

resolveParam' cx d = case resolveParam cx d of
    Left e -> error $ "error resolving " <> show d <> ": " <> T.unpack e
    Right x -> x

resolveNetSize' cx d = case resolveNetSize cx d of
    Left e -> error $ "error resolving " <> show d <> ": " <> T.unpack e
    Right x -> x

resolve :: Context a -> P.Expr -> A.ConstExpr
resolve cx e = go e
  where
    go (P.EIntLit i) = A.EIntLit dummySpan i
    go (P.EDotted d) = resolveParam' cx d
    go (P.EUnArith op e) = A.EUnArith dummySpan op (go e)
    go (P.EBinArith op l r) = A.EBinArith dummySpan op (go l) (go r)
    go (P.EBinCmp op l r) = A.EBinCmp dummySpan op (go l) (go r)
    go (P.ERangeSize l r) = A.ERangeSize dummySpan (go l) (go r)
    go (P.ESize d) = resolveNetSize' cx d

mkContext :: A.Design a -> Context a
mkContext d = Context names d (error "current module not yet initialized")
  where
    names = fmap modNames $ A.designMods d
    modNames m = ItemNames
        (S.foldMapWithIndex (\idx p -> M.singleton (A.paramName p) idx) $
            A.moduleParams m)
        (S.foldMapWithIndex (\idx l -> case A.logicKind l of
            A.LkInst inst -> M.singleton (A.instName inst) idx
            _ -> M.empty) $ A.moduleLogics m)
        (S.foldMapWithIndex (\idx n -> M.singleton (A.netName n) idx) $
            A.moduleNets m)

withCurModule :: Int -> Context a -> Context a
withCurModule modId cx = 
    if modId < 0 || modId >= S.length (A.designMods $ cxDesign cx) then
        error "modId out of range"
    else
        cx { cxCurMod = modId }
