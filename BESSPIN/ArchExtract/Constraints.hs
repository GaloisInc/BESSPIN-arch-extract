{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Constraints where

import Control.Monad
import Data.Foldable
import Data.Generics
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro

import Debug.Trace

import Language.Clafer hiding (Module)
import Language.Clafer.Common
import Language.Clafer.Front.AbsClafer hiding (Module)
import qualified Language.Clafer.Front.AbsClafer as C
import Language.Clafer.Front.PrintClafer
import qualified Language.Clafer.ClaferArgs as Args

import BESSPIN.ArchExtract.Architecture
import qualified BESSPIN.ArchExtract.Config as Config


-- Shift all `EParam` and `EInstParam` by prepending `idx`.  This converts
-- `x.y` into `i.x.y`, where `i` is the instance at index `idx`.
shiftExpr idx e = everywhere (mkT go) e
  where
    go (EParam p) = EInstParam [idx] p
    go (EInstParam is p) = EInstParam (idx : is) p
    go e = e


instParamConstraints d m = flip S.foldMapWithIndex (moduleLogics m) $ \idx logic ->
    case logicKind logic of
        LkInst inst -> go idx inst
        _ -> S.empty
  where
    go idx inst =
        let formals = (moduleParams $ d `designMod` instModId inst) in
        flip S.foldMapWithIndex formals $ \paramIdx formal ->
            let actualExpr = join $ instParams inst S.!? paramIdx in
            let formalExpr = paramDefault formal in
            case paramVal idx actualExpr formalExpr of
                Nothing -> S.empty
                Just c -> S.singleton $ EBinCmp BEq (EInstParam [idx] paramIdx) c

    paramVal instIdx (Just e) _ = Just e
    paramVal instIdx Nothing (Just e) = Just $ shiftExpr instIdx e
    paramVal instIdx Nothing Nothing = Nothing

defaultConstraints m = flip S.foldMapWithIndex (moduleParams m) $ \idx param ->
    case paramDefault param of
        Just e -> S.singleton $ EBinCmp BEq (EParam idx) e
        Nothing -> S.empty

addInstParamConstraints d m = over _moduleConstraints (<> instParamConstraints d m) m


connTy m side (ExtPort i) = portTy $ moduleSidePort m side i
connTy m side (LogicPort i j) =  pinTy $ logicSidePin (m `moduleLogic` i) side j

tyEqConstraints :: Text -> Ty -> Ty -> [ConstExpr]
tyEqConstraints name t1 t2 = go t1 t2
  where
    go (TWire ws1 ds1) (TWire ws2 ds2)
      | length ws1 == length ws2 && length ds1 == length ds2 =
        zipWith (EBinCmp BEq) ws1 ws2 <> zipWith (EBinCmp BEq) ds1 ds2
    go (TEnum t1) t2 = go t1 t2
    go (TAlias _ t1) t2 = go t1 t2
    go t1 (TEnum t2) = go t1 t2
    go t1 (TAlias _ t2) = go t1 t2
    go t1 t2 = traceShow ("warning: net", name, "connects incompatible types", t1, t2) []

typeConstraints m = flip foldMap (moduleNets m) $ \net ->
    let connTys = fmap (connTy m Source) (netSources net) <>
            fmap (connTy m Sink) (netSinks net) in
    let name = moduleName m <> "." <> netName net in
    foldMap (\t -> S.fromList $ tyEqConstraints name (netTy net) t) connTys 

addTypeConstraints m = over _moduleConstraints (<> typeConstraints m) m


