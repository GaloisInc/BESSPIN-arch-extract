{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Constraints where

import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.Generics
import Data.Map (Map)
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

shiftTy idx t = everywhere (mkT go) t
  where
    go (TWire ws ds) = TWire (map (shiftExpr idx) ws) (map (shiftExpr idx) ds)
    go t = t


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

addInstParamConstraints d m = over _moduleConstraints (<> instParamConstraints d m) m


defaultConstraints m = flip S.foldMapWithIndex (moduleParams m) $ \idx param ->
    case paramDefault param of
        Just e -> S.singleton $ EBinCmp BEq (EParam idx) e
        Nothing -> S.empty

addRootDefaultConstraints d idx =
    over (_designMod idx . _moduleConstraints)
        (<> defaultConstraints (d `designMod` idx)) d


connTy m side (ExtPort i) = portTy $ moduleSidePort m side i
connTy m side (LogicPort i j) =  pinTy $ logicSidePin (m `moduleLogic` i) side j

tyEqConstraints :: Show a => (Ty -> Ty -> a) -> Ty -> Ty -> [ConstExpr]
tyEqConstraints warn t1 t2 = go t1 t2
  where
    go (TWire ws1 ds1) (TWire ws2 ds2)
      | length ws1 == length ws2 && length ds1 == length ds2 =
        zipWith (EBinCmp BEq) ws1 ws2 <> zipWith (EBinCmp BEq) ds1 ds2
    go (TEnum t1) t2 = go t1 t2
    go (TAlias _ t1) t2 = go t1 t2
    go t1 (TEnum t2) = go t1 t2
    go t1 (TAlias _ t2) = go t1 t2
    go t1 t2 = traceShow (warn t1 t2) []


netTypeConstraints m = flip foldMap (moduleNets m) $ \net ->
    let connTys = fmap (connTy m Source) (netSources net) <>
            fmap (connTy m Sink) (netSinks net) in
    let name = moduleName m <> "." <> netName net in
    let warn = \t1 t2 -> ("warning: net", name, "connects incompatible types", t1, t2) in
    foldMap (\t -> S.fromList $ tyEqConstraints warn (netTy net) t) connTys

addNetTypeConstraints m = over _moduleConstraints (<> netTypeConstraints m) m


portTypeConstraints d m = flip S.foldMapWithIndex (moduleLogics m) $ \idx logic ->
    case logicKind logic of
        LkInst inst ->
            let instMod = d `designMod` instModId inst in
            let warn f t1 t2 = ("warning: instance", moduleName m <> "." <> instName inst,
                    "port", portName f, "connects incompatible types", t1, t2) in
            let go f a = S.fromList $
                    tyEqConstraints (warn f) (shiftTy idx $ portTy f) (pinTy a) in
            join $
                S.zipWith go (moduleInputs instMod) (logicInputs logic) <>
                S.zipWith go (moduleOutputs instMod) (logicOutputs logic)
        _ -> S.empty

addPortTypeConstraints d m = over _moduleConstraints (<> portTypeConstraints d m) m


addTypeConstraints d m = addPortTypeConstraints d $ addNetTypeConstraints m


type VarPath = ([Int], Int)

-- Flatten a design down to a list of variables and a list of constraints.  In
-- the output `ConstExpr`s, `Param i` refers to the `i`th variable in the list.
flattenConstraints :: Design a -> Int -> (Seq Text, Seq ConstExpr)
flattenConstraints d rootId = (varNames, cons)
  where
    varNames :: Seq Text
    -- Key is a path to a parameter in the design.  The list of inst indexes is
    -- stored in reverse, so `([a, b], c) = inst_b.inst_a.param_c`.
    pathMap :: Map ([Int], Int) Int
    (varNames, pathMap) = mkVars "$" [] rootId S.empty M.empty

    mkVars namePrefix instPath modId accNames accMap =
        let m = d `designMod` modId in
        let accNames' = accNames <> fmap (\p -> namePrefix <> paramName p) (moduleParams m) in
        let accMap' = accMap <> M.fromList [((instPath, i), S.length accNames + i) |
                i <- [0 .. S.length (moduleParams m) - 1]] in
        S.foldlWithIndex (\(accNames, accMap) idx logic -> case logicKind logic of
            LkInst inst ->
                mkVars (namePrefix <> instName inst <> "$") (idx : instPath)
                    (instModId inst) accNames accMap
            _ -> (accNames, accMap)) (accNames', accMap') (moduleLogics m)

    findVar :: ([Int], Int) -> Int
    findVar p = case M.lookup p pathMap of
        Just i -> i
        Nothing -> error $ "no var has path " ++ show p

    convExpr instPath e = everywhere (mkT go) e
      where
        go (EParam p) = EParam $ findVar (instPath, p)
        go (EInstParam is p) = EParam $ findVar (reverse is ++ instPath, p)
        go e = e

    cons = mkCons [] rootId

    mkCons instPath modId =
        let m = d `designMod` modId in
        fmap (convExpr instPath) (moduleConstraints m) <>
        S.foldMapWithIndex (\idx logic -> case logicKind logic of
            LkInst inst -> mkCons (idx : instPath) (instModId inst)
            _ -> S.empty) (moduleLogics m)
