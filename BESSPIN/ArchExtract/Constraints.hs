{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Constraints where

import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.Generics
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro
import Lens.Micro.Platform

import Debug.Trace

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


data ParamExpr = PeExplicit ConstExpr | PeDefault ConstExpr | PeUnset
  deriving (Show)

instParamConstraints :: Bool -> Design a -> Module b -> Seq Constraint
instParamConstraints useDefaults d m =
    flip S.foldMapWithIndex (moduleLogics m) $ \idx logic ->
        case logicKind logic of
            LkInst inst -> go idx inst
            _ -> S.empty
  where
    go idx inst =
        let formals = (moduleParams $ d `designMod` instModId inst) in
        flip S.foldMapWithIndex formals $ \paramIdx formal ->
            let actualExpr = join $ instParams inst S.!? paramIdx in
            let formalExpr = paramDefault formal in
            let mkEq c = EBinCmp BEq (EInstParam [idx] paramIdx) c in
            case paramVal idx actualExpr formalExpr of
                PeExplicit c -> S.singleton $
                    Constraint (mkEq c) (CoInstParam idx paramIdx)
                PeDefault c | useDefaults -> S.singleton $
                    Constraint (mkEq c) (CoInstParamDefault idx paramIdx)
                _ -> S.empty

    paramVal instIdx (Just e) _ = PeExplicit e
    paramVal instIdx Nothing (Just e) = PeDefault $ shiftExpr instIdx e
    paramVal instIdx Nothing Nothing = PeUnset

addInstParamConstraints useDefaults d m =
    over _moduleConstraints (<> instParamConstraints useDefaults d m) m


defaultConstraints :: Module a -> Seq Constraint
defaultConstraints m = flip S.foldMapWithIndex (moduleParams m) $ \idx param ->
    case paramDefault param of
        Just e -> S.singleton $
            Constraint (EBinCmp BEq (EParam idx) e) (CoParamDefault idx)
        Nothing -> S.empty

addDefaultConstraints m = over _moduleConstraints (<> defaultConstraints m) m

addRootDefaultConstraints d idx =
    over (_designMod idx . _moduleConstraints)
        (<> defaultConstraints (d `designMod` idx)) d


connTy m side (ExtPort i) = portTy $ moduleSidePort m side i
connTy m side (LogicPort i j) =  pinTy $ logicSidePin (m `moduleLogic` i) side j

tyEqConstraints :: Show a => (Ty -> Ty -> a) -> Ty -> Ty -> [ConstExpr]
tyEqConstraints warn t1 t2 = go t1 t2
  where
    go (TWire ws1 ds1) (TWire ws2 ds2)
      | length ds1 == length ds2 =
        EBinCmp BEq (prod ws1) (prod ws2) : zipWith (EBinCmp BEq) ds1 ds2
    go (TWire _ []) TUnsizedInt = []
    go TUnsizedInt (TWire _ []) = []
    go (TEnum t1) t2 = go t1 t2
    go (TAlias _ t1) t2 = go t1 t2
    go t1 (TEnum t2) = go t1 t2
    go t1 (TAlias _ t2) = go t1 t2
    go t1 t2 = traceShow (warn t1 t2) []

    prod [] = EIntLit 1
    prod xs = foldl1 (EBinArith BMul) xs


netTypeConstraints :: Module a -> Seq Constraint
netTypeConstraints m = flip S.foldMapWithIndex (moduleNets m) $ \netIdx net ->
    let connTys side conns =
            S.mapWithIndex (\connIdx conn ->
                    (connTy m side conn, CoNetConn (NetId netIdx) side connIdx)) conns in
    let name = moduleName m <> "." <> netName net in
    let warn = \t1 t2 -> ("warning: net", name, "connects incompatible types", t1, t2) in
    foldMap (\(t,o) ->
            S.fromList $ map (\e -> Constraint e o) $
                tyEqConstraints warn (netTy net) t)
        (connTys Source (netSources net) <> connTys Sink (netSinks net))

addNetTypeConstraints m = over _moduleConstraints (<> netTypeConstraints m) m


portTypeConstraints :: Design a -> Module b -> Seq Constraint
portTypeConstraints d m = flip S.foldMapWithIndex (moduleLogics m) $ \idx logic ->
    case logicKind logic of
        LkInst inst ->
            let instMod = d `designMod` instModId inst in
            let warn p t1 t2 = ("warning: instance", moduleName m <> "." <> instName inst,
                    "port", portName p, "connects incompatible types", t1, t2) in
            let go side portIdx port =
                    let formal = shiftTy idx $ portTy port in
                    let actual = pinTy $ logicSidePin logic side portIdx in
                    let origin = CoPortConn idx side portIdx in
                    S.fromList $ map (\e -> Constraint e origin) $
                        tyEqConstraints (warn port) formal actual in
            join $
                S.mapWithIndex (go Sink) (moduleInputs instMod) <>
                S.mapWithIndex (go Source) (moduleOutputs instMod)
        _ -> S.empty

addPortTypeConstraints d m = over _moduleConstraints (<> portTypeConstraints d m) m


addTypeConstraints d m = addPortTypeConstraints d $ addNetTypeConstraints m


addConstraintsForConfig cfg d = over _designMods go d
  where
    go ms = flip S.mapWithIndex ms $ \idx m ->
        (if Config.constraintsUseInstParams cfg then
            addInstParamConstraints (Config.constraintsUseInstParamDefaults cfg) d
        else id) $
        (if Config.constraintsUseNetTypes cfg then addNetTypeConstraints else id) $
        (if Config.constraintsUsePortTypes cfg then addPortTypeConstraints d else id) $
        (if Set.member idx forceDefaultIds then addDefaultConstraints else id) $
        m

    modIdMap :: Map Text Int
    modIdMap = M.fromList $ toList $
        S.mapWithIndex (\idx m -> (moduleName m, idx)) $ designMods d

    forceDefaultIds :: Set Int
    forceDefaultIds = Set.fromList $
        map (modIdMap M.!) $ Config.constraintsForceModuleDefaults cfg


type VarPath = ([Int], Int)

-- Flatten a design down to a list of variables and a list of constraints.  In
-- the output `ConstExpr`s, `Param i` refers to the `i`th variable in the list.
flattenConstraints :: Design a -> Int -> (Seq Text, Seq Constraint)
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

    convExpr :: [Int] -> ConstExpr -> ConstExpr
    convExpr instPath e = everywhere (mkT go) e
      where
        go (EParam p) = EParam $ findVar (instPath, p)
        go (EInstParam is p) = EParam $ findVar (reverse is ++ instPath, p)
        go e = e

    cons = mkCons [] rootId

    mkCons instPath modId =
        let m = d `designMod` modId in
        let conv (Constraint e o) =
                Constraint (convExpr instPath e) (CoText $ showOrigin d m o) in
        fmap conv (moduleConstraints m) <>
        S.foldMapWithIndex (\idx logic -> case logicKind logic of
            LkInst inst -> mkCons (idx : instPath) (instModId inst)
            _ -> S.empty) (moduleLogics m)

showConn d m side c = case c of
    ExtPort i -> "port-" <> portName (moduleSidePort m side i)
    LogicPort i j ->
        let l = m `moduleLogic` i in
        case logicKind l of
            LkInst inst ->
                let instMod = d `designMod` instModId inst in
                "logic-inst-" <> instName inst <> "$" <>
                    portName (moduleSidePort instMod (flipSide side) j)
            LkRegister name ->
                "logic-reg-" <> name <> "$" <> T.pack (show side) <> T.pack (show j)
            LkDFlipFlop name resets ->
                let prefix = "logic-dff-" <> name <> "$" in
                case (side, j) of
                    (Source, 0) -> prefix <> "q"
                    (Sink, 0) -> prefix <> "d"
                    (Sink, 1) -> prefix <> "clk"
                    (Sink, n) -> prefix <> "res" <> T.pack (show $ n - 2)
            _ -> "logic-" <> T.pack (show i) <> "$" <> T.pack (show side) <> T.pack (show j)

showOrigin d m o = case o of
    CoInstParam i j ->
        let LkInst inst = logicKind $ m `moduleLogic` i in
        let instMod = d `designMod` instModId inst in
        "param-" <> moduleName m <> "$" <> instName inst <> "$" <>
            paramName (instMod `moduleParam` j)
    CoInstParamDefault i j ->
        let LkInst inst = logicKind $ m `moduleLogic` i in
        let instMod = d `designMod` instModId inst in
        "param-default-" <> moduleName m <> "$" <> instName inst <> "$" <>
            paramName (instMod `moduleParam` j)
    CoParamDefault i ->
        "param-default-" <> moduleName m <> "$" <> paramName (m `moduleParam` i)
    CoNetConn i side j ->
        let n = m `moduleNet` i in
        "net-conn-" <> moduleName m <> "$" <> head (T.lines $ netName n) <>
            "-to-" <> showConn d m side (netSideConn n side j)
    CoPortConn i side j ->
        let LkInst inst = logicKind $ m `moduleLogic` i in
        let instMod = d `designMod` instModId inst in
        "port-conn-" <> moduleName m <> "$" <> instName inst <> "$" <>
            portName (moduleSidePort instMod (flipSide side) j)
    CoText t -> t
