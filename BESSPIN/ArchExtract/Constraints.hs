{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Constraints
    ( module BESSPIN.ArchExtract.Constraints
    , module BESSPIN.ArchExtract.Constraints.Flat
    , simplifyConstraints
    ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
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
import BESSPIN.ArchExtract.Constraints.Flat
import qualified BESSPIN.ArchExtract.Config as Config
import qualified BESSPIN.ArchExtract.Constraints.Parser as P
import qualified BESSPIN.ArchExtract.Constraints.Resolve as R
import BESSPIN.ArchExtract.Constraints.Simplify


-- Shift all `EParam` and `EInstParam` by prepending `idx`.  This converts
-- `x.y` into `i.x.y`, where `i` is the instance at index `idx`.
shiftExpr idx e = everywhere (mkT go) e
  where
    go (EParam sp p) = EInstParam sp [idx] p
    go (EInstParam sp is p) = EInstParam sp (idx : is) p
    go e = e

shiftTy idx t = everywhere (mkT go) t
  where
    go (TWire ws ds) = TWire (map (shiftExpr idx) ws) (map (shiftExpr idx) ds)
    go t = t


maybeWrap True f x = f x
maybeWrap False _ x = x


data ParamExpr = PeExplicit ConstExpr | PeDefault ConstExpr | PeUnset
  deriving (Show)

-- Function that maybe wraps an expression using a function, or maybe doesn't,
-- depending on the override config flags.
type OverrideFunc = (ConstExpr -> ConstExpr) -> ConstExpr -> ConstExpr

-- Generate constraints arising from module instantiation parameters.  That is,
-- converts `mkMod #(foo = bar + baz) the_mod ...` into `parent$the_mod$foo =
-- parent$bar + parent$baz`.  Includes constraints for parameters that are left
-- at their default values, but not PkLocal or PkEnum parameters.
--
-- If `override` is set, generates `EOverrideInstParam` expressions so that
-- parameter expressions can be overridden.  This includes overrides for
-- defaulted parameters (PkNormal ones only), since they can be overridden by
-- providing a value at the instantiation site.
instParamConstraints :: OverrideFunc -> Design a -> Module b -> Seq Constraint
instParamConstraints maybeOverride d m =
    flip S.foldMapWithIndex (moduleLogics m) $ \idx logic ->
        case logicKind logic of
            LkInst inst -> go idx inst
            _ -> S.empty
  where
    go idx inst =
        let formals = (moduleParams $ d `designMod` instModId inst) in
        flip S.foldMapWithIndex formals $ \paramIdx formal -> do
            guard $ paramKind formal == PkNormal
            let actualExpr = join $ instParams inst S.!? paramIdx
            let formalExpr = paramDefault formal
            (e, origin) <- case (actualExpr, formalExpr) of
                (Just e, _) -> return (e, CoInstParam idx paramIdx)
                (Nothing, Just e) -> return (shiftExpr idx e, CoInstParam idx paramIdx)
                (Nothing, Nothing) -> mzero
            return $ Constraint
                (EBinCmp dummySpan BEq (EInstParam dummySpan [idx] paramIdx)
                    (maybeOverride (EOverrideInstParam idx paramIdx) e))
                origin

addInstParamConstraints maybeOverride d m =
    over _moduleConstraints (<> instParamConstraints maybeOverride d m) m


-- Generate constraints arising from default initializers of `PkLocal`
-- parameters.  That is, converts `parameter foo = bar + baz` into `parent$foo
-- = parent$bar + parent$baz`.
--
-- If `override` is set, generates `EOverrideLocalParam` expressions so that
-- the initializer expressions can be overridden.
localDefaultConstraints :: OverrideFunc -> Module a -> Seq Constraint
localDefaultConstraints maybeOverride m =
    filteredDefaultConstraints maybeOverride m (== PkLocal)

-- Generate constraints arising from default initializers of `PkNormal`
-- parameters.  This is like `localDefaultConstraints`, but for cases like
-- `module #(foo = 17) top ...`, where you actually want a `parent$foo = 17`
-- constraint in `top` itself.  Mostly useful for the root module, which
-- doesn't have an instantiation to set values for those parameters.
rootDefaultConstraints :: OverrideFunc -> Module a -> Seq Constraint
rootDefaultConstraints maybeOverride m =
    filteredDefaultConstraints maybeOverride m (== PkNormal)

-- Helper function for local/rootDefaultConstraints
filteredDefaultConstraints :: OverrideFunc -> Module a -> (ParamKind -> Bool) -> Seq Constraint
filteredDefaultConstraints maybeOverride m f =
    flip S.foldMapWithIndex (moduleParams m) $ \idx param -> case paramDefault param of
        Just e | f $ paramKind param -> S.singleton $
            Constraint
                (EBinCmp dummySpan BEq (EParam dummySpan idx)
                    (maybeOverride (EOverrideLocalParam idx) e))
                (CoParamDefault idx)
        _ -> S.empty


addLocalDefaultConstraints maybeOverride m =
    over _moduleConstraints (<> localDefaultConstraints maybeOverride m) m

addRootDefaultConstraints maybeOverride m =
    over _moduleConstraints (<> rootDefaultConstraints maybeOverride m) m


connTy m side (ExtPort i) = portTy $ moduleSidePort m side i
connTy m side (LogicPort i j) =  pinTy $ logicSidePin (m `moduleLogic` i) side j

tyEqConstraints :: Show a => (Ty -> Ty -> a) -> Ty -> Ty -> [ConstExpr]
tyEqConstraints warn t1 t2 = go t1 t2
  where
    go (TWire ws1 ds1) (TWire ws2 ds2)
      | length ds1 == length ds2 =
        EBinCmp dummySpan BEq (prod ws1) (prod ws2) :
        zipWith (EBinCmp dummySpan BEq) ds1 ds2
    go (TWire _ []) TUnsizedInt = []
    go TUnsizedInt (TWire _ []) = []
    go (TEnum t1) t2 = go t1 t2
    go (TAlias _ t1) t2 = go t1 t2
    go t1 (TEnum t2) = go t1 t2
    go t1 (TAlias _ t2) = go t1 t2
    go t1 t2 = traceShow (warn t1 t2) []

    prod [] = EIntLit dummySpan 1
    prod xs = foldl1 (EBinArith dummySpan BMul) xs


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


addCustomConstraints es m =
    over _moduleConstraints (<> S.fromList (map (\e -> Constraint e CoCustom) es)) m


addConstraintsForConfig cfg d = over _designMods go d
  where
    go ms = flip S.mapWithIndex ms $ \idx m ->
        (if Config.constraintsUseInstParams cfg then
            addInstParamConstraints instParamOverride d
        else id) $
        (if Config.constraintsUseLocalDefaults cfg then
            addLocalDefaultConstraints localParamOverride
        else id) $
        (if Set.member idx forceDefaultIds then
            addRootDefaultConstraints forcedParamOverride
        else id) $
        (if Config.constraintsUseNetTypes cfg then addNetTypeConstraints else id) $
        (if Config.constraintsUsePortTypes cfg then addPortTypeConstraints d else id) $
        (case M.lookup idx customMap of
            Nothing -> id
            Just es -> addCustomConstraints es) $
        m

    modIdMap :: Map Text Int
    modIdMap = M.fromList $ toList $
        S.mapWithIndex (\idx m -> (moduleName m, idx)) $ designMods d

    forceDefaultIds :: Set Int
    forceDefaultIds = Set.fromList $
        map (modIdMap M.!) $ Config.constraintsForceModuleDefaults cfg

    instParamOverride f e = maybeOverride Config.constraintsOverrideInstParams f e
    localParamOverride f e = maybeOverride Config.constraintsOverrideLocalParams f e
    forcedParamOverride f e = maybeOverride Config.constraintsOverrideForcedParams f e

    maybeOverride getFlag f e
        | not $ getFlag cfg = e
        | Config.constraintsAllowOverrideNonConstant cfg = f e
        | isConstExpr e = f e
        | otherwise = e

    rcx = R.mkContext d

    parseResolve idx s = R.resolve (R.withCurModule idx rcx) $ P.parseConstraint s

    customMap = foldMap (\(modName, conStrs) -> case M.lookup modName modIdMap of
            Nothing -> error $ "got constraints for unknown module " <> T.unpack modName
            Just idx -> M.singleton idx $ map (parseResolve idx) conStrs) $
        Config.constraintsCustom cfg

-- Checks if a `ConstExpr` (which may be better named "param expr") is truly
-- constant, meaning it doesn't depend on the values of any parameters.
isConstExpr e = everything (&&) (True `mkQ` go) e
  where
    go (EParam _ _) = False
    go (EInstParam _ _ _) = False
    go _ = True


data ModInfo = ModInfo
    { miModId :: Int
    , miPrefix :: Text
    , miInsts :: Map Int ModInfo
    -- Maps local parameter index to an index in the global var list.
    , miVarMap :: Map Int Int
    }

data FlattenState = FlattenState
    { fsVars :: Seq Text
    , fsOverrides :: Seq Override
    , fsOverrideMap :: Map OverrideOrigin Int
    , fsConstraints :: Seq Constraint
    }

addVar :: Text -> State FlattenState Int
addVar n = do
    i <- gets $ S.length . fsVars
    modify $ \s -> s { fsVars = fsVars s |> n }
    return i

getOverride :: Design a -> OverrideOrigin -> ConstExpr -> State FlattenState Int
getOverride d origin expr = state $ \s -> case M.lookup origin (fsOverrideMap s) of
    Just i ->
        (i, s
            { fsOverrides = fsOverrides s &
                ix i . _overrideDefault %~ mergeOverrideDefault expr
            })
    Nothing ->
        let name = genOverrideName d origin in
        let i = S.length $ fsOverrides s in
        (i, s
            { fsOverrides = fsOverrides s |> (Override name origin (Just expr))
            , fsOverrideMap = M.insert origin i $ fsOverrideMap s
            })

genOverrideName d o = case o of
    OoLocal modId paramIdx ->
        let m = d `designMod` modId in
        let p = m `moduleParam` paramIdx in
        moduleName m <> "$" <> paramName p
    OoInst modId instIdx paramIdx ->
        let m = d `designMod` modId in
        let LkInst i = logicKind $ m `moduleLogic` instIdx in
        let p = (d `designMod` instModId i) `moduleParam` paramIdx in
        moduleName m <> "$" <> instName i <> "$" <> paramName p

-- Combine new expr `e` with the existing default for an override.  Constant
-- exprs take precedence over non-constant exprs, but two distinct constants
-- conflict, producing `Nothing`.
mergeOverrideDefault _ Nothing = Nothing
mergeOverrideDefault e (Just e')
  | not (isConstExpr e) = Just e'
  | not (isConstExpr e') = Just e
  | unspan e == unspan e' = Just e
  | otherwise = Nothing
  where
    unspan e = everywhere (mkT $ \_ -> Span 0 0) e

addConstraint :: ConstExpr -> ConstraintOrigin -> State FlattenState ()
addConstraint e o = modify $ \s -> s { fsConstraints = fsConstraints s |> Constraint e o }


-- Flatten a design down to a list of variables and a list of constraints.  In
-- the output `ConstExpr`s, `Param i` refers to the `i`th variable in the list.
flattenConstraints :: Design a -> Int -> FlatConstraints
flattenConstraints d rootId = conv $ execState (go rootId "") initState
  where
    initState = FlattenState S.empty S.empty M.empty S.empty

    go modId prefix = do
        let m = d `designMod` modId

        -- First, recurse into child instances.
        insts <- liftM (foldMap id) $ flip S.traverseWithIndex (moduleLogics m) $ \idx l ->
            case logicKind l of
                LkInst inst -> do
                    mi <- go (instModId inst) (prefix <> "$" <> instName inst)
                    return $ M.singleton idx mi
                _ -> return M.empty

        -- Generate vars for all parameter of `m`
        varMap <- liftM (foldMap id) $ flip S.traverseWithIndex (moduleParams m) $ \idx p -> do
            varIdx <- addVar (prefix <> "$" <> paramName p)
            return $ M.singleton idx varIdx

        -- Translate constraints, defining overrides as a side effect.
        let mi = ModInfo modId prefix insts varMap
        forM_ (moduleConstraints m) $ \c -> do
            e <- goExpr mi $ constraintExpr c
            let o = CoText $ showOrigin d m $ constraintOrigin c
            addConstraint e o

        return mi

    goExpr mi e = everywhereM (mkM go) e
      where
        go (EParam sp p) = return $ EParam sp (miVarMap mi M.! p)
        go (EInstParam sp is p) =
            let mi' = foldl (\mi i -> miInsts mi M.! i) mi is in
            return $ EParam sp (miVarMap mi' M.! p)
        go (EOverrideLocalParam i e) = do
            o <- getOverride d (OoLocal (miModId mi) i) e
            return $ EOverride o e
        go (EOverrideInstParam i j e) = do
            o <- getOverride d (OoInst (miModId mi) i j) e
            return $ EOverride o e
        go e = return e

    conv fs = FlatConstraints
        { fcVars = fsVars fs
        , fcOverrides = fsOverrides fs
        , fcConstraints = fsConstraints fs
        }


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
    CoCustom -> "custom"
    CoText t -> t
