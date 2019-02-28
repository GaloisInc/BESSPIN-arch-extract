{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Constraints where

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
import qualified BESSPIN.ArchExtract.Config as Config
import qualified BESSPIN.ArchExtract.Constraints.Parser as P
import qualified BESSPIN.ArchExtract.Constraints.Resolve as R


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


data OverrideOrigin = OoLocal Int Int | OoInst Int Int Int
    deriving (Show, Eq, Ord)

data FlatConstraints = FlatConstraints
    { fcVars :: Seq Text
    , fcOverrides :: Seq (Text, OverrideOrigin)
    , fcConstraints :: Seq Constraint
    }
    deriving (Show)


data ModInfo = ModInfo
    { miModId :: Int
    , miPrefix :: Text
    , miInsts :: Map Int ModInfo
    -- Maps local parameter index to an index in the global var list.
    , miVarMap :: Map Int Int
    }

data FlattenState = FlattenState
    { fsVars :: Seq Text
    , fsOverrides :: Seq (Text, OverrideOrigin)
    , fsOverrideMap :: Map OverrideOrigin Int
    , fsConstraints :: Seq Constraint
    }

addVar :: Text -> State FlattenState Int
addVar n = do
    i <- gets $ S.length . fsVars
    modify $ \s -> s { fsVars = fsVars s |> n }
    return i

getOverride :: Design a -> OverrideOrigin -> State FlattenState Int
getOverride d origin = state $ \s -> case M.lookup origin (fsOverrideMap s) of
    Just i -> (i, s)
    Nothing ->
        let name = overrideName d origin in
        let i = S.length $ fsOverrides s in
        (i, s
            { fsOverrides = fsOverrides s |> (name, origin)
            , fsOverrideMap = M.insert origin i $ fsOverrideMap s
            })

overrideName d o = case o of
    OoLocal modId paramIdx ->
        let m = d `designMod` modId in
        let p = m `moduleParam` paramIdx in
        moduleName m <> "$" <> paramName p
    OoInst modId instIdx paramIdx ->
        let m = d `designMod` modId in
        let LkInst i = logicKind $ m `moduleLogic` instIdx in
        let p = (d `designMod` instModId i) `moduleParam` paramIdx in
        moduleName m <> "$" <> instName i <> "$" <> paramName p

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
            traceShow ("one-to-one", checkOneToOneEq e, o) $ addConstraint e o

        return mi

    goExpr mi e = everywhereM (mkM go) e
      where
        go (EParam sp p) = return $ EParam sp (miVarMap mi M.! p)
        go (EInstParam sp is p) =
            let mi' = foldl (\mi i -> miInsts mi M.! i) mi is in
            return $ EParam sp (miVarMap mi' M.! p)
        go (EOverrideLocalParam i e) = do
            o <- getOverride d $ OoLocal (miModId mi) i
            return $ EOverride o e
        go (EOverrideInstParam i j e) = do
            o <- getOverride d $ OoInst (miModId mi) i j
            return $ EOverride o e
        go e = return e

    conv fs = FlatConstraints
        { fcVars = fsVars fs
        , fcOverrides = fsOverrides fs
        , fcConstraints = fsConstraints fs
        }


{-

  where
    varNames :: Seq Text
    -- Key is a path to a parameter in the design.  The list of inst indexes is
    -- stored in reverse, so `([a, b], c) = inst_b.inst_a.param_c`.
    pathMap :: Map ([Int], Int) Int
    instNameMap :: Map [Int] Text
    (varNames, pathMap, prefixMap) = mkVars "$" [] rootId S.empty M.empty M.empty

    mkVars namePrefix instPath modId accNames accMap accPrefixes =
        let m = d `designMod` modId in
        let accNames' = accNames <> fmap (\p -> namePrefix <> paramName p) (moduleParams m) in
        let accMap' = accMap <> M.fromList [((instPath, i), S.length accNames + i) |
                i <- [0 .. S.length (moduleParams m) - 1]] in
        let accPrefixes' = M.insert instPath namePrefix accPrefixes in
        S.foldlWithIndex (\(accNames, accMap, accPrefixes) idx logic -> case logicKind logic of
                LkInst inst ->
                    mkVars (namePrefix <> instName inst <> "$") (idx : instPath)
                        (instModId inst) accNames accMap
                _ -> (accNames, accMap, accPrefixes))
            (accNames', accMap', accPrefixes') (moduleLogics m)

    findVar :: ([Int], Int) -> Int
    findVar p = case M.lookup p pathMap of
        Just i -> i
        Nothing -> error $ "no var has path " ++ show p

    convExpr :: [Int] -> ConstExpr -> ConstExpr
    convExpr instPath e = everywhere (mkT go) e
      where
        go (EParam sp p) = EParam sp $ findVar (instPath, p)
        go (EInstParam sp is p) = EParam sp $ findVar (reverse is ++ instPath, p)
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
-}

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



simplifyConstraints :: FlatConstraints -> FlatConstraints
simplifyConstraints fc = fc { fcConstraints = S.filter go $ fcConstraints fc }
  where
    go c = not $ checkConstEq $ constraintExpr c


data OneToOneResult =
    -- The expression's value is a constant.
      RConst
    -- The expression is a one-to-one expression of the indicated variable.
    | RVar Int
    -- Neither of the above conditions holds.
    | RUnknown
    deriving (Show, Eq)

joinOneToOne RConst x = x
joinOneToOne x RConst = x
joinOneToOne _ _ = RUnknown

oneToOneExpr :: ConstExpr -> OneToOneResult
oneToOneExpr e = go e
  where
    go (EIntLit _ _) = RConst
    go (EParam _ v) = RVar v
    go (EInstParam _ _ _) = RUnknown
    go (EUnArith _ UClog2 e) = go e
    go (EUnArith _ UIsPow2 e) = scramble $ go e
    go (EBinArith _ _ l r) = mix (go l) (go r)
    go (EBinCmp _ _ l r) = scramble $ mix (go l) (go r)
    go (ERangeSize _ l r) = mix (go l) (go r)
    go (EOverride _ e) = go e       -- TODO
    go (EOverrideLocalParam _ _) = RUnknown
    go (EOverrideInstParam _ _ _) = RUnknown

    -- Simulate the effect of a non-one-to-one function.
    scramble RConst = RConst
    scramble (RVar _)  = RUnknown
    scramble RUnknown  = RUnknown

    -- Simulate the effect of a binary operation that mixes its two operands in
    -- a reversible way.  If one side is `RConst`, the operation can be
    -- reversed to retrieve the other operand, but if both sides are `RVar`
    -- (even the same `RVar`), information may be lost.
    mix RConst x = x
    mix x RConst = x
    mix _ _ = RUnknown

checkOneToOneEq :: ConstExpr -> Maybe (Int, Int)
checkOneToOneEq (EBinCmp _ BEq l r)
  | RVar i <- oneToOneExpr l, RVar j <- oneToOneExpr r = Just (i, j)
checkOneToOneEq _ = Nothing

checkConstEq :: ConstExpr -> Bool
checkConstEq (EBinCmp _ BEq l r)
  | RConst <- oneToOneExpr l, RConst <- oneToOneExpr r = True
checkConstEq _ = False
