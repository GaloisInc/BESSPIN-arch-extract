{-# LANGUAGE OverloadedStrings, TemplateHaskell, RankNTypes #-}
module BESSPIN.ArchExtract.BSV.Extract where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.Array.ST
import Data.Foldable
import Data.Generics
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UnionFind.ST as UF
import Lens.Micro.Platform
import Text.Read (readMaybe)

import Debug.Trace
import Data.List

import BESSPIN.ArchExtract.Lens
import qualified BESSPIN.ArchExtract.Architecture as A
import BESSPIN.ArchExtract.BSV.Raw
import BESSPIN.ArchExtract.Simplify (reconnectNets)


data ExtractState = ExtractState
    { esCurModule :: A.Module ()
    , esCurRuleName :: Text
    , esRuleCounter :: Int
    }

makeLenses' ''ExtractState

type ExtractM a = State ExtractState a


addThing :: a -> Lens' ExtractState (Seq a) -> ExtractM Int
addThing x fld = do
    idx <- S.length <$> use fld
    fld %= (|> x)
    return idx

addNet :: A.Net () -> ExtractM A.NetId
addNet n = A.NetId <$> addThing n (_esCurModule . A._moduleNets)

addLogic :: A.Logic () -> ExtractM Int
addLogic l = addThing l (_esCurModule . A._moduleLogics)


withThing' :: Lens' ExtractState a -> a -> ExtractM b -> ExtractM (b, a)
withThing' fld x m = do
    old <- fld <<.= x
    r <- m
    new <- fld <<.= old
    return (r, new)

withModule  :: A.Module () -> ExtractM () -> ExtractM (A.Module ())
withModule x m = snd <$> withModule' x m

withModule' :: A.Module () -> ExtractM a -> ExtractM (a, A.Module ())
withModule' x m = withThing' _esCurModule x m

withRuleName  :: Text -> ExtractM a -> ExtractM a
withRuleName x m = fst <$> withThing' _esCurRuleName x m

nextRuleIdx :: ExtractM Int
nextRuleIdx = do
    idx <- use _esRuleCounter
    _esRuleCounter %= (+1)
    return idx


data BSVModule = BSVModule Id Ty Expr

findPackageModules :: Package -> [BSVModule]
findPackageModules p = map fst $ M.elems moduleMap
  where
    -- There are two forms of modules.  For `(* synthesize *)` modules, there
    -- is a top-level definition of type `Module ifc` that contains a let
    -- binding for the actual module definition.
    go (Def _ (TModule _) [Clause [] e])
      | ELet (Def i (TModule tIfc) [Clause [] body]) _ <- e
      = M.singleton (idName i) (BSVModule i tIfc body, 1)
    -- For non-synthesize modules, the type is `IsModule m c -> m ifc`, and the
    -- body is the module definition directly.
    go (Def i (TIsModule (TVar iM) (TVar iC) `TArrow` TApp (TVar iM') [tIfc])
            [Clause [PTcDict] body])
      | iM == iM' = M.singleton (idName i) (BSVModule i tIfc body, 0)
    go _ = M.empty

    moduleMap = M.unionsWith (\(a, ap) (b, bp) ->
        if ap > bp then (a, ap) else (b, bp)) (map go $ toList $ packageDefs p)

extractDesign :: [Package] -> A.Design ()
extractDesign ps = A.Design archMods
  where
    bsvMods = concatMap findPackageModules ps
    initState = ExtractState
        { esCurModule = error "no current module"
        , esCurRuleName =
            traceShow "ran Action not in any rule" $
            "[[error: no rule]]"
        , esRuleCounter = 0
        }
    archMods = S.fromList $ evalState (mapM extractModule bsvMods) initState

extractModule :: BSVModule -> ExtractM (A.Module ())
extractModule (BSVModule (Id name _ _) _ body) = do
    let initMod = A.Module name S.empty S.empty S.empty S.empty S.empty S.empty
    m <- withModule initMod $ do
        v <- eval M.empty body >>= runModule
        traceM (show ("module", name, "got value:", v))
        extractMethods v
        return ()
    return $
        reconnectNets $
        m

extractMethods :: Value -> ExtractM ()
extractMethods (VStruct _ fs) = do
    forM_ (M.toList fs) $ \(rawName, val) -> do
        let name = T.takeWhileEnd (/= '.') rawName
        traceM $ "extracting method " ++ show name ++ " = " ++ show val
        extractMethod name val
extractMethods v = traceM ("can't extract methods from " ++ show v)

extractMethod :: Text -> Value -> ExtractM ()
extractMethod name v = goInputs v
  where
    -- Identify input ports from the function arguments, if any.
    goInputs v | isFunc v = do
        inpNames <- funcArgNames v
        traceM $ "goInputs: " ++ show v ++ ": apply to args " ++ show inpNames
        inpNets <- mapM genInputPort $ map (\n -> name <> "." <> n) inpNames
        v' <- appValue v [] (map VNet inpNets)
        goAction v'
    goInputs v = goAction v

    goAction v | isComputation v = do
        traceM $ "goAction: " ++ show v ++ ": runAction"
        v' <- withRuleName name $ runAction v
        goOutput v'
    goAction v = goOutput v

    goOutput v | trace ("goOutput: got " ++ show v) False = undefined
    goOutput (VNet netId) = do
        genOutputPort (name <> ".out") netId
    goOutput VConst = return ()
    goOutput v = traceM $ "unexpected value in method slot: " ++ show v


funcArgNames :: Value -> ExtractM [Text]
funcArgNames (VClosure ps _ _) =
    return $ zipWith (\idx p -> case patName p of
        Nothing -> "in" <> T.pack (show idx)
        Just name -> name) [0..] ps
funcArgNames (VPartApp v _ args) =
    drop (length args) <$> funcArgNames v
funcArgNames (VModuleCtor _) =
    traceShow "funcArgNames: ModuleCtor case NYI" $ return []
funcArgNames (VPrim _) =
    traceShow "funcArgNames: Prim case NYI" $ return []
funcArgNames v = error $ "not a function: " ++ show v

patName :: Pat -> Maybe Text
patName (PVar (Id name _ _)) = Just name
patName _ = Nothing


data Value =
    -- Run-time values are carried on nets.  Evaluating `ERegRead r` produces a
    -- `VNet` referencing `r`'s output (`Q`) net, while evaluating `a & b`
    -- generates a `Logic` and returns its output net.
      VNet A.NetId
    -- Compile-time constants, such as integer literals.  These can be used
    -- anywhere in place of a `VNet`.
    | VConst

    -- Logic values.  These are used as arguments for various operations in the
    -- `Action` monad.
    | VModInst Int
    -- First arg is the index of the register's associated `LkRuleMux` in
    -- `moduleLogics`.  Second arg is the ID of its output net.
    | VDff Int A.NetId

    -- Structs are used to represent whole modules: a module definition in the
    -- source turns into a monadic computation returning a struct.
    | VStruct Text (Map Text Value)

    -- Function-like values.
    | VClosure [Pat] Scope Expr
    -- Partial application.
    | VPartApp Value [Ty] [Value]
    | VModuleCtor Int
    | VPrim Prim

    -- Monadic computations.  These are processed by `runModule` and
    -- `runAction`.
    | VReturn Value
    | VBind Value Value
    -- Module monad primitives.
    | VNamed Text Value
    | VMkReg Ty
    | VMkModule Int [Ty] [Value]
    | VAddRules [RuleVal]
    -- Action monad primitives.
    -- First arg is the index of the target register's rule mux; second arg is
    -- the value to write.  The effect of the write is to connect the value's
    -- output net to a fresh input of the rule mux for the current rule.
    | VRegWrite Int Value
    -- TODO: method call

    -- Produced by ETcDict, and matched by VTcDict.  Should be unused
    -- otherwise.
    | VTcDict
    | VUnknown
    deriving (Show)

data RuleVal = RuleVal Text [Value] Value
    deriving (Show)

type Scope = Map Text Value


-- Evaluate `e` in `sc`.  This lives in the `ExtractM` monad because some
-- evaluation steps produce nets, logics, etc. in the current module.  This
-- includes some `Expr`s that look pure in the source language, like `a & b`.
eval :: Scope -> Expr -> ExtractM Value
eval _ e | traceShow ("evaluate", e) False = undefined
eval sc (EVar i@(Id name _ _))
  | Just v <- M.lookup name sc = return v
  | otherwise = traceShow ("unknown variable", i) $ return VUnknown
eval sc (ELam ps body) = return $ VClosure ps sc body
eval sc (EApp f tys args) = do
    fv <- eval sc f
    argvs <- mapM (eval sc) args
    appValue fv tys argvs
-- TODO: handling multi-clause defs will require multi-clause VClosure, and
-- associated changes to application & pattern matching.
eval sc (ELet (Def (Id name _ _) _ [Clause ps body]) e) =
    eval (M.insert name (VClosure ps sc body) sc) e
-- TODO: letrec is totally unsupported at the moment.  Not sure how hard this
-- would be to implement, but it doesn't seem to be used very often.
eval sc (ELetRec ds e) =
    traceShow ("letrec is not yet supported", [name | Def (Id name _ _) _ _ <- ds]) $
    let sc' = M.fromList [(name, VUnknown) | Def (Id name _ _) _ _ <- ds] in
    eval sc' e

eval sc (ELit _) = return VConst
eval sc (ERules _) = traceShow ("found unraised ERules during eval") $ return VUnknown
eval sc (EStatic p f) = traceShow ("EStatic NYI", p, f) $ return VUnknown

eval sc (EStruct (TCon (Id sName _ _)) fs) = do
    fvs <- forM fs $ \(i, expr) -> do
        val <- eval sc expr
        return (idName i, val)
    return $ VStruct sName $ M.fromList fvs
eval sc (EStruct ty _) =
    traceShow "EStruct with non-TCon type is not supported" $ return VUnknown

eval sc (EPrim p) = return $ VPrim p
eval sc (EDo ss e) = case ss of
    [] -> eval sc e
    s : ss' ->  do
        let (p, m) = case s of
                SBind p _ m -> (p, m)
                SBind' m -> (PWild, m)
        mv <- eval sc m
        let kv = VClosure [p] sc (EDo ss' e)
        return $ VBind mv kv
eval sc (EAddRules rs) = do
    rvs <- mapM (evalRule sc) rs
    return $ VAddRules rvs
eval sc ETcDict = return VTcDict
eval sc (ERegRead e) = do
    r <- eval sc e
    appPrim PRegRead [] [r]
eval sc (ERegWrite re ve) = do
    r <- eval sc re
    v <- eval sc ve
    appPrim PRegWrite [] [r, v]
eval sc (EUnOp op e) = do
    v <- eval sc e
    appPrim (PUnOp op) [] [v]
eval sc (EBinOp op l r) = do
    lv <- eval sc l
    rv <- eval sc r
    appPrim (PBinOp op) [] [lv, rv]

eval sc (EUnknown _) = return VUnknown


evalRule sc (Rule name conds body) = do
    idx <- nextRuleIdx
    let name' = case name of
            Just n -> n
            Nothing -> "rule" <> T.pack (show idx)
    conds' <- mapM (eval sc) conds
    body' <- eval sc body
    return $ RuleVal name' conds' body'


-- Apply a value to (type and value) arguments.
appValue f tys vals | traceShow ("apply", f, tys, vals) False = undefined
appValue f tys [] | not $ isFunc f =
    traceShow ("can't apply", f, "to type arguments", tys) $ return f
appValue f tys vals | not $ isFunc f =
    traceShow ("can't apply", f, "to arguments", tys, vals) $ return VUnknown
appValue f tys vals = do
    (numTys, numVals) <- countArgs f
    -- Partial application is handled by the individual `appFoo` functions.
    -- Over-application is handled here, by applying to exactly the requested
    -- number of args and then recursing.
    let (exactTys, moreTys) = splitAt numTys tys
        (exactVals, moreVals) = splitAt numVals vals

    let hasExact = length exactTys == numTys && length exactVals == numVals
    let hasMore = not $ null moreTys && null moreVals

    case (hasExact, hasMore) of
        (True, False) -> appExact f exactTys exactVals
        (True, True) -> do
            f' <- appExact f exactTys exactVals
            appValue f' moreTys moreVals
        (False, False) -> return $ mkPartApp f tys vals
        -- This last one is an error case.  We have not enough types and too
        -- many args, or vice versa.
        (False, True)
          | not $ null moreVals ->
            traceShow ("can't apply", f, "to arguments") $ return VUnknown
          | otherwise ->
            -- Discard the excess type arguments and hope for the best.
            traceShow ("too many type arguments for", f) $
                appValue (mkPartApp f exactTys exactVals) [] moreVals

-- Like `appValue`, but the caller must provide exactly the right number of
-- `tys` and `vals`.
appExact f tys vals = case f of
    VClosure ps sc' body -> appClosure ps vals sc' body
    VPartApp f tys' vals' -> appExact f (tys' ++ tys) (vals' ++ vals)
    VPrim p -> appPrim p tys vals
    -- TODO: VModuleCtor modId -> ...
    -- Non-function values request no arguments, so we aren't really applying
    -- anything here.
    _ -> return f

    -- For applying a non-function value to types, return the value
    -- unchanged.  This is a workaround for our lack of support for partial
    -- application of type arguments.
--    _ | null argvs -> traceShow ("can't apply", fv, "to types") $ return fv
    -- Applying a non-function to values is a more serious error.
--    _ -> traceShow ("can't apply", fv, "to arguments") $ return VUnknown

mkPartApp (VPartApp f tys vals) tys' vals' =
    VPartApp f (tys ++ tys') (vals ++ vals')
mkPartApp f tys vals =
    VPartApp f tys vals

isFunc (VClosure _ _ _) = True
isFunc (VPartApp _ _ _) = True
isFunc (VModuleCtor _) = True
isFunc (VPrim _) = True
isFunc _ = False

-- Count the number of type and value arguments that `v` expects to receive.
-- Returns (0,0) for non-functions.
countArgs :: Value -> ExtractM (Int, Int)
countArgs (VClosure ps _ _) = return (0, length ps)
countArgs (VPartApp f tys vals) = do
    (numTys, numVals) <- countArgs f
    return (numTys - length tys, numVals - length vals)
countArgs (VPrim p) = return $ countArgsPrim p
-- TODO: countArgs (VModuleCtor modId) = ...
countArgs _ = return (0, 0)

appClosure :: [Pat] -> [Value] -> Scope -> Expr -> ExtractM Value
appClosure ps vs sc body = do
    sc' <- bindPats (zip ps vs) sc
    eval sc' body

bindPats :: [(Pat, Value)] -> Scope -> ExtractM Scope
bindPats pvs sc = foldM (\sc (p, v) -> bindPat p v sc) sc pvs

-- Match `v` against `p`, adding bindings to `sc`.  This can generate logic for
-- some kinds of patterns.
bindPat :: Pat -> Value -> Scope -> ExtractM Scope
bindPat p v sc = case p of
    PWild -> return sc
    PVar (Id name _ _) -> return $ M.insert name v sc
    PTcDict -> case v of
        VTcDict -> return sc
        _ -> traceShow ("passed non-VTcDict", v, "to PTcDict argument", p) $ return sc
    PUnknown _ ->
        traceShow ("tried to match", v, "against unknown pattern") $ return sc

countArgsPrim :: Prim -> (Int, Int)
countArgsPrim p = case p of
    PReturn -> (0, 1)
    PBind -> (0, 2)
    PMkRegU -> (2, 0)
    PPack -> (0, 1)
    PUnpack -> (0, 1)
    PTruncate -> (0, 1)
    PIndex -> (0, 2)
    PRegRead -> (0, 1)
    PRegWrite -> (0, 2)
    PUnOp _ -> (0, 1)
    PBinOp _ -> (0, 2)
    PSetName _ -> (0, 1)

appPrim :: Prim -> [Ty] -> [Value] -> ExtractM Value
appPrim PReturn [] [v] = return $ VReturn v
appPrim PBind [] [m, k] = return $ VBind m k
appPrim PMkRegU [ty, _width] [] = return $ VMkReg ty
appPrim PPack [] [v] = return v
appPrim PUnpack [] [v] = return v
appPrim PTruncate [] [v] = return v
appPrim PIndex [] [v, i] = traceShow ("PIndex NYI") $ return VUnknown
appPrim PRegRead [] [VDff _ qNetId] = return $ VNet qNetId
appPrim PRegWrite [] [VDff muxIdx _, v] = return $ VRegWrite muxIdx v
appPrim (PUnOp _) [] [VConst] = return VConst
appPrim (PUnOp _) [] [VNet v] = VNet <$> genCombLogic (v <| S.empty)
appPrim (PBinOp _) [] [VConst, VConst] = return VConst
appPrim (PBinOp _) [] [VConst, VNet b] = VNet <$> genCombLogic (b <| S.empty)
appPrim (PBinOp _) [] [VNet a, VConst] = VNet <$> genCombLogic (a <| S.empty)
appPrim (PBinOp _) [] [VNet a, VNet b] = VNet <$> genCombLogic (a <| b <| S.empty)
appPrim (PSetName name) [] [c] = return $ VNamed name c
appPrim p tys vals =
    traceShow ("bad arguments for primitive", p, tys, vals) $ return VUnknown

-- Create a combinational logic element with `inps` as its inputs and a fresh
-- net as its output.  Returns the ID of the output net.
genCombLogic :: Seq A.NetId -> ExtractM A.NetId
genCombLogic inps = do
    out <- addNet $ A.Net "comb" 0 S.empty S.empty A.TUnknown ()
    addLogic $ A.Logic A.LkExpr
        (fmap (\n -> A.Pin n A.TUnknown) inps)
        (S.singleton $ A.Pin out A.TUnknown)
        ()
    return out

-- Create an input port connected to a fresh net.  Returns the ID of the net.
genInputPort :: Text -> ExtractM A.NetId
genInputPort name = do
    let ty = A.TUnknown
    netId <- addNet $ A.Net name 0 S.empty S.empty ty ()
    addThing (A.Port name netId ty) (_esCurModule . A._moduleInputs)
    return netId

-- Create an output port, connected to the provided net.
genOutputPort :: Text -> A.NetId -> ExtractM ()
genOutputPort name netId = do
    let ty = A.TUnknown
    addThing (A.Port name netId ty) (_esCurModule . A._moduleOutputs)
    return ()


isComputation (VReturn _) = True
isComputation (VBind _ _) = True
isComputation (VNamed _ _) = True
isComputation (VMkReg _) = True
isComputation (VMkModule _ _ _) = True
isComputation (VAddRules _) = True
isComputation (VRegWrite _ _) = True
isComputation _ = False

-- Run a monadic computation.  `handle` should implement the behavior of all
-- the primitive operations of the monad.  (`VBind` and `VReturn` are handled
-- internally.)
runMonad :: (Value -> ExtractM Value) -> Value -> ExtractM Value
runMonad handle c = go c
  where
    go c = case c of
        VReturn v -> return v
        VBind m k -> do
            -- Run computation `m` to get a value
            v <- go m
            -- Pass the value to `k` to get a new computation
            c' <- appValue k [] [v]
            -- Run the new computation
            go c'
        c -> handle c

-- Run a computation in the Module monad.
runModule :: Value -> ExtractM Value
runModule c = runMonad handleModule c

handleModule :: Value -> ExtractM Value
handleModule c = go Nothing c
  where
    go :: Maybe Text -> Value -> ExtractM Value
    go _ (VNamed name c) = go (Just name) c
    go optName (VMkReg _ty) = do
        name <- case optName of
            Just name -> return name
            Nothing -> do
                count <- S.length <$> use (_esCurModule . A._moduleLogics)
                return $ "_reg" <> T.pack (show count)
        let ty = A.TUnknown

        dNet <- addNet $ A.Net (name <> "$D") 0 S.empty S.empty ty ()
        qNet <- addNet $ A.Net (name <> "$Q") 0 S.empty S.empty ty ()
        addLogic $ A.Logic
            (A.LkDFlipFlop name 0)
            (A.Pin dNet ty <| S.empty)
            (A.Pin qNet ty <| S.empty)
            ()

        muxIdx <- addLogic $ A.Logic
            (A.LkRuleMux S.empty (name <| S.empty))
            S.empty
            (A.Pin dNet ty <| S.empty)
            ()

        return $ VDff muxIdx qNet
    go _ (VAddRules rs) = do
        forM_ rs $ \(RuleVal name conds body) ->
            -- TODO: do something with conds
            withRuleName name $ runAction body
        return VConst   -- returns unit
    go _ c = traceShow ("unsupported Module action", c) $ return VUnknown

runAction :: Value -> ExtractM Value
runAction c = runMonad handleAction c

netPin :: A.NetId -> ExtractM A.Pin
netPin netId = do
    ty <- use $ _esCurModule . A._moduleNet netId . A._netTy
    return $ A.Pin netId ty

handleAction :: Value -> ExtractM Value
handleAction c = go c
  where
    go :: Value -> ExtractM Value
    go (VRegWrite muxIdx val) = do
        traceM $ "handle regwrite: " ++ show (muxIdx, val)
        -- Add a new mux input for the current rule.
        name <- use _esCurRuleName
        netId <- case val of
            VNet netId -> return netId
            VConst -> genCombLogic S.empty
            _ ->
                trace ("bad value in VRegWrite: " ++ show val) $
                genCombLogic S.empty
        pin <- netPin netId

        zoom (_esCurModule . A._moduleLogic muxIdx) $ do
            A._logicKind %= \lk -> case lk of
                A.LkRuleMux rs ps -> A.LkRuleMux (rs |> name) ps
                _ -> error $ "VRegWrite muxIdx refers to non-mux?"
            A._logicInputs %= (|> pin)

        return VConst   -- reg write returns unit
    go c = traceShow ("unsupported Action action", c) $ return VUnknown
