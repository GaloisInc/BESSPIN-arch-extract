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
import BESSPIN.ArchExtract.BSV.PrintRaw
import BESSPIN.ArchExtract.Simplify (reconnectNets)


data ExtractState = ExtractState
    { esCurModule :: A.Module ()
    , esCurRuleName :: Text
    , esRuleCounter :: Int
    , esStructs :: Map Text Struct
    , esDefs :: Map Text Def
    , esModMap :: Map Text (A.ModId, Ty)
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

-- Split a function type into a list of type variables, a list of argument
-- types, and a return type.
splitFnTy :: Ty -> ([Id], [Ty], Ty)
splitFnTy (TForall tyVars ty') =
    let (vars', args', ret') = splitFnTy ty' in
    (tyVars ++ vars', args', ret')
splitFnTy (TArrow argTy ty') =
    let (vars', args', ret') = splitFnTy ty' in
    (vars', argTy : args', ret')
splitFnTy ty = ([], [], ty)

buildFnTy :: [Id] -> [Ty] -> Ty -> Ty
buildFnTy [] [] retTy = retTy
buildFnTy [] (argTy : argTys) retTy = TArrow argTy $ buildFnTy [] argTys retTy
buildFnTy tyVars argTys retTy = TForall tyVars $ buildFnTy [] argTys retTy

splitLambda :: Expr -> ([Pat], Expr)
splitLambda (ELam ps e') =
    let (pats', body') = splitLambda e' in
    (ps ++ pats', body')
splitLambda e = ([], e)

buildLambda :: [Pat] -> Expr -> Expr
buildLambda [] e = e
buildLambda ps e = ELam ps e

-- # Module processing
--
-- Modules come in two forms.
--
-- For modules with the `(* synthesize *)` attribute, `bsc` produces a pair of
-- definitions:
--
--      mkFoo :: forall m c. IsModule m c -> Params... -> m IFC
--      mkFoo <dict> = mkFoo @m @c <dict>
--
--      mkFoo- :: Params... -> Module IFC-
--      mkFoo- = \params ->
--          let mkFoo :: Params... -> Module IFC
--              mkFoo = \params... -> <module body>
--          in liftM ... mkFoo
--
-- For modules without `(* synthesize *)`, `bsc` produces only a single
-- definition:
--
--      mkFoo :: forall m c. IsModule m c -> Params... -> m IFC
--      mkFoo <dict> = \params... -> <module body>
--
-- For consistency, we standardize on the second form, except we replace the
-- return type `m IFC` with `Module IFC`.  For synthesized modules,
-- instantiations of the module call the `mkFoo` definition, not `mkFoo-`, so
-- the arguments passed at the call site are consistent with the second form.

-- Check if a `Def` is the dummy `mkFoo = mkFoo` definition generated for
-- synthesized modules.
isDummyDef (Def (Id name _ _) _ [Clause [PTcDict] e])
  | EApp (EVar (Id name' _ _)) [_, _] [ETcDict] <- e
  = name == T.takeWhileEnd (/= '.') name'
isDummyDef _ = False

convertModule :: Def -> Maybe BSVModule
convertModule d | isDummyDef d = Nothing
-- Synthesized module case.  This case is looking at the `mkFoo-` definition,
-- which needs adjustments to its type and value to get into the standard form.
convertModule (Def _ ty cs)
  | [Clause [] body] <- cs
  , (args, ELet (Def i ty' [Clause [] body']) _) <- splitLambda body
  , (tyVars, argTys, TModule ifcTy) <- splitFnTy ty'
  = Just $ BSVModule i
    (buildFnTy (iM : iC : tyVars) (dictTy : argTys) (TModule ifcTy))
    body'
  where
    iM = Id "_m__" 0 0
    iC = Id "_c__" 0 0
    dictTy = TIsModule (TVar iM) (TVar iC)
-- Non-synthesized module case.  The only adjustment required is changing the
-- return type.
convertModule (Def i ty cs)
  | [Clause [PTcDict] body] <- cs
  , (tyVars, argTys, TApp (TVar iM) [ifcTy]) <- splitFnTy ty
  , (TIsModule (TVar iM') (TVar _iC) : _) <- argTys
  , iM == iM'
  = Just $ BSVModule i
    (buildFnTy tyVars argTys (TModule ifcTy))
    body
convertModule _ = Nothing

isModDict iM (TIsModule (TVar iM') (TVar _)) = iM == iM'
isModDict _ _ = False

addPackageName :: Id -> BSVModule -> BSVModule
addPackageName (Id pkgName _ _) (BSVModule (Id name l c) t e) =
    BSVModule (Id (pkgName <> "." <> name) l c) t e

findPackageModules :: Package -> [BSVModule]
findPackageModules p = 
    map (addPackageName $ packageId p) $
    mapMaybe convertModule $
    toList $ packageDefs p

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
        , esStructs = structs
        , esDefs = defs
        , esModMap =
            traceShow ("known modules", M.keys modMap) $ modMap
        }
    archMods = S.fromList $ evalState (mapM extractModule bsvMods) initState

    structs = M.unions $ do
        p <- ps
        s <- toList $ packageStructs p
        return $ M.singleton (idName $ structId s) s

    defs = M.unions $ do
        p <- ps
        d <- toList $ packageDefs p
        return $ M.singleton (idName $ defId d) d

    modMap = M.fromList $
        zipWith (\(BSVModule (Id name _ _) ty _) idx -> (name, (idx, ty)))
            bsvMods [0..]

extractModule :: BSVModule -> ExtractM (A.Module ())
extractModule (BSVModule (Id name _ _) ty body) = do
    traceM (" --- extracting module " ++ show name ++ " ---")
    traceM (T.unpack $ printAny ty)
    traceM (T.unpack $ printAny body)
    traceM "---"
    let initMod = A.Module name S.empty S.empty S.empty S.empty S.empty S.empty
    m <- withModule initMod $ do
        v <- eval M.empty body

        (numTys, numVals) <- countArgs v
        when (numTys > 0) $ traceM $
            show name ++ ": can't provide type arguments to module body"
        let argVals = replicate numVals VConst

        v' <- if numVals > 0 then appValue v [] argVals else return v
        m <- runModule v'
        traceM (show ("module", name, "got value:", m))
        extractMethods m
        return ()
    return $
        reconnectNets $
        m

extractMethods :: Value -> ExtractM ()
extractMethods (VStruct _ fs) = do
    forM_ (M.toList fs) $ \(rawName, val) -> case val of
        -- If an interface contains a subinterface, the module structs will be
        -- nested.
        VStruct _ _ -> extractMethods val
        _ -> do
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

    goOutput v | traceShow ("goOutput: got", v, "for", name) False = undefined
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
funcArgNames (VModuleCtor _ _ _) =
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
    | VStruct Ty (Map Text Value)
    -- Struct field accessor function
    | VFieldAcc Text Text

    -- Function-like values.
    | VClosure [Pat] Scope Expr
    -- Partial application.
    | VPartApp Value [Ty] [Value]
    | VModuleCtor A.ModId Int Int
    | VModuleMethod Text Text
    | VPrim Prim

    -- Monadic computations.  These are processed by `runModule` and
    -- `runAction`.
    | VReturn Value
    | VBind Value Value
    -- Module monad primitives.
    | VNamed Text Value
    | VMkReg Ty
    | VMkModule A.ModId [Ty] [Value]
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

badEval msg = badEval' msg VUnknown
badEval' msg value = traceShow ("evaluation failure:", msg) value


-- Evaluate `e` in `sc`.  This lives in the `ExtractM` monad because some
-- evaluation steps produce nets, logics, etc. in the current module.  This
-- includes some `Expr`s that look pure in the source language, like `a & b`.
eval :: Scope -> Expr -> ExtractM Value
eval _ e | traceShow ("evaluate", e) False = undefined
eval sc (EVar i@(Id name _ _))
  | Just v <- M.lookup name sc = return v
  | otherwise = do
    optMod <- use $ _esModMap . at name
    optDef <- use $ _esDefs . at name

    case (optMod, optDef) of
        (Just (modId, ty), _) -> do
            let (tyVars, argTys, retTy) = splitFnTy ty
            return $ VModuleCtor modId (length tyVars) (length argTys)
        (Nothing, Just (Def _ _ [Clause [] e])) -> eval M.empty e
        (Nothing, Just (Def _ _ [Clause ps e])) -> eval M.empty $ ELam ps e
        (Nothing, Just (Def _ _ _)) -> return $
            badEval ("reference to multi-clause def (NYI)", name)
        (Nothing, Nothing) -> return $ badEval ("unknown variable", name)
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
    let go name = (name, badEval ("letrec binding (NYI)", name)) in
    let sc' = M.fromList $ map (go . idName . defId) ds in
    eval sc' e

eval sc (ELit _) = return VConst
-- ERules should be converted to EAddRules by RaiseRaw
eval sc (ERules _) = return $ badEval ("ERules unsupported")
eval sc (EStatic p f) = return $ badEval ("EStatic NYI", p, f)

eval sc (EStruct TUnit []) = return VConst
eval sc (EStruct (TCon _) []) = return VConst
eval sc (EStruct ty fs) = do
    fvs <- forM fs $ \(i, expr) -> do
        val <- eval sc expr
        return (idName i, val)
    return $ VStruct ty $ M.fromList fvs

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
eval sc (EUnknown cbor) = return $ badEval ("EUnknown", cbor)


evalRule sc (Rule name conds body) = do
    idx <- nextRuleIdx
    let name' = case name of
            Just n -> n
            Nothing -> "rule" <> T.pack (show idx)
    conds' <- mapM (eval sc) conds
    body' <- eval sc body
    return $ RuleVal name' conds' body'


-- Apply a value to (type and value) arguments.
appValue :: Value -> [Ty] -> [Value] -> ExtractM Value
appValue f tys vals | traceShow ("apply", f, tys, vals) False = undefined
appValue f tys [] | not $ isFunc f = return $
    badEval' ("value doesn't accept type arguments", f, tys) f
appValue f tys vals | not $ isFunc f = return $
    badEval ("value doesn't accept arguments", f, tys, vals)
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
          | not $ null moreVals -> return $
            badEval ("applied value to too few types and too many arguments",
                f, tys, vals)
          | otherwise ->
            -- Discard the excess type arguments and hope for the best.
            badEval' ("applied value to too many types and too few arguments",
                f, tys, vals)
                <$> appValue (mkPartApp f exactTys exactVals) [] moreVals

-- Like `appValue`, but the caller must provide exactly the right number of
-- `tys` and `vals`.
appExact f tys vals = case f of
    VClosure ps sc' body -> appClosure ps vals sc' body
    VPartApp f tys' vals' -> appExact f (tys' ++ tys) (vals' ++ vals)
    VPrim p -> appPrim p tys vals
    VModuleCtor modId _ _ -> return $ VMkModule modId tys vals
    -- Non-function values request no arguments, so we aren't really applying
    -- anything here.
    _ -> return f

mkPartApp (VPartApp f tys vals) tys' vals' =
    VPartApp f (tys ++ tys') (vals ++ vals')
mkPartApp f tys vals =
    VPartApp f tys vals

isFunc (VClosure _ _ _) = True
isFunc (VPartApp _ _ _) = True
isFunc (VModuleCtor _ _ _) = True
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
countArgs (VModuleCtor _ numTys numVals) = return (numTys, numVals)
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
    PMkReg -> (2, 1)
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
    PIf -> (1, 3)

appPrim :: Prim -> [Ty] -> [Value] -> ExtractM Value
appPrim PReturn [] [v] = return $ VReturn v
appPrim PBind [] [m, k] = return $ VBind m k
appPrim PMkReg [ty, _width] [_init] = return $ VMkReg ty
appPrim PMkRegU [ty, _width] [] = return $ VMkReg ty
appPrim PPack [] [v] = return v
appPrim PUnpack [] [v] = return v
appPrim PTruncate [] [v] = return v
appPrim PIndex [] [v, i] = return $ badEval ("PIndex NYI", v, i)
appPrim PRegRead [] [VDff _ qNetId] = return $ VNet qNetId
appPrim PRegWrite [] [VDff muxIdx _, v] = return $ VRegWrite muxIdx v
appPrim (PUnOp _) [] vs | Just inps <- collectNets vs =
    if S.null inps then return VConst else VNet <$> genCombLogic inps
appPrim (PBinOp _) [] vs | Just inps <- collectNets vs =
    if S.null inps then return VConst else VNet <$> genCombLogic inps
-- `if` in monadic code is NYI.  Non-monadic `if` generates a mux.
appPrim PIf [ty] vs
  | not $ isComputationType ty, Just inps <- collectNets vs =
    if S.null inps then return VConst else VNet <$> genCombLogic inps
appPrim (PSetName name) [] [c] = return $ VNamed name c
appPrim p tys vals = return $
    badEval ("bad arguments for primitive", p, tys, vals)

-- Process a list of values.  On `VNet`, produces the `NetId`.  On `VConst`,
-- produces nothing.  If any other value is present in the input, `collectNets`
-- returns `Nothing`.
collectNets :: [Value] -> Maybe (Seq A.NetId)
collectNets vs = go vs
  where
    go [] = Just S.empty
    go (VNet netId : vs) = (netId <|) <$> go vs
    go (VConst : vs) = go vs
    go (_ : vs) = Nothing

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

isComputationType (TModule _) = True
isComputationType _ = False

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
    go name comp | traceShow ("running (Module)", comp, name) False = undefined
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
    go optName (VMkModule modId tys vals) = do
        name <- case optName of
            Just name -> return name
            Nothing -> do
                count <- S.length <$> use (_esCurModule . A._moduleLogics)
                return $ "_mod" <> T.pack (show count)

        idx <- addLogic $ A.Logic
            -- TODO: handle params
            (A.LkInst $ A.Inst modId name S.empty)
            -- TODO: get method signatures from interface struct definition
            S.empty
            S.empty
            ()
        
        -- TODO: generate a rule mux for each method
        return $ VModInst idx
    go _ (VAddRules rs) = do
        forM_ rs $ \(RuleVal name conds body) ->
            -- TODO: do something with conds
            withRuleName name $ runAction body
        return VConst   -- returns unit
    go _ c = return $ badEval ("unsupported Module computation", c)

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
    go c = return $ badEval ("unsupported Action computation", c)
