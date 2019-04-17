{-# LANGUAGE OverloadedStrings, TemplateHaskell, RankNTypes #-}
module BESSPIN.ArchExtract.BSV.Extract where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
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

import qualified BESSPIN.ArchExtract.Config as Config
import BESSPIN.ArchExtract.Lens
import qualified BESSPIN.ArchExtract.Architecture as A
import BESSPIN.ArchExtract.BSV.Raw
import BESSPIN.ArchExtract.BSV.PrintRaw
import BESSPIN.ArchExtract.BSV.Interface
import BESSPIN.ArchExtract.Simplify (reconnectNets)


data ExtractState = ExtractState
    { esCurModule :: A.Module ()
    , esCurRuleName :: Text
    , esRuleCounter :: Int
    , esStructs :: Map Text Struct
    , esDefs :: Map Text Def
    , esModMap :: Map Text (A.ModId, Ty, IfcSpec)
    , esIfcSpecs :: Map Text IfcSpec
    -- Error messages from `badEval`
    , esErrors :: Seq Text
    , esNodeErrors :: Map Int (Seq Text)
    , esModuleErrors :: Seq Text
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

takeErrors :: ExtractM (Seq Text)
takeErrors = do
    errs <- use _esErrors
    _esErrors .= S.empty
    return errs

saveErrors :: NodeId -> Value -> ExtractM ()
saveErrors nid v = do
    errs <- takeErrors
    let msgs = errs |> T.pack ("result: " ++ show v)
    _esNodeErrors %= M.alter (\x -> Just $ maybe msgs (<> msgs) x) nid
    _esModuleErrors %= (<> errs)

saveErrors' :: NodeId -> ExtractM ()
saveErrors' nid = do
    errs <- takeErrors
    let msgs = errs
    _esNodeErrors %= M.alter (\x -> Just $ maybe msgs (<> msgs) x) nid
    _esModuleErrors %= (<> errs)

takeModuleErrors :: ExtractM (Seq Text)
takeModuleErrors = do
    errs <- use _esModuleErrors
    _esModuleErrors .= S.empty
    return errs


-- The Bool is True if this module came from a library package.
data BSVModule = BSVModule Id Ty Expr Bool

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

convertModule :: Bool -> Def -> Maybe BSVModule
-- Synthesized module case.  This case is looking at the `mkFoo-` definition,
-- which needs adjustments to its type and value to get into the standard form.
convertModule isLib (Def i0 ty cs)
  | [Clause [] body] <- cs
  , (_, body') <- splitLambda body
  , (ds@(_:_), _) <- splitLet body'
  , Def i ty' [Clause [] body''] <- last ds
  , (tyVars, argTys, TModule ifcTy) <- splitFnTy ty'
  = Just $ BSVModule (addPkgName i)
    (buildFnTy (iM : iC : tyVars) (dictTy : argTys) (TModule ifcTy))
    body''
    isLib
  where
    iM = Id "_m__" 0 0
    iC = Id "_c__" 0 0
    dictTy = TIsModule (TVar iM) (TVar iC)
    pkgName = fst $ T.breakOn "." $ idName i0
    addPkgName (Id name l c) = Id (pkgName <> "." <> name) l c
-- Synthesized module stub definition.  This is the `mkFoo` definition, which
-- simply calls `mkFoo-`.  We recognize it by checking if the body of the
-- definition is an `EApp` (for full non-synthesized modules, the body is
-- usually an `ELet` or `ELam` instead).
convertModule isLib (Def i ty cs)
  | [Clause dcts body] <- cs
  , EApp _ _ _ <- body
  = Nothing
-- Non-synthesized module case.  The only adjustment required is changing the
-- return type.
convertModule isLib (Def i ty cs)
  | [Clause dcts body] <- cs
  --, all (\p -> case p of PTcDict -> True; _ -> False) dcts
  , (tyVars, argTys, TApp (TVar iM) [ifcTy]) <- splitFnTy ty
  , any (isModDict iM) argTys
  = Just $ BSVModule i
    (buildFnTy tyVars argTys (TModule ifcTy))
    body
    isLib
convertModule _ _ = Nothing

isModDict iM (TIsModule (TVar iM') (TVar _)) = idName iM == idName iM'
isModDict _ _ = False

findPackageModules :: Config.BSV -> Package -> [BSVModule]
findPackageModules cfg p =
    mapMaybe (convertModule isLib) $
    toList $ packageDefs p
  where
    isLib = Set.member (idName $ packageId p) (Config.bsvLibraryPackages cfg)

returnedIfcName :: Ty -> Maybe Text
returnedIfcName ty
  | (_, _, ret) <- splitFnTy ty
  , traceShow ("rifcname", ret) True
  , TModule tIfc <- ret
  , (TIfc (Id name _ _), _) <- splitAppTy tIfc
  = Just name
  | otherwise = Nothing

data ExtractResult = ExtractResult
    { erDesign :: A.Design ()
    , erModuleErrors :: Map Text (Seq Text)
    , erNodeErrors :: Map Int (Seq Text)
    }
    deriving (Show)

extractDesign :: Config.BSV -> [Package] -> A.Design ()
extractDesign cfg ps = erDesign $ extractDesign' cfg ps

extractDesign' :: Config.BSV -> [Package] -> ExtractResult
extractDesign' cfg ps =
    ExtractResult (A.Design archMods) modErrs nodeErrs
  where
    bsvMods = concatMap (findPackageModules cfg) ps

    structs = M.unions $ do
        p <- ps
        s <- toList $ packageStructs p
        return $ M.singleton (idName $ structId s) s

    defs = M.unions $ do
        p <- ps
        d <- toList $ packageDefs p
        return $ M.singleton (idName $ defId d) d

    ifcSpecs = translateIfcStructs structs

    modIfc name ty = case returnedIfcName ty >>= flip M.lookup ifcSpecs of
        Nothing ->
            badCall' ("couldn't find interface for module", name, ty) dummyIfc
        Just ifc -> ifc

    modMap = M.fromList $
        zipWith (\(BSVModule (Id name _ _) ty _ _) idx ->
            (name, (idx, ty, modIfc name ty))) bsvMods [0..]

    initState = ExtractState
        { esCurModule = error "no current module"
        , esCurRuleName =
            badCall' "ran Action not in any rule" $
            "[[error: no rule]]"
        , esRuleCounter = 0
        , esStructs = structs
        , esDefs = defs
        , esModMap =
            trace ("known modules\n" ++ unlines (map (("  " ++) . show) $ M.toList modMap)) $
            modMap
        , esIfcSpecs =
            trace ("known ifcs\n" ++ unlines (map (("  " ++) . show) $ M.toList ifcSpecs)) $
            ifcSpecs
        , esErrors = S.empty
        , esNodeErrors = M.empty
        , esModuleErrors = S.empty
        }
    (extractedMods, finalState) = runState (mapM go bsvMods) initState
    archMods = S.fromList [m | (m,_,_) <- extractedMods]
    modErrs = M.fromList [(A.moduleName m, e <> fe) | (m,e,fe) <- extractedMods]
    nodeErrs = esNodeErrors finalState
    go m@(BSVModule (Id name _ _) ty _ _) = do
        m' <- extractModule m (modIfc name ty)
        finalErrs <- takeErrors
        errs <- takeModuleErrors
        return (m', errs, finalErrs)


extractModule :: BSVModule -> IfcSpec -> ExtractM (A.Module ())
extractModule (BSVModule (Id name _ _) ty body isLib) ifc = do
    traceM (" --- extracting module " ++ show name ++ " ---")
    traceM (T.unpack $ printAny ty)
    traceM (T.unpack $ printAny body)
    traceM "---"
    let kind = if isLib then A.MkExtern else A.MkNormal
    let initMod = A.Module name kind S.empty S.empty S.empty S.empty S.empty S.empty
    m <- withModule initMod $
        if not isLib then do
            v <- eval M.empty body

            (numTys, numVals) <- countArgs v
            when (numTys > 0) $ void $
                badEval ("can't provide type arguments to module body", name, v)
            let argVals = replicate numVals VConst

            v' <- if numVals > 0 then appValue v [] argVals else return v
            m <- runModule v'
            buildIfcPorts "" ifc m
        else do
            buildIfcPorts_ "" ifc
    return $
        reconnectNets $
        m


data Value =
    -- Run-time values are carried on nets.  Evaluating `ERegRead r` produces a
    -- `VNet` referencing `r`'s output (`Q`) net, while evaluating `a & b`
    -- generates a `Logic` and returns its output net.
      VNet A.NetId
    -- Compile-time constants, such as integer literals.  These can be used
    -- anywhere in place of a `VNet`.
    | VConst

    -- Delayed access to a global definition.  If a global refers to itself,
    -- the recursive reference evaluates to one of these.
    | VThunkGlobal Text
    -- Delayed function application.  Constructed by `appValue` when at least
    -- one input is a thunk.
    | VThunk Value [Ty] [Value]

    -- Function-like values.
    | VClosure [Pat] Scope Expr
    -- Partial application.  Much easier to have one unified way of handling
    -- partial applications, instead of making every single callable type
    -- handle it separately.
    | VPartApp Value [Ty] [Value]
    | VPrim Prim

    -- First arg is the index of the register's associated `LkRuleMux` in
    -- `moduleLogics`.  Second arg is the ID of its output net.
    | VDff Int A.NetId

    | VModuleCtor A.ModId (Int, Int) IfcSpec
    | VMethod
        (Int, Int)      -- number of type and value arguments
        Int             -- logic ID of the rule mux for the method's inputs
        (Maybe A.NetId) -- the output net, if any
    -- Combinational method.  Similar to `VMethod`, except combinational
    -- methods don't have rule muxes on their inputs.  Args: LkInst index,
    -- index of the first input port, index of the output port.
    | VCombMethod
        (Int, Int)      -- number of type and value arguments
        (Seq A.NetId)   -- input nets
        A.NetId         -- output net

    -- Structs are used to represent whole modules: a module definition in the
    -- source turns into a monadic computation returning a struct.
    | VStruct Ty (Map Text Value)
    -- Struct field accessor function.  We store only the field name
    -- (unqualified), to match the keys in `VStruct`.  The Int is the number of
    -- type arguments the accessor expects.
    | VFieldAcc Text Int

    -- Monadic computations.  These are processed by `runModule` and
    -- `runAction`.
    | VReturn Value
    | VBind Value Value Int
    -- Generic wrapper for monadic computations, represented as a function
    -- (whose final return type is `m a`) applied to type and value arguments.
    -- This lets us avoid having separate representations of, say, module
    -- constructors and the computations that they produce.
    | VCompute Value [Ty] [Value]
    -- Module monad primitives.  These are for special cases that don't fit
    -- into `VCompute`.
    | VNamed Text Value
    | VMkReg Ty
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

isThunk (VThunkGlobal _) = True
isThunk (VThunk _ _ _) = True
isThunk _ = False

data RuleVal = RuleVal Text [Value] Value
    deriving (Show)

type Scope = Map Text Value

badEval :: Show a => a -> ExtractM Value
badEval msg = badEval' msg VUnknown

truncateMessage n msg =
    let (keep, rest) = splitAt 1000 msg in
    keep ++ (if not $ null rest then " ... (message truncated)" else "")

badEval' :: Show a => a -> r -> ExtractM r
badEval' msg value = do
    _esErrors %= (|> T.pack msg')
    traceShow ("evaluation failure: " ++ msg') $ return value
  where
    msg' = truncateMessage 1000 (show msg)

badEval_ :: Show a => a -> ExtractM ()
badEval_ msg = badEval' msg ()

badCall' :: Show a => a -> r -> r
badCall' msg value = trace msg' value
  where
    msg' = "evaluation failure: " ++ truncateMessage 1000 (show msg)


-- Evaluate `e` in `sc`.  This lives in the `ExtractM` monad because some
-- evaluation steps produce nets, logics, etc. in the current module.  This
-- includes some `Expr`s that look pure in the source language, like `a & b`.
eval :: Scope -> Expr -> ExtractM Value
eval sc (EVar i@(Id name _ _))
  | Just v <- M.lookup name sc = return v
  | otherwise = do
    optMod <- use $ _esModMap . at name
    optDef <- use $ _esDefs . at name

    case (optMod, optDef) of
        (Just (modId, ty, ifc), _) -> do
            let (tyVars, argTys, retTy) = splitFnTy ty
            return $ VModuleCtor modId (length tyVars, length argTys) ifc
        (Nothing, Just d) -> return $ VThunkGlobal name
        (Nothing, Nothing) -> badEval ("unknown variable", name)
eval sc (ELam ps body) = return $ VClosure ps sc body
eval sc (EApp f tys args) = do
    fv <- eval sc f
    argvs <- mapM (eval sc) args
    appValue fv tys argvs
-- TODO: handling multi-clause defs will require multi-clause VClosure, and
-- associated changes to application & pattern matching.
eval sc (ELet (Def (Id name _ _) _ [Clause [] body]) e nid _) = do
    v <- eval sc body
    saveErrors nid v
    eval (M.insert name v sc) e
eval sc (ELet (Def (Id name _ _) _ [Clause ps body]) e nid _) = do
    let v = VClosure ps sc body
    saveErrors nid v
    eval (M.insert name v sc) e
eval sc (ELet (Def (Id name _ _) _ cs) e nid _) = do
    v <- badEval ("multi-clause let binding NYI", name)
    saveErrors nid v
    eval (M.insert name v sc) e
-- TODO: letrec is totally unsupported at the moment.  Not sure how hard this
-- would be to implement, but it doesn't seem to be used very often.
eval sc (ELetRec ds e) = do
    bnds <- liftM M.fromList $ forM ds $ \d -> do
        let name = idName $ defId d
        v <- badEval ("letrec binding NYI", name)
        return (name, v)
    eval (bnds <> sc) e

eval sc (ELit _) = return VConst
-- ERules should be converted to EAddRules by RaiseRaw
eval sc (ERules _) = badEval ("ERules unsupported")
eval sc (EStatic (Id p _ _) (Id f _ _)) = do
    s <- use $ _esStructs . at p
    numTys <- case s of
        Nothing -> badEval' ("saw accessor for unknown struct", p) 0
        Just s -> return $ length $ structTyParams s
    return $ VFieldAcc (lastWord f) numTys

eval sc (EStruct TUnit []) = return VConst
eval sc (EStruct (TCon _) []) = return VConst
eval sc (EStruct ty fs) = do
    fvs <- forM fs $ \(i, expr) -> do
        val <- eval sc expr
        return (lastWord $ idName i, val)
    return $ VStruct ty $ M.fromList fvs

eval sc (EPrim p) = return $ VPrim p
eval sc (EDo ss e) = case ss of
    [] -> eval sc e
    s : ss' ->  do
        let (p, m, sid) = case s of
                SBind p _ m sid -> (p, m, sid)
                SBind' m sid -> (PWild, m, sid)
                SNote _ -> (PWild, EConst "SNote", 0)
        mv <- eval sc m
        let kv = VClosure [p] sc (EDo ss' e)
        return $ VBind mv kv sid
eval sc (EAddRules rs) = do
    rvs <- mapM (evalRule sc) rs
    return $ VAddRules rvs
eval sc ETcDict = return VTcDict
eval sc (EConst _) = return VConst
eval sc (EUnknown cbor) = badEval ("EUnknown", cbor)

evalDef name d = case d of
    Def _ _ [Clause [] e] -> eval M.empty e
    Def _ _ [Clause ps e] -> eval M.empty $ ELam ps e
    Def _ _ _ -> badEval ("reference to multi-clause def (NYI)", name)

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
appValue f tys vals | isThunk f || any isThunk vals =
    return $ VThunk f tys vals
-- Should probably investigate *why* we're seeing `EApp`s with no arguments,
-- but it's easy enough to ignore for now...
appValue f [] [] | not $ isFunc f = return f
appValue f tys [] | not $ isFunc f =
    badEval' ("value doesn't accept type arguments", f, tys) f
appValue f tys vals | not $ isFunc f =
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
          | not $ null moreVals ->
            badEval ("applied value to too few types and too many arguments",
                f, tys, vals)
          | otherwise -> do
            -- Discard the excess type arguments and hope for the best.
            badEval_ ("applied value to too many types and too few arguments",
                f, tys, vals)
            appValue (mkPartApp f exactTys exactVals) [] moreVals

-- Like `appValue`, but the caller must provide exactly the right number of
-- `tys` and `vals`.
appExact f tys vals = case f of
    VClosure ps sc' body -> appClosure ps vals sc' body
    VPartApp f tys' vals' -> appExact f (tys' ++ tys) (vals' ++ vals)
    VPrim p -> appPrim p tys vals
    VModuleCtor _ _ _ -> return $ VCompute f tys vals
    VFieldAcc name _ -> case vals of
        [VStruct _ fs]
          | Just v <- M.lookup name fs -> return v
          | otherwise -> badEval ("unknown struct field", name, fs)
        _ -> badEval ("expected exactly one struct argument", f, vals)
    VMethod _ _ _ -> return $ VCompute f tys vals
    VCombMethod _ inNets outNet -> do
        argNets <- mapM asNet vals
        forM_ (zip argNets (toList inNets)) $ \(a,b) ->
            void $ mkNetAlias' a b >>= addLogic
        return $ VNet outNet
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
isFunc (VFieldAcc _ _) = True
isFunc (VMethod _ _ _) = True
isFunc (VCombMethod _ _ _) = True
isFunc _ = False

-- Count the number of type and value arguments that `v` expects to receive.
-- Returns (0,0) for non-functions.
countArgs :: Value -> ExtractM (Int, Int)
countArgs (VClosure ps _ _) = return (0, length ps)
countArgs (VPartApp f tys vals) = do
    (numTys, numVals) <- countArgs f
    return (numTys - length tys, numVals - length vals)
countArgs (VPrim p) = return $ countArgsPrim p
countArgs (VModuleCtor _ numArgs _) = return numArgs
countArgs (VFieldAcc _ numTys) = return (numTys, 1)
countArgs (VMethod numArgs _ _) = return numArgs
countArgs (VCombMethod numArgs _ _) = return numArgs
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
        _ -> badEval' ("passed non-VTcDict", v, "to PTcDict argument", p) sc
    PUnknown _ ->
        badEval' ("tried to match", v, "against unknown pattern") sc

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
    PSlice -> (0, 3)
    PRegRead -> (0, 1)
    PRegWrite -> (0, 2)
    PUnOp _ -> (0, 1)
    PBinOp _ -> (0, 2)
    PSetName _ -> (0, 1)
    PIf -> (1, 3)

appPrim :: Prim -> [Ty] -> [Value] -> ExtractM Value
appPrim PReturn [] [v] = return $ VReturn v
appPrim PBind [] [m, k] = return $ VBind m k 0
appPrim PMkReg [ty, _width] [_init] = return $ VMkReg ty
appPrim PMkRegU [ty, _width] [] = return $ VMkReg ty
appPrim PPack [] [v] = return v
appPrim PUnpack [] [v] = return v
appPrim PTruncate [] [v] = return v
appPrim PIndex [] vs@[v, i] | Just inps <- collectNets vs =
    if S.null inps then return VConst else VNet <$> genCombLogic inps
appPrim PSlice [] vs@[v, hi, lo] | Just inps <- collectNets vs =
    if S.null inps then return VConst else VNet <$> genCombLogic inps
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
appPrim p tys vals =
    badEval ("bad arguments for primitive", p, tys, vals)


force :: Value -> ExtractM Value
force v = do
    traceM $ "BEGIN forcing " ++ show v
    v' <- force' v
    traceM $ "END forcing " ++ show v
    return v'

force' :: Value -> ExtractM Value
force' (VThunk f ty args) = do
    f <- force f
    args <- mapM force args
    v <- appValue f ty args
    force v
force' (VThunkGlobal name) = do
    optDef <- use $ _esDefs . at name
    case optDef of
        Just d -> evalDef name d
        Nothing -> badEval ("VThunkGlobal refers to unknown definition", name)
force' v = return v


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

asNet :: Value -> ExtractM A.NetId
asNet (VNet n) = return n
asNet VConst = addNet $ A.mkNet "const" (-1) A.TUnknown
asNet v = do
    badEval_ ("expected a net or const", v)
    asNet VConst

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

-- Add a new rule access to `muxIdx`, with `nets` as its input nets.  The rule
-- name is taken from `esCurRuleName`.
accessRuleMux :: Int -> [A.NetId] -> ExtractM ()
accessRuleMux muxIdx nets = do
    ruleName <- use _esCurRuleName
    pins <- S.fromList <$> mapM netPin nets
    zoom (_esCurModule . A._moduleLogic muxIdx) $ do
        A._logicInputs %= (<> pins)
        A._logicKind %= \lk -> case lk of
            A.LkRuleMux rs ps -> A.LkRuleMux (rs |> ruleName) ps
            _ -> error $ "expected RuleMux"

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
isComputation (VBind _ _ _) = True
isComputation (VCompute _ _ _) = True
isComputation (VNamed _ _) = True
isComputation (VMkReg _) = True
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
    go c = force c >>= \c -> case c of
        VReturn v -> return v
        VBind m k sid -> do
            -- Run computation `m` to get a value
            v <- go m
            when (sid /= 0) $ saveErrors sid v
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
    go _ (VNamed name c) = runMonad (go (Just name)) c
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
    go optName (VCompute (VModuleCtor modId _ ifc) tys vals) = do
        name <- case optName of
            Just name -> return name
            Nothing -> do
                count <- S.length <$> use (_esCurModule . A._moduleLogics)
                return $ "_mod" <> T.pack (show count)

        buildModule name modId ifc
    go _ (VAddRules rs) = do
        forM_ rs $ \(RuleVal name conds body) ->
            -- TODO: do something with conds
            withRuleName name $ runAction body
        return VConst   -- returns unit
    go _ c = badEval ("unsupported Module computation", c)

buildModule :: Text -> A.ModId -> IfcSpec -> ExtractM Value
buildModule name modId ifc = do
    -- Create the module itself, with no pins.  The pins are added
    -- incrementally inside `buildIfcLogic`.
    instId <- addLogic $ A.Logic
        (A.LkInst $ A.Inst modId name S.empty)
        S.empty S.empty ()

    buildIfcLogic (name <> ".") instId ifc

-- Generate the logic, nets, etc. for each method in `ifc`, including nested
-- methods.  Generated names are prefixed with `prefix` (useful for qualifying
-- the interface's method names).  Returns a `VStruct` of `VMethod` (or
-- `VCombMethod`) objects representing the interface.
buildIfcLogic :: Text -> Int -> IfcSpec -> ExtractM Value
buildIfcLogic prefix instId ifc = do
    kvs <- forM (ifcEntries ifc) $ \(name, entry) -> do
        v <- buildIfcItemLogic (prefix <> name) instId (ieItem entry)
        return (name, v)

    -- TODO: get the interface name so we can use `TIfc` here
    return $ VStruct TUnit (M.fromList kvs)

buildIfcItemLogic :: Text -> Int -> IfcItem -> ExtractM Value
buildIfcItemLogic qualName instId (IiSubIfc ifc) =
    buildIfcLogic (qualName <> ".") instId ifc
buildIfcItemLogic qualName instId (IiMethod m) = do
    inNets <- forM (imArgNames m) $ \argName ->
        addNet $ A.mkNet (qualName <> "." <> argName) 0 A.TUnknown
    outNets <- forM [0 .. methodOutPorts m - 1] $ \_ ->
        addNet $ A.mkNet (qualName <> ".return") 0 A.TUnknown

    inPins <- S.fromList <$> mapM netPin inNets
    outPins <- S.fromList <$> mapM netPin outNets
    zoom (_esCurModule . A._moduleLogic instId) $ do
        A._logicInputs %= (<> inPins)
        A._logicOutputs %= (<> outPins)

    case imKind m of
        MkComb
          | imArgCounts m /= (0, 0) -> return $
            VCombMethod (imArgCounts m) (S.fromList inNets) (head outNets)
          | otherwise -> return $
            VNet $ head outNets
        MkAction hasResult -> do
            muxIdx <- addLogic $ A.Logic
                (A.LkRuleMux S.empty (S.fromList $ imArgNames m))
                S.empty inPins ()
            return $ VMethod (imArgCounts m)
                muxIdx
                (if hasResult then Just $ head outNets else Nothing)

runAction :: Value -> ExtractM Value
runAction c = runMonad handleAction c

netPin :: A.NetId -> ExtractM A.Pin
netPin netId = zoom (_esCurModule . A._moduleNet netId) $ do
    ty <- use A._netTy
    return $ A.Pin netId ty

netPort :: A.NetId -> ExtractM A.Port
netPort netId = zoom (_esCurModule . A._moduleNet netId) $ do
    name <- use A._netName
    ty <- use A._netTy
    return $ A.Port name netId ty

mkNetAlias' :: A.NetId -> A.NetId -> ExtractM (A.Logic ())
mkNetAlias' a b = do
    a' <- netPin a
    b' <- netPin b
    return $ A.mkNetAlias a' b'

handleAction :: Value -> ExtractM Value
handleAction c = go c
  where
    go :: Value -> ExtractM Value
    go (VRegWrite muxIdx val) = do
        -- Add a new mux input for the current rule.
        name <- use _esCurRuleName
        netId <- case val of
            VNet netId -> return netId
            VConst -> genCombLogic S.empty
            _ -> do
                badEval_ ("bad value in VRegWrite: " ++ show val)
                genCombLogic S.empty

        accessRuleMux muxIdx [netId]

        return VConst   -- reg write returns unit
    go (VCompute (VMethod _ muxIdx optOutNet) _ vals) = do
        argNets <- mapM asNet vals
        accessRuleMux muxIdx argNets
        return $ maybe VConst VNet optOutNet
    go c = badEval ("unsupported Action computation", c)



-- Generate input/output ports for each method in `ifc`, and connect them up to
-- the implementations in `impl`.  Generated port names are prefixed with
-- `prefix`.
buildIfcPorts :: Text -> IfcSpec -> Value -> ExtractM ()
buildIfcPorts prefix ifc impl = do
    vals <- case impl of
        VStruct _ x -> return x
        _ -> badEval' ("interface impl is not a struct", impl, ifc) M.empty
    forM_ (ifcEntries ifc) $ \(name, entry) -> do
        v <- case M.lookup name vals of
            Just x -> return x
            Nothing -> badEval ("interface impl is missing an entry", name, vals, ifc)
        buildIfcItemPorts (prefix <> name) (ieItem entry) v

buildIfcItemPorts :: Text -> IfcItem -> Value -> ExtractM ()
buildIfcItemPorts qualName (IiSubIfc ifc) impl =
    buildIfcPorts (qualName <> ".") ifc impl
buildIfcItemPorts qualName (IiMethod m) impl = do
    inNets <- forM (imArgNames m) $ \argName ->
        addNet $ A.mkNet (qualName <> "." <> argName) 0 A.TUnknown
    outNets <- forM [0 .. methodOutPorts m - 1] $ \_ ->
        addNet $ A.mkNet (qualName <> ".return") 0 A.TUnknown

    inPorts <- S.fromList <$> mapM netPort inNets
    outPorts <- S.fromList <$> mapM netPort outNets
    _esCurModule . A._moduleInputs %= (<> inPorts)
    _esCurModule . A._moduleOutputs %= (<> outPorts)

    case imKind m of
        MkComb -> do
            val <- if hasArgs then appValue impl [] (map VNet inNets)
                else return impl
            case val of
                VNet valNet -> void $ mkNetAlias' valNet (head outNets) >>= addLogic
                VConst -> return ()
                _ -> badEval_ ("combinational method returned non-logic value", val)

        MkAction hasResult -> do
            comp <- if hasArgs then appValue impl [] (map VNet inNets)
                else return impl
            val <- withRuleName qualName $ runAction comp
            when hasResult $ case val of
                VNet valNet -> void $ mkNetAlias' valNet (head outNets) >>= addLogic
                VConst -> return ()
                _ -> badEval_ ("action method returned non-logic value", val)
  where
    hasArgs = not $ null $ imArgNames m



-- Similar to `buildIfcPorts`, but doesn't connect anything to the new ports.
buildIfcPorts_ :: Text -> IfcSpec -> ExtractM ()
buildIfcPorts_ prefix ifc = do
    forM_ (ifcEntries ifc) $ \(name, entry) -> do
        buildIfcItemPorts_ (prefix <> name) (ieItem entry)

buildIfcItemPorts_ :: Text -> IfcItem -> ExtractM ()
buildIfcItemPorts_ qualName (IiSubIfc ifc) =
    buildIfcPorts_ (qualName <> ".") ifc
buildIfcItemPorts_ qualName (IiMethod m) = do
    inNets <- forM (imArgNames m) $ \argName ->
        addNet $ A.mkNet (qualName <> "." <> argName) 0 A.TUnknown
    outNets <- forM [0 .. methodOutPorts m - 1] $ \_ ->
        addNet $ A.mkNet (qualName <> ".return") 0 A.TUnknown

    inPorts <- S.fromList <$> mapM netPort inNets
    outPorts <- S.fromList <$> mapM netPort outNets
    _esCurModule . A._moduleInputs %= (<> inPorts)
    _esCurModule . A._moduleOutputs %= (<> outPorts)



-- Get the last dot-separated word from `t`.
lastWord :: Text -> Text
lastWord t = T.takeWhileEnd (/= '.') t
