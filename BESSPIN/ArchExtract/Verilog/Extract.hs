{-# LANGUAGE OverloadedStrings, TemplateHaskell, Rank2Types #-}
module BESSPIN.ArchExtract.Verilog.Extract where

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

import BESSPIN.ArchExtract.Architecture hiding (moduleNets)
import qualified BESSPIN.ArchExtract.Architecture as A
import BESSPIN.ArchExtract.Verilog.AST
import qualified BESSPIN.ArchExtract.Verilog.AST as V
import BESSPIN.ArchExtract.Verilog.Match
import BESSPIN.ArchExtract.Verilog.TypeCheck
import BESSPIN.ArchExtract.Simplify hiding (mkNet)
import BESSPIN.ArchExtract.Lens
import BESSPIN.ArchExtract.Constraints (shiftExpr, shiftTy)


-- Extraction state definition

-- Must be declared at the top, before `makeLenses'` invocation.
data NetOrigin =
    NoDecl { noDeclId :: Int } |
    -- noDeclId: index of the `InstDecl` in `moduleDecls`
    -- noPortIdx: index of the port in the instantiated module's `moduleDecls`
    NoInstPort { noDeclId :: Int, noPortIdx :: Int }
    deriving (Show, Eq, Ord)

data ModSig = ModSig
    { msInputs :: Seq A.Port
    , msOutputs :: Seq A.Port
    , msParams :: Seq A.Param
    -- For each `A.Port` in `A.moduleInputs`, give the "index of the index" in
    -- `V.modulePorts`.  That is, if `msInputOrigin` maps `a` to `v`, then
    -- there exists a `d` such that `V.modulePorts` maps `v` to `d` and the
    -- `V.PortDecl` at `V.moduleDecls ! d` produced `A.moduleInputs ! a`.
    , msInputOrigin :: Seq Int
    , msOutputOrigin :: Seq Int
    , msParamOrigin :: Seq Int
    }
    deriving (Show)

data ExtractState = ExtractState
    { esModule :: A.Module ()
    , esDeclNets :: Map NetOrigin (NetId, A.Ty)
    -- Maps the index of the `V.ParamDecl` in `V.moduleDecls` to the index of
    -- the `A.Param` in `A.moduleParams`.
    , esDeclParams :: Map Int Int
    , esParamOrigin :: Seq Int
    , esInputOrigin :: Seq Int
    , esOutputOrigin :: Seq Int
    -- Only the name, params, inputs, and outputs are populated on the modules
    -- in `esModSigs`.
    , esModSigs :: Seq ModSig
    }
    deriving (Show)

makeLenses' ''ExtractState

type ExtractM a = State ExtractState a

addThing :: a -> Lens' ExtractState (Seq a) -> ExtractM Int
addThing x fld = do
    idx <- S.length <$> use fld
    fld %= (|> x)
    return idx

-- Add `A.Param` `x`, originating from `V.moduleParams ! i`.  Returns the index
-- of the new entry in `A.moduleParams`.
addParam i x = do
    addThing i $ _esParamOrigin
    addThing x $ _esModule . _moduleParams
addInput i x = do
    addThing i $ _esInputOrigin
    addThing x $ _esModule . _moduleInputs
addOutput i x = do
    addThing i $ _esOutputOrigin
    addThing x $ _esModule . _moduleOutputs
addLogic x = addThing x $ _esModule . _moduleLogics
addNet x = NetId <$> (addThing x $ _esModule . _moduleNets)

addDeclNet no x = do
    id <- addNet x
    _esDeclNets %= M.insert no (id, A.netTy x)
    return id

freshNet' baseName ty = do
    idx <- gets $ S.length . A.moduleNets . esModule
    let name = baseName <> "$" <> T.pack (show idx)
    addNet $ Net name prioFresh S.empty S.empty ty ()

freshNet ty = freshNet' "tmp" ty

addDeclParam declId idx x = do
    id <- addParam idx x
    _esDeclParams %= M.insert declId id
    return id

setParamDefault :: Int -> ConstExpr -> ExtractM ()
setParamDefault i e = do
    _esModule . _moduleParams . ix i . _paramDefault .= Just e

nextLogicIdx :: ExtractM Int
nextLogicIdx = S.length <$> use (_esModule . _moduleLogics)

mkNet :: Text -> Int -> A.Ty -> A.Net ()
mkNet name prio ty = Net name prio S.empty S.empty ty ()


lookupNet :: NetOrigin -> ExtractM (Maybe (NetId, A.Ty))
lookupNet no = use $ _esDeclNets . at no


findNet :: NetOrigin -> ExtractM (NetId, A.Ty)
--findNet no = use $ _esDeclNets . singular (ix no)
findNet no = do
    nets <- use _esDeclNets
    case M.lookup no nets of
        Nothing -> error $ "no net for origin " ++ show no
        Just x -> return x

findNet' :: NetOrigin -> ExtractM (A.Net ())
findNet' no = do
    (netId, _) <- findNet no
    use $ _esModule . _moduleNet netId

findNetName :: NetOrigin -> ExtractM Text
findNetName no = A.netName <$> findNet' no

findParam :: Int -> ExtractM Int
findParam declId = use $ _esDeclParams . singular (ix declId)

findModSig :: Int -> ExtractM ModSig
findModSig i = use $ _esModSigs . singular (ix i)


-- Top level

extractArch :: V.Design -> A.Design ()
extractArch vDes =
    -- Run cleanup on each constructed module.
    mapMods (\mod ->
        mergeAliasedNets $
        -- Break up "uninteresting" nets early, before they can get merged with
        -- other nets.
        disconnect (\_ _ _ net ->
            let baseName = last $ T.splitOn (T.singleton '.') (netName net) in
            not $ baseName `elem` map T.pack ["clock", "clk", "reset"]) $
        reconnectNets $
        mod) $
    A.Design mods
  where
    vMods = V.designModules vDes
    -- (1) For each module, convert signature information only (ports, params).
    --     Save each module's `ExtractState` from this step.
    -- (2) Collect the full set of module signatures from the intermediate
    --     states.
    -- (3) For each module, convert the rest of it, resuming from the saved
    --     `ExtractState`, but with the full set of signatures available.
    sigStates = fmap buildModSig vMods
    modSigs = fmap stateSig sigStates
    mods = fmap (buildMod modSigs) $ S.zip vMods sigStates


-- Module signatures

initMod name = A.Module
    { A.moduleName = name
    , A.moduleParams = S.empty
    , A.moduleInputs = S.empty
    , A.moduleOutputs = S.empty
    , A.moduleLogics = S.empty
    , A.moduleNets = S.empty
    , A.moduleConstraints = S.empty
    }

buildModSig :: V.Module -> ExtractState
buildModSig vMod = execState go initState
  where
    initState = ExtractState
        (initMod $ V.moduleName vMod)
        M.empty M.empty
        S.empty S.empty S.empty
        (error "mod sigs are not yet available")
    go :: ExtractM ()
    go = do
        convParams vMod
        convPorts vMod

stateSig :: ExtractState -> ModSig
stateSig es = ModSig
    { msInputs = A.moduleInputs $ esModule es
    , msOutputs = A.moduleOutputs $ esModule es
    , msParams = A.moduleParams $ esModule es
    , msParamOrigin = esParamOrigin es
    , msInputOrigin = esInputOrigin es
    , msOutputOrigin = esOutputOrigin es
    }


-- Sig: params

-- Add all declared parameters from `vMod` to `esModule`.
convParams :: V.Module -> ExtractM ()
convParams vMod = do
    -- First, add declarations for all params, but leave the initializers
    -- blank.
    void $ flip S.traverseWithIndex (V.moduleParams vMod) $ \idx declId ->
        let ParamDecl name _ _ = V.moduleDecls vMod `S.index` declId in
        void $ addDeclParam declId idx $ A.Param name Nothing

    -- Now we can translate the initializer expressions, including ones that
    -- refer to othe parameters.
    forM_ (V.moduleParams vMod) $ \idx ->
        let decl = V.moduleDecls vMod `S.index` idx in
        case decl of
            ParamDecl _ _ (Just init) -> do
                i <- findParam idx
                e <- convConstExpr init
                setParamDefault i e
            _ -> return ()

-- Convert all ports, returning a sequence of (inIdx, outIdx) pairs, giving the
-- indices of the converted ports in `moduleInputs` and `moduleOutputs`.
convPorts :: V.Module -> ExtractM ()
convPorts vMod = do
    void $ flip S.traverseWithIndex (V.modulePorts vMod) $ \idx declId -> do
        let V.PortDecl name vTy dir = V.moduleDecls vMod `S.index` declId
        ty <- convTy vMod vTy
        netId <- addDeclNet (NoDecl declId) (mkNet name prioExtPort ty)
        let port = A.Port name netId ty
        case dir of
            Input -> addInput idx port
            Output -> addOutput idx port
            InOut -> addInput idx port >> addOutput idx port



buildMod :: Seq ModSig -> (V.Module, ExtractState) -> A.Module ()
buildMod modSigs (vMod, prevState) = esModule $ execState go initState
  where
    initState = prevState { esModSigs = modSigs }

    go :: ExtractM ()
    go = do
        convDecls vMod
        convItems vMod

convDecls :: V.Module -> ExtractM ()
convDecls vMod =
    void $ flip S.traverseWithIndex (V.moduleDecls vMod) $ \idx decl -> case decl of
        V.PortDecl {} -> return ()
        V.ParamDecl {} -> return ()
        V.VarDecl name vTy -> do
            ty <- convTy vMod vTy
            void $ addDeclNet (NoDecl idx) (mkNet name prioWire ty)
        V.TypedefDecl {} -> return ()
        V.InstDecl instName modId paramVals -> do
            instSig <- findModSig modId
            aLogicIdx <- nextLogicIdx
            let zippedInputs = S.zip (msInputs instSig) (msInputOrigin instSig)
            inPins <- forM zippedInputs $ \(port, vIdx) -> do
                let name = instName <> "." <> A.portName port
                let ty = shiftTy aLogicIdx $ A.portTy port
                netId <- addDeclNet (NoInstPort idx vIdx) (mkNet name prioInstPort ty)
                return $ Pin netId ty
            let zippedOutputs = S.zip (msOutputs instSig) (msOutputOrigin instSig)
            outPins <- forM zippedOutputs $ \(port, vIdx) -> do
                let name = instName <> "." <> A.portName port
                let ty = shiftTy aLogicIdx $ A.portTy port
                netId <- addDeclNet (NoInstPort idx vIdx) (mkNet name prioInstPort ty)
                return $ Pin netId ty
            params <- forM (msParamOrigin instSig) $ \vIdx ->
                mapM convConstExpr $ join $ paramVals S.!? vIdx
            void $ addLogic $
                Logic (LkInst $ Inst modId instName params) inPins outPins ()

convItems :: V.Module -> ExtractM ()
convItems vMod =
    forM_ (V.moduleItems vMod) $ \item -> case item of
        V.InitVar declId e -> doAssign (Var declId) e
        V.InitInst declId portConns -> do
            let V.InstDecl _ modId _ = V.moduleDecls vMod `S.index` declId
            instSig <- findModSig modId
            void $ flip S.traverseWithIndex (msInputOrigin instSig) $ \aIdx vIdx -> do
                let vExpr = portConns `S.index` vIdx
                rPin <- rvalPin vExpr
                lPin <- netPin $ NoInstPort declId vIdx
                addNetAlias lPin rPin
            void $ flip S.traverseWithIndex (msOutputOrigin instSig) $ \aIdx vIdx -> do
                let vExpr = portConns `S.index` vIdx
                lPin <- lvalPin vExpr
                rPin <- netPin $ NoInstPort declId vIdx
                addNetAlias lPin rPin
        V.ContAssign l r -> doAssign l r
        V.Always {} | Right dffs <- inferDFlipFlop item -> forM_ dffs $ \dff -> do
            clkPin <- rvalPin $ dffClk dff
            dPin <- rvalPin $ dffD dff
            arstPins <- mapM rvalPin $ S.fromList $ dffAsyncResets dff
            qPin <- netPin $ NoDecl $ dffQ dff
            name <- findNetName $ NoDecl $ dffQ dff
            addLogic $ Logic
                (LkDFlipFlop name $ S.length arstPins)
                (dPin <| clkPin <| arstPins)
                (S.singleton qPin)
                ()
        V.Always evts body | any (isJust . eventEdge) evts -> do
            let assigns = foldMap stmtAssigns body
            let regMap = M.unionsWith (<>) $
                    concatMap (\(l,r,imp) ->
                        let (llVars, lrVars) = lvalVars l in
                        let rVars = exprVars r in
                        map (\lv -> M.singleton lv (Set.fromList $ lrVars <> rVars <> imp))
                            llVars
                    ) assigns
            mapM_ (\(lv, rvs) -> do
                    name <- findNetName $ NoDecl lv
                    lPin <- netPin $ NoDecl lv
                    rPins <- netPins $ map NoDecl $ Set.toList rvs
                    addLogic $ Logic (LkRegister name) rPins (S.singleton lPin) ()
                ) (M.toList regMap)
        V.Always _ body -> mapM_ convStmt body
        V.Initial body -> mapM_ convStmt body

  where
    doAssign l r = do
        lPin <- lvalPin l
        rPin <- rvalPin r
        void $ addNetAlias lPin rPin

addNetAlias lPin rPin =
    addLogic $ Logic LkNetAlias (S.singleton rPin) (S.singleton lPin) ()

-- Get a single pin representing the output of an expression.  Generates a new
-- `Logic` and `Net` if needed.
rvalPin :: Expr -> ExtractM Pin
rvalPin (Var v) = lookupNet (NoDecl v) >>= \nt -> case nt of
    Just (net, ty) -> return $ Pin net ty
    -- If there's no net for this var, it must be a `ParamDecl`.  Treat it as
    -- an unknown expression, which will generate a 0-input `Logic`.
    Nothing -> rvalPin UnknownExpr
rvalPin e = do
    ty <- typeofExpr e
    inPins <- netPins $ exprVarDecls e
    outNet <- freshNet ty
    addLogic $ Logic LkOther inPins (S.singleton $ Pin outNet ty) ()
    return $ Pin outNet ty

lvalPin :: Expr -> ExtractM Pin
lvalPin (Var v) = lookupNet (NoDecl v) >>= \nt -> case nt of
    Just (net, ty) -> return $ Pin net ty
    Nothing -> lvalPin UnknownExpr
lvalPin e = do
    ty <- typeofExpr e
    inNet <- freshNet ty
    -- TODO: this is pretty imprecise
    outPins <- netPins $ exprVarDecls e
    addLogic $ Logic LkOther (S.singleton $ Pin inNet ty) outPins ()
    return $ Pin inNet ty

-- Get the declId of every variable used in `e`.
exprVars :: Expr -> [Int]
exprVars e = everything (<>) ([] `mkQ` go) e
  where
    go (Var declId) = [declId]
    go _ = []

gExprVars :: Data a => a -> [Int]
gExprVars x = everything (<>) ([] `mkQ` exprVars) x

-- Like `exprVars`, but separates variables that would be modified if `e` were
-- used as an lvalue from variables that would only be read (for example,
-- memory addresses).
lvalVars :: Expr -> ([Int], [Int])
lvalVars e = go e
  where
    go (V.Var declId) = ([declId], [])
    go (V.Param _) = ([], [])
    go (V.Index base idx) = go base <> ([], gExprVars idx)
    go (V.MemIndex base idxs) = go base <> ([], gExprVars idxs)
    go (V.Const _) = ([], [])
    go (V.ConstInt _ _) = ([], [])
    go (V.ConstBool _ _) = ([], [])
    go (V.Concat es) = mconcat $ map go es
    go (V.MultiConcat _ es) = mconcat $ map go es
    go (V.Field base _) = go base
    go (V.AssignPat _ es) = mconcat $ map go es
    go V.UnknownExpr = ([], [])
    go e = error $ "unexpected " ++ show e ++ " in lvalue"


exprVarDecls e = map NoDecl $ exprVars e

netPins :: [NetOrigin] -> ExtractM (Seq Pin)
netPins nos = mconcat <$> mapM go nos
  where
    go no = lookupNet no >>= \nt -> case nt of
        Nothing -> return S.empty
        Just (net, ty) -> return $ S.singleton $ Pin net ty

netPin :: NetOrigin -> ExtractM Pin
netPin no = lookupNet no >>= \nt -> case nt of
    Nothing -> error $ "missing net for origin " ++ show no
    Just (net, ty) -> return $ Pin net ty


-- Walk a statement to find all assignments inside.  For each one, emit a tuple
-- `(lval, rval, impVars)`, where `impVars` is the set of implicit
-- dependencies, or variable accessed on branch conditions preceding the
-- assignment.
stmtAssigns :: Stmt -> [(Expr, Expr, [Int])]
stmtAssigns s = go [] s
  where
    -- `imp`: Implicit inputs to any generated `Logic`s, resulting from
    -- the conditions on enclosing `if`s and such.
    go :: [Int] -> Stmt -> [(Expr, Expr, [Int])]
    go imp (If cond then_ else_) =
        let imp' = imp <> exprVars cond in
        foldMap (go imp') then_ <> maybe [] (foldMap $ go imp') else_
    go imp (Case cond cases) =
        let imp' = imp <> exprVars cond in
        foldMap (\(es, ss) -> foldMap (go $ imp' <> foldMap exprVars es) ss) cases
    go imp (For inits cond steps body) =
        -- Not sure how much of `For` we really need to handle, but here's a
        -- conservative guess...
        let imp' = imp <> exprVars cond in
        foldMap (go imp) inits <>
        foldMap (go imp') steps <>
        foldMap (go imp') body
    go imp (NonBlockingAssign lval rval) = [(lval, rval, imp)]
    go imp (BlockingAssign lval rval) = [(lval, rval, imp)]
    go imp (BlockingUpdate lval) = [(lval, lval, imp)]

convStmt :: Stmt -> ExtractM ()
convStmt s =
    mapM_ (\(l,r,imp) -> do
            lPins <- netPins $ exprVarDecls l
            rPins <- netPins $ exprVarDecls r
            addLogic $ Logic LkOther rPins lPins ()
        ) (stmtAssigns s)


prioExtPort = 3
prioInstPort = 2
prioWire = 4
prioFresh = 1
prioDcNet = -1


-- Type conversion

convTy :: V.Module -> V.Ty -> ExtractM A.Ty
convTy _ (V.TTy base vPacked vUnpacked) = do
    packed <- mapM convConstRangeSize vPacked
    unpacked <- mapM convConstRangeSize vUnpacked
    let wire = A.TWire packed unpacked
    let sim = A.TSimVal
    return $ case base of
        V.TLogic -> wire
        V.TReg -> wire
        V.TWire -> wire
        V.TTri -> wire
        V.TInt -> sim
        V.TInteger -> sim
        V.TString -> sim
        V.TReal -> sim
        V.TTime -> sim
convTy vMod (V.TEnum ty) = A.TEnum <$> convTy vMod ty
convTy vMod (V.TRef declId) =
    let V.TypedefDecl name ty = V.moduleDecls vMod `S.index` declId in
    A.TAlias name <$> convTy vMod ty


-- Expression conversion

typeofExpr :: V.Expr -> ExtractM A.Ty
typeofExpr e = exprType declVarTy convConstExpr e

declVarTy :: Int -> ExtractM A.Ty
-- TODO: look up `idx` as a param
declVarTy idx = gets (M.lookup (NoDecl idx) . esDeclNets) >>= \x -> case x of
    Nothing -> return A.TUnknown
    Just (_, ty) -> return ty

convConstExpr :: V.Expr -> ExtractM A.ConstExpr
convConstExpr e = go e
  where
    go (V.Param declId) = A.EParam <$> findParam declId
    go (V.Const t) | Just i <- parseBitConst t = return $ A.EIntLit i
    go (V.ConstInt _ i) = return $ A.EIntLit i
    go (V.Binary V.BAdd l r) = A.EBinArith A.BAdd <$> go l <*> go r
    go (V.Binary V.BSub l r) = A.EBinArith A.BSub <$> go l <*> go r
    go (V.Binary V.BMul l r) = A.EBinArith A.BMul <$> go l <*> go r
    go (V.Binary V.BEq l r) = A.EBinCmp A.BEq <$> go l <*> go r
    go (V.Binary V.BNe l r) = A.EBinCmp A.BNe <$> go l <*> go r
    go (V.Binary V.BLt l r) = A.EBinCmp A.BLt <$> go l <*> go r
    go (V.Binary V.BLe l r) = A.EBinCmp A.BLe <$> go l <*> go r
    go (V.Binary V.BGt l r) = A.EBinCmp A.BGt <$> go l <*> go r
    go (V.Binary V.BGe l r) = A.EBinCmp A.BGe <$> go l <*> go r
    go (V.Builtin BkClog2 [e]) = A.EUnArith A.UClog2 <$> go e
    go (V.Builtin BkSize [e]) = typeofExpr e >>= \ty -> case ty of
        A.TWire [] [] -> return $ A.EIntLit 1
        A.TWire ws [] -> return $ foldl1 (A.EBinArith A.BMul) ws
        t -> error $ "took $size of unsupported type " ++ show t
    go e = error $ "unsupported constexpr: " ++ show e

convConstRangeSize :: V.Range -> ExtractM A.ConstExpr
convConstRangeSize (V.Range l r) =
    A.ERangeSize <$> convConstExpr l <*> convConstExpr r

parseBitConst :: Text -> Maybe Int
parseBitConst t =
    case T.unpack $ T.take 2 rest of
        "'b" -> Just $ T.foldl (\i c -> 2 * i + case c of '0' -> 0; '1' -> 1) 0 digits
        _ -> Nothing
  where
    (width, rest) = T.breakOn "'" t
    digits = T.drop 2 rest
