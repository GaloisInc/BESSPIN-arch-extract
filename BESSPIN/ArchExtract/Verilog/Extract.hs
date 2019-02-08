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
import BESSPIN.ArchExtract.Simplify
import BESSPIN.ArchExtract.Lens


-- Must be declared at the top, before `makeLenses'` invocation.
data NetOrigin =
    NoDecl { noDeclId :: Int } |
    -- noDeclId: index of the `InstDecl` in `moduleDecls`
    -- noPortIdx: index of the port in the instantiated module's `moduleDecls`
    NoInstPort { noDeclId :: Int, noPortIdx :: Int }
    deriving (Show, Eq, Ord)

data ExtractState = ExtractState
    { esModule :: A.Module ()
    , esDeclNets :: Map NetOrigin (NetId, A.Ty)
    -- Maps the index of the `V.ParamDecl` in `V.moduleDecls` to the index of
    -- the `A.Param` in `A.moduleParams`.
    , esDeclParams :: Map Int Int 
    , esAllMods :: Seq V.Module
    }
    deriving (Show)

makeLenses' ''ExtractState

type ExtractM a = State ExtractState a

addThing :: a -> Lens' ExtractState (Seq a) -> ExtractM Int
addThing x fld = do
    idx <- S.length <$> use fld
    fld %= (|> x)
    return idx

addParam x = addThing x $ _esModule . _moduleParams
addInput x = addThing x $ _esModule . _moduleInputs
addOutput x = addThing x $ _esModule . _moduleOutputs
addLogic x = addThing x $ _esModule . _moduleLogics
addNet x = NetId <$> (addThing x $ _esModule . _moduleNets)

addDeclNet no x ty = do
    id <- addNet x
    _esDeclNets %= M.insert no (id, ty)
    return id

freshNet' baseName ty = do
    idx <- gets $ S.length . A.moduleNets . esModule
    let name = baseName <> "$" <> T.pack (show idx)
    addNet $ Net name prioFresh S.empty S.empty ty ()

freshNet ty = freshNet' "tmp" ty

addDeclParam declId x = do
    id <- addParam x
    _esDeclParams %= M.insert declId id
    return id

setParamDefault :: Int -> ConstExpr -> ExtractM ()
setParamDefault i e = do
    _esModule . _moduleParams . ix i . _paramDefault .= Just e


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

findModule :: Int -> ExtractM V.Module
findModule i = use $ _esAllMods . singular (ix i)


extractArch :: V.Design -> A.Design ()
extractArch vDes =
    mapMods (\mod ->
        --filterLogics (\_ _ -> False) $
        --filterInsts (\_ i ->
            --not $ modInstName i `elem` map T.pack ["ra1reg"]) $
        --filterInsts (\_ i ->
        --    not $ moduleName (designMods d `S.index` modInstId i)
        --        `elem` map T.pack ["mux2", "eqcmp"]) $
        --filterInstsToLogic (\_ i ->
            --not $ moduleName (designMods d `S.index` modInstId i)
                --`elem` []) $
                --`elem` map T.pack ["mux2", "eqcmp"]) $
                --`elem` map T.pack ["mux2", "eqcmp", "flopenr", "flopenrc"]) $
            --`elem` map T.pack ["adder"]) mod) $
        mod) $
    d
  where
    vMods = V.designModules vDes
    d = A.Design $ fmap (extractMod vMods) vMods

extractMod :: Seq V.Module -> V.Module -> A.Module ()
extractMod vMods vMod =
    mergeAliasedNets $
    -- Break up "uninteresting" nets early, before they can get merged with
    -- other nets.
    disconnect (\_ _ _ net ->
        let baseName = last $ T.splitOn (T.singleton '.') (netName net) in
        not $ baseName `elem` map T.pack ["clock", "clk", "reset"]) $
    reconnectNets $ buildMod vMods vMod


buildMod :: Seq V.Module -> V.Module -> A.Module ()
buildMod vMods vMod = esModule $ execState go initState
  where
    initMod = A.Module
        { A.moduleName = V.moduleName vMod 
        , A.moduleParams = S.empty
        , A.moduleInputs = S.empty
        , A.moduleOutputs = S.empty
        , A.moduleLogics = S.empty
        , A.moduleNets = S.empty
        }
    initState = ExtractState initMod M.empty M.empty vMods

    go :: ExtractM ()
    go = do
        addDeclaredNets vMod
        addDeclaredParams vMod
        convDecls vMod
        trace (T.unpack $ describeMod vMod) $ convItems vMod

describeMod :: V.Module -> Text
describeMod vMod = T.unlines $
    ["module " <> V.moduleName vMod] ++
    ["  decls:"] ++
    toList (S.mapWithIndex
        (\i x -> "    " <> T.pack (show i) <> ": " <> T.pack (show x))
        (V.moduleDecls vMod)) ++
    ["  ports:"] ++
    toList (S.mapWithIndex
        (\i x -> "    " <> T.pack (show i) <> ": " <> T.pack (show x))
        (V.modulePorts vMod)) ++
    ["  items:"] ++
    toList (S.mapWithIndex
        (\i x -> "    " <> T.pack (show i) <> ": " <> T.pack (show x))
        (V.moduleItems vMod))


-- Building `moduleNets` from `V.moduleDecls`

-- Info used to build a `Net` and its corresponding `NetMap` entry.
data NetParts = NetParts
    { netPartsName :: Text
    , netPartsNamePriority :: Int
    , netPartsOrigin :: NetOrigin
    , netPartsTy :: A.Ty
    }
    deriving (Show)

type NetMap = Map NetOrigin (NetId, A.Ty)

addDeclNetFromParts :: NetParts -> ExtractM NetId
addDeclNetFromParts (NetParts name prio origin ty) =
    addDeclNet origin (Net name prio S.empty S.empty ty ()) ty


prioExtPort = 3
prioInstPort = 2
prioWire = 4
prioFresh = 1
prioDcNet = -1

-- Add all declared nets in `vMod`.  The nets are included in `esDeclNets` so
-- they can be looked up by `NetOrigin` later.
addDeclaredNets :: V.Module -> ExtractM ()
addDeclaredNets vMod = do
    vMods <- use _esAllMods
    void $ flip S.traverseWithIndex (V.moduleDecls vMod) $ \idx decl ->
        forM_ (declNet vMods vMod idx decl) $ \np ->
            addDeclNetFromParts np

declNet :: Seq V.Module -> V.Module -> Int -> V.Decl -> [NetParts]
declNet vMods vMod i (V.PortDecl name vTy _) =
    [NetParts name prioExtPort (NoDecl i) (convTy vMod vTy)]
declNet vMods vMod i (V.ParamDecl name vTy _) = []
declNet vMods vMod i (V.VarDecl name vTy) =
    [NetParts name prioWire (NoDecl i) (convTy vMod vTy)]
declNet vMods vMod i (V.TypedefDecl _ _) = []
declNet vMods vMod i (V.InstDecl name modId _) =
    let vInstMod = vMods `S.index` modId in
    map (\portIdx ->
            let PortDecl portName vTy _ = moduleDecls vInstMod `S.index` portIdx in
            let ty = convTy vInstMod vTy in
            NetParts (name <> T.pack "." <> portName) prioInstPort (NoInstPort i portIdx) ty)
        (toList $ modulePorts vInstMod)

convTy :: V.Module -> V.Ty -> A.Ty
convTy _ (V.TTy base packed unpacked) =
    let wire = A.TWire (length packed) (length unpacked) in
    let sim = A.TSimVal in
    case base of
        V.TLogic -> wire
        V.TReg -> wire
        V.TWire -> wire
        V.TTri -> wire
        V.TInt -> sim
        V.TInteger -> sim
        V.TString -> sim
        V.TReal -> sim
        V.TTime -> sim
convTy vMod (V.TEnum ty) = A.TEnum $ convTy vMod ty
convTy vMod (V.TRef declId) =
    let V.TypedefDecl name ty = V.moduleDecls vMod `S.index` declId in
    A.TAlias name $ convTy vMod ty


-- Add all declared parameters in `vMod`.
addDeclaredParams :: V.Module -> ExtractM ()
addDeclaredParams vMod = do
    -- First, add declarations for all params, but leave the initializers
    -- blank.
    void $ flip S.traverseWithIndex (V.moduleDecls vMod) $ \idx decl ->
        case decl of
            ParamDecl name _ _ -> void $ addDeclParam idx $ A.Param name Nothing
            _ -> return ()

    -- Now we can translate the initializer expressions, including ones that
    -- refer to othe parameters.
    void $ flip S.traverseWithIndex (V.moduleDecls vMod) $ \idx decl ->
        case decl of
            ParamDecl _ _ (Just init) -> do
                i <- findParam idx
                e <- traceShow ("convert", idx, decl) $ convConstExpr init
                setParamDefault i e
            _ -> return ()


-- Building `moduleInputs`, `moduleOutputs`, and `moduleInsts` from
-- `V.moduleDecls`.

convDecls :: V.Module -> ExtractM ()
convDecls vMod = void $ S.traverseWithIndex convDecl (V.moduleDecls vMod) 

convDecl :: Int -> V.Decl -> ExtractM ()
convDecl idx (V.PortDecl name vTy dir) = do
    (net, ty) <- findNet $ NoDecl idx
    let port = A.Port name net ty
    void $ case dir of
        V.Input -> addInput port
        V.Output -> addOutput port
        V.InOut -> addInput port >> addOutput port
convDecl _ (V.ParamDecl _ _ _) = return ()
convDecl _ (V.VarDecl _ _) = return ()
convDecl _ (V.TypedefDecl _ _) = return ()
convDecl idx (V.InstDecl name modId paramVals) = do
    vInstMod <- findModule modId
    ins <- forM (dirPins [V.Input, V.InOut] vInstMod) $ \portIdx -> do
        (net, ty) <- findNet $ NoInstPort idx portIdx
        return $ Pin net ty
    outs <- forM (dirPins [V.Output, V.InOut] vInstMod) $ \portIdx -> do
        (net, ty) <- findNet $ NoInstPort idx portIdx
        return $ Pin net ty
    params <- S.fromList <$> mapM (mapM convConstExpr) paramVals
    void $ addLogic $
        Logic (LkInst $ Inst modId name params) (S.fromList ins) (S.fromList outs) ()

-- Get the port index (the index of the PortDecl in `V.moduleDecls`) of all
-- ports of `vMod` whose direction is in `dirs`.
dirPins :: [PortDir] -> V.Module -> [Int]
dirPins dirs vMod = filter (\portIdx ->
        V.portDeclDir (V.moduleDecls vMod `S.index` portIdx) `elem` dirs)
    (toList $ modulePorts vMod)


-- Building `moduleLogics` from `V.moduleItems`

isInput Input = True
isInput InOut = True
isInput Output = False

isOutput Output = True
isOutput InOut = True
isOutput Input = False

convItems :: V.Module -> ExtractM ()
convItems vMod = mapM_ (convItem vMod) (V.moduleItems vMod)

convItem :: V.Module -> V.Item -> ExtractM ()
convItem vMod (InitVar declId e) = convAssign (Var declId) e
convItem vMod (InitInst declId portConns) = do
    let vInst = V.moduleDecls vMod `S.index` declId
    vInstMod <- findModule $ V.instanceModId vInst
    forM_ (zip (toList $ V.modulePorts vInstMod) portConns) $ \(portIdx, conn) -> do
        let vPort = V.moduleDecls vInstMod `S.index` portIdx
        when (isInput $ V.portDeclDir vPort) $
            mkLogic (rvalKind conn) [NoInstPort declId portIdx] (exprVarDecls conn)
        when (isOutput $ V.portDeclDir vPort) $
            mkLogic LkNetAlias (exprVarDecls conn) [NoInstPort declId portIdx]
convItem vMod (ContAssign l r) = convAssign l r
convItem vMod item@V.Always{} | traceShow ("always in", V.moduleName vMod,
    "inferred:", inferDFlipFlop item) False = undefined
convItem vMod item@V.Always{} | Right dffs <- inferDFlipFlop item = forM_ dffs $ \dff -> do
    clkPin <- rvalPin $ dffClk dff
    dPin <- rvalPin $ dffD dff
    arstPins <- mapM rvalPin $ S.fromList $ dffAsyncResets dff
    (net, ty) <- findNet $ NoDecl $ dffQ dff
    let qPin = Pin net ty
    name <- findNetName $ NoDecl $ dffQ dff
    addLogic $ Logic
        (LkDFlipFlop name $ S.length arstPins)
        (dPin <| clkPin <| arstPins)
        (S.singleton qPin)
        ()
-- Assignments inside edge-sensitive `Always` blocks generate registers.
convItem vMod (Always evts body) | any (isJust . eventEdge) evts =
    mapM_ (\(lv, rvs) -> do
            name <- findNetName $ NoDecl lv
            mkLogic (LkRegister name) [NoDecl lv] (map NoDecl $ Set.toList rvs)
        ) (M.toList regMap)
  where
    assigns = foldMap stmtAssigns body

    -- We generate a register for each net that's assigned to in this `always`.
    -- `regMap` maps each assigned (output) net to the inputs to the register.
    regMap :: Map Int (Set Int)
    regMap = M.unionsWith (<>) $
        concatMap (\(l,r,imp) ->
            map (\lv -> M.singleton lv (Set.fromList $ exprVars r <> imp)) (exprVars l)
        ) assigns
-- Other `Always` blocks generate normal `LkOther` logic.
convItem vMod (Always _ body) = mapM_ convStmt body
convItem vMod (Initial body) = mapM_ convStmt body

convAssign :: V.Expr -> V.Expr -> ExtractM ()
convAssign lhs rhs =
    mkLogic (rvalKind rhs) (exprVarDecls lhs) (exprVarDecls rhs)

-- Get the origin info for every net used in `e`.
exprVars :: Expr -> [Int]
exprVars e = everything (<>) ([] `mkQ` go) e
  where
    go (Var defId) = [defId]
    go _ = []

exprVarDecls e = map NoDecl $ exprVars e

-- Get the `LogicKind` for an rvalue expression.
rvalKind :: Expr -> LogicKind
rvalKind (Var defId) = LkNetAlias
rvalKind _ = LkOther

-- Add a Logic node for 
mkLogic :: LogicKind -> [NetOrigin] -> [NetOrigin] -> ExtractM ()
mkLogic kind lhs rhs = do
    inPins <- netPins rhs
    outPins <- netPins lhs
    void $ addLogic $ Logic kind inPins outPins ()

netPins :: [NetOrigin] -> ExtractM (Seq Pin)
netPins nos = mconcat <$> mapM go nos
  where
    go no = lookupNet no >>= \nt -> case nt of
        Nothing -> return S.empty
        Just (net, ty) -> return $ S.singleton $ Pin net ty

-- Get a single pin representing the output of an expression.  Generates a new
-- `Logic` and `Net` if needed.
rvalPin :: Expr -> ExtractM Pin
rvalPin (Var v) = lookupNet (NoDecl v) >>= \nt -> case nt of
    Just (net, ty) -> return $ Pin net ty
    -- If there's no net for this var, it must be a `ParamDecl`.  Treat it as
    -- an unknown expression, which will generate a 0-input `Logic`.
    Nothing -> rvalPin UnknownExpr
rvalPin e = do
    inPins <- netPins $ exprVarDecls e
    declNets <- gets esDeclNets
    let ty = exprType (declVarTy declNets) e
    outNet <- freshNet ty
    addLogic $ Logic LkOther inPins (S.singleton $ Pin outNet ty) ()
    return $ Pin outNet ty
  where
    declVarTy declNets idx = case M.lookup (NoDecl idx) declNets of
        Nothing -> TUnknown
        Just (_, ty) -> ty


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
    mapM_ (\(l,r,imp) ->
            mkLogic LkOther (exprVarDecls l) (exprVarDecls r ++ map NoDecl imp)
        ) (stmtAssigns s)


convConstExpr :: V.Expr -> ExtractM A.ConstExpr
convConstExpr e = go e
  where
    go (V.Param declId) = A.EParam <$> findParam declId
    go (V.Const t) | Just i <- parseBitConst t = return $ A.EIntLit i
    go (V.ConstInt _ i) = return $ A.EIntLit i
    go (V.Binary V.BAdd l r) = A.EBinOp A.CbAdd <$> go l <*> go r
    go (V.Binary V.BSub l r) = A.EBinOp A.CbSub <$> go l <*> go r
    go (V.Binary V.BMul l r) = A.EBinOp A.CbMul <$> go l <*> go r
    go (V.Builtin BkClog2 [e]) = A.ELog2 <$> go e
    go e = error $ "unsupported constexpr: " ++ show e

parseBitConst :: Text -> Maybe Int
parseBitConst t =
    case T.unpack $ T.take 2 rest of
        "'b" -> Just $ T.foldl (\i c -> 2 * i + case c of '0' -> 0; '1' -> 1) 0 digits
        _ -> Nothing
  where
    (width, rest) = T.breakOn "'" t
    digits = T.drop 2 rest
