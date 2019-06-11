{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module BESSPIN.ArchExtract.Chisel.Extract where

import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro.Platform

import Debug.FilterTrace

import qualified BESSPIN.ArchExtract.Config as Config
import BESSPIN.ArchExtract.Lens
import qualified BESSPIN.ArchExtract.Architecture as A
import BESSPIN.ArchExtract.Chisel.FIRRTL.AST
import BESSPIN.ArchExtract.Simplify (reconnectNets, mergeAliasedNets)


TraceAPI trace traceId traceShow traceShowId traceM traceShowM = mkTraceAPI "Chisel.Extract"



data Interface =
    -- The `Int` is the index of the port among the module's inputs/outputs.
      IfPort Direction Int Ty
    | IfBundle (Seq (Text, Interface)) (Map Text Interface)
    deriving (Show)

data Value =
      VNet A.NetId
    -- TODO: should be `VVector Value`, to handle vectors of vectors, vectors
    -- of bundles, etc.
    | VVector A.NetId
    -- `Bool` is the "flip" flag for the field.
    | VBundle (Map Text (Value, Bool))
    -- Acts like `.0` when used on the RHS and `.1` when used on the LHS of a
    -- connection statement.
    --
    -- Note that `VDff` should never appear inside a `VBundle`.
    | VDff Value Value
    -- Index of the `Logic` node.  Also stores the element type, which often is
    -- not provided at port definitions.
    | VRam Int Ty
    | VError
    deriving (Show)

data ModInfo = ModInfo
    { miName :: Text
    , miPorts :: [Port]
    , miIfc :: Interface
    , miModId :: A.ModId
    }

data ExtractState = ExtractState
    { esCurModule :: A.Module ()
    , esModMap :: Map Text ModInfo
    , esLocalScope :: Map Text Value
    }

makeLenses' ''ModInfo
makeLenses' ''ExtractState

type ExtractM a = State ExtractState a

withModule :: A.Module () -> ExtractM a -> ExtractM (a, A.Module ())
withModule initMod act = do
    oldMod <- _esCurModule <<.= initMod
    r <- act
    newMod <- _esCurModule <<.= oldMod
    return (r, newMod)

withScope :: ExtractM a -> ExtractM a
withScope act = do
    oldScope <- _esLocalScope <<.= M.empty
    r <- act
    _esLocalScope .= oldScope
    return r

bindLocal :: Text -> Value -> ExtractM ()
bindLocal name val = _esLocalScope %= M.insert name val


extractDesign :: Config.Chisel -> Circuit -> A.Design ()
extractDesign cfg circ = evalState (extractDesign' cfg circ) initState
  where
    firMods = circuitModules circ
    modMap = M.fromList $ zipWith mkModInfo firMods [0..]

    mkModInfo m i = (name, ModInfo name ports ifc i)
      where
        name = moduleName m
        ports = modulePorts m
        ifc = convertInterface $ modulePorts m

    initState = ExtractState
        { esCurModule = error $ "current module is unset"
        , esModMap = modMap
        , esLocalScope = M.empty
        }

extractDesign' :: Config.Chisel -> Circuit -> ExtractM (A.Design ())
extractDesign' cfg circ = do
    mods <- S.fromList <$> mapM (extractModule cfg) (circuitModules circ)
    return $ A.Design mods

extractModule :: Config.Chisel -> Module -> ExtractM (A.Module ())
extractModule cfg m = do
    ((), m') <- withModule initMod $ do
        let ifc = convertInterface $ modulePorts m

        (v, ins, outs) <- buildIfcNets "" ifc (moduleSig $ modulePorts m)
        ins' <- mapM buildPort ins
        outs' <- mapM buildPort outs
        _esCurModule . A._moduleInputs .= S.fromList ins'
        _esCurModule . A._moduleOutputs .= S.fromList outs'

        withScope $ do
            let vars = case v of
                    VBundle fs -> M.toList fs
                    _ -> traceShow ("impossible: module buildIfcNets produced non-bundle?",
                        moduleName m, v) []

            forM_ vars $ \(name, (val, _)) -> bindLocal name val

            case moduleKind m of
                MkNormal body -> do
                    let bbox = Set.member (moduleName m) (Config.chiselBlackboxModules cfg)
                    when (not bbox) $ do
                        conns <- evalStmt body
                        traceShowM ("connections", conns)
                        makeConnections conns
                MkExtern _ -> return ()

    return $
        mergeAliasedNets $
        reconnectNets $
        m'
  where
    initMod = A.Module (moduleName m) (convertModuleKind $ moduleKind m)
        S.empty S.empty S.empty S.empty S.empty S.empty

convertModuleKind (MkNormal _) = A.MkNormal
convertModuleKind (MkExtern _) = A.MkExtern

buildNet :: Text -> Int -> Ty -> ExtractM A.NetId
buildNet name prio ty = buildNet' name prio (convertTy ty)

buildNet' :: Text -> Int -> A.Ty -> ExtractM A.NetId
buildNet' name prio ty = zoom (_esCurModule . A._moduleNets) $ do
    idx <- gets S.length
    modify (|> A.Net name prio S.empty S.empty ty ())
    return $ A.NetId idx

buildLogic :: A.LogicKind -> [A.NetId] -> [A.NetId] -> ExtractM Int
buildLogic kind ins outs = do
    ins' <- S.fromList <$> mapM buildPin ins
    outs' <- S.fromList <$> mapM buildPin outs
    zoom (_esCurModule . A._moduleLogics) $ do
        idx <- gets S.length
        modify (|> A.Logic kind ins' outs' ())
        return idx

buildPin :: A.NetId -> ExtractM A.Pin
buildPin n = do
    ty <- use $ _esCurModule . A._moduleNet n . A._netTy
    return $ A.Pin n ty

buildNetAlias :: A.NetId -> A.NetId -> ExtractM Int
buildNetAlias src dest = buildLogic A.LkNetAlias [src] [dest]

buildPort :: A.NetId -> ExtractM A.Port
buildPort n = do
    (name, ty) <- zoom (_esCurModule . A._moduleNet n) $ do
        name <- use A._netName
        ty <- use A._netTy
        return (head $ T.lines name, ty)
    return $ A.Port name n ty



flipDir :: Direction -> Direction
flipDir Input = Output
flipDir Output = Input

mkIfBundle :: [(Text, Interface)] -> Interface
mkIfBundle xs = IfBundle (S.fromList xs) (M.fromList xs)

convertInterface :: [Port] -> Interface
convertInterface ps = evalState (mkIfBundle <$> mapM goPort ps) (0, 0)
  where
    goPort :: Port -> State (Int, Int) (Text, Interface)
    goPort (Port _ name dir ty) = go dir ty >>= \ifc -> return (name, ifc)

    goField :: Direction -> Field -> State (Int, Int) (Text, Interface)
    goField dir (Field name ty flip) = go dir' ty >>= \ifc -> return (name, ifc)
      where dir' = if flip then flipDir dir else dir

    go :: Direction -> Ty -> State (Int, Int) Interface
    go dir (TBundle fs) = mkIfBundle <$> mapM (goField dir) fs
    go dir ty = do
        i <- assignId dir
        return $ IfPort dir i ty

    assignId Input = _1 <<%= (+1)
    assignId Output = _2 <<%= (+1)

-- Return all the input and output ports of `ifc`, in order.
interfacePorts :: Interface -> ([([Text], Ty)], [([Text], Ty)])
interfacePorts (IfPort Input _ ty) = ([([], ty)], [])
interfacePorts (IfPort Output _ ty) = ([], [([], ty)])
interfacePorts (IfBundle xs _) =
    mconcat $ map (\(name, ifc) ->
        interfacePorts ifc & each . each . _1 %~ (name :)) $ toList xs


convertTy :: Ty -> A.Ty
convertTy _ = A.TUnknown


asNet :: Value -> ExtractM A.NetId
asNet (VNet n) = return n
asNet (VVector n) = traceShow ("warning: casting vector to net", n) $ return n
asNet (VBundle fs) = traceShow ("error: casting bundle to net", fs) $ buildDummyNet "<bundle>"
asNet (VDff r l) = traceShow ("warning: casting dff to net", r, l) $ asNet r
asNet (VRam idx ty) = traceShow ("error: casting ram to net", idx, ty) $ buildDummyNet "<error>"
-- We already reported whatever error produced this `VError`.
asNet VError = buildDummyNet "<error>"

buildDummyNet :: Text -> ExtractM A.NetId
buildDummyNet name = buildNet name (-10) TUnknown


-- Compute the set of net connections made by a `Stmt`.  (A statement can also
-- disconnect nets, represented as connecting the net to `Nothing`.)  Statement
-- evaluation can also have side effects on the extraction state, such as
-- producing new logic nodes or adding variables to the current scope.
evalStmt :: Stmt -> ExtractM (Map A.NetId (Maybe A.NetId))
evalStmt (SDef _ d) = evalDef d >> return M.empty
evalStmt (SCond _ cond then_ else_) = do
    condVal <- asNet =<< evalExpr cond
    a <- evalStmt then_
    b <- evalStmt else_
    let overlap = M.intersectionWith (\x y -> (x, y)) a b
    overlap' <- traverse (\(x, y) -> Just <$> muxNetsOpt condVal x y) overlap
    return $ overlap' <> a <> b
evalStmt (SBlock stmts) =
    -- `M.union` is left-biased, so we `flip` it to make new connections
    -- override the old ones.
    foldM (\acc s -> flip M.union acc <$> evalStmt s) M.empty stmts
evalStmt (SPartialConnect _ lhs rhs) = do
    lhsVal <- evalExpr lhs
    rhsVal <- evalExpr rhs
    doConnect lhsVal rhsVal
evalStmt (SConnect _ lhs rhs) = do
    lhsVal <- evalExpr lhs
    rhsVal <- evalExpr rhs
    doConnect lhsVal rhsVal
evalStmt (SIsInvalid _ e) = do
    val <- evalExpr e
    doDisconnect val
evalStmt (SAttach src es) = traceShow ("evalStmt SAttach NYI", src, es) $ return M.empty
evalStmt (SStop _ _ _ _) = return M.empty
evalStmt (SPrint _ _ _ _ _) = return M.empty
evalStmt SEmpty = return M.empty

-- Evaluate a definition.  This typically adds new local variables.
evalDef :: Def -> ExtractM ()
evalDef (DWire name ty) = do
    (v, _) <- makeTypedValue name ty
    bindLocal name v
evalDef (DReg name ty _clk _res _init) = do
    inNet <- buildNet (name <> ".D") 15 ty
    outNet <- buildNet (name <> ".Q") 15 ty
    buildLogic (A.LkDFlipFlop name 0) [inNet] [outNet]
    inVal <- packTy (name <> ".D") ty inNet
    outVal <- unpackTy (name <> ".Q") ty outNet
    bindLocal name $ VDff outVal inVal
evalDef (DInst name modName) = use (_esModMap . at modName) >>= \x -> case x of
    Nothing ->
        traceShow ("instantiation of unknown module", name, modName) $ return ()
    Just mi -> do
        (v, ins, outs) <- buildIfcNets name (miIfc mi) (moduleSig $ miPorts mi)
        buildLogic (A.LkInst $ A.Inst (miModId mi) name S.empty) ins outs
        bindLocal name $ v
evalDef (DMemory name ty depth rds wrs rdwrs) = do
    idx <- buildRam name ty depth
    bindLocal name $ VRam idx ty

    forM_ rds $ \rd -> do
        (addrNet, dataNet) <- addRamReadPort idx rd ty
        traceShowM ("NYI: don't know how to connect up address lines of DMemory ports",
                name, rd, addrNet)
        rdVal <- unpackTy (name <> "." <> rd) ty dataNet
        bindLocal rd $ VDff rdVal VError

    forM_ wrs $ \wr -> do
        (addrNet, dataNet) <- addRamWritePort idx wr ty
        traceShowM ("NYI: don't know how to connect up address lines of DMemory ports",
                name, wr, addrNet)
        wrVal <- packTy (name <> "." <> wr) ty dataNet
        bindLocal wr $ VDff VError wrVal

    forM_ rdwrs $ \rdwr -> do
        (rAddrNet, rDataNet) <- addRamReadPort idx (rdwr <> ".rd") ty
        (wAddrNet, wDataNet) <- addRamWritePort idx (rdwr <> ".wr") ty
        traceShowM ("NYI: don't know how to connect up address lines of DMemory ports",
                name, rdwr, rAddrNet, wAddrNet)
        rdVal <- unpackTy (name <> "." <> rdwr) ty rDataNet
        wrVal <- packTy (name <> "." <> rdwr) ty wDataNet
        bindLocal rdwr $ VDff rdVal wrVal
evalDef (DNode name expr) = bindLocal name =<< nameValue name =<< evalExpr expr
evalDef (DCMem name ty depth isSync) = do
    idx <- buildRam name ty depth
    bindLocal name $ VRam idx ty
evalDef (DCMemPort name ty memName args dir)
  | [addr, _clk] <- args = use (_esLocalScope . at memName) >>= \x -> case x of
    Just (VRam idx ty') -> do
        addrNet <- asNet =<< evalExpr addr
        -- TODO: "Infer" ports are usually read-only or write-only, but we
        -- always produce both halves, usually involving big unused struct
        -- pack/unpack nodes.  We should have a postprocessing pass remove
        -- those from the architecture.
        wr <- if dir `elem` [MpdInfer, MpdWrite, MpdReadWrite] then do
                (wAddrNet, wDataNet) <- addRamWritePort idx (name <> ".wr") ty
                buildNetAlias addrNet wAddrNet
                traceShowM ("write port", name, memName, ty, ty')
                packTy (name <> ".wr") ty' wDataNet
            else return VError
        rd <- if dir `elem` [MpdInfer, MpdRead, MpdReadWrite] then do
                (rAddrNet, rDataNet) <- addRamReadPort idx (name <> ".rd") ty
                buildNetAlias addrNet rAddrNet
                unpackTy (name <> ".rd") ty' rDataNet
            else return VError
        bindLocal name $ VDff rd wr
    Just v -> traceShowM ("port references non-memory", memName, name, v)
    Nothing -> traceShowM ("unknown local memory", memName, name)
  | otherwise = traceShowM ("bad arg count for CMemPort", name, memName, args)


buildRam :: Text -> Ty -> Int -> ExtractM Int
buildRam name ty depth = do
    dummyRamIn <- buildDummyNet "<ram?>"
    dummyRamOut <- buildDummyNet "<ram?>"
    dummyClock <- buildDummyNet "<clk?>"
    idx <- buildLogic (A.LkRam name (A.EIntLit (A.Span 0 0) depth) 0 0 0)
        [dummyRamIn, dummyClock] [dummyRamOut]
    bindLocal name $ VRam idx ty
    return idx

-- Add a new read port to the indicated `LkRam` logic node.  Returns the nets
-- connected to the new read-address and read-data ports.
addRamReadPort :: Int -> Text -> Ty -> ExtractM (A.NetId, A.NetId)
addRamReadPort idx name ty = do
    -- TODO: infer address width from RAM depth, like FIRRTL compiler does
    addrNet <- buildNet (name <> ".ra") 10 (TUInt $ WInt 99)
    dataNet <- buildNet (name <> ".rd") 10 ty
    addrPin <- buildPin addrNet
    dataPin <- buildPin dataNet
    zoom (_esCurModule . A._moduleLogic idx) $ do
        numWrites <- zoom A._logicKind $ state $ \lk -> case lk of
            A.LkRam {} ->
                (A.lkRamWritePorts lk,
                    lk { A.lkRamReadPorts = A.lkRamReadPorts lk + 1 })
            _ -> (0, traceShow ("tried to add read port to non-RAM", lk, idx, name) lk)
        A._logicInputs %= \ins -> S.insertAt (S.length ins - 3 * numWrites) addrPin ins
        A._logicOutputs %= (|> dataPin)
    return (addrNet, dataNet)

-- Add a new write port to the indicated `LkRam` logic node.  Returns the nets
-- connected to the new write-address and write-data ports.
addRamWritePort :: Int -> Text -> Ty -> ExtractM (A.NetId, A.NetId)
addRamWritePort idx name ty = do
    -- TODO: infer address width from RAM depth, like FIRRTL compiler does
    addrNet <- buildNet (name <> ".wa") 10 (TUInt $ WInt 99)
    dataNet <- buildNet (name <> ".wd") 10 ty
    enableNet <- buildNet (name <> ".we") 10 (TUInt $ WInt 1)
    addrPin <- buildPin addrNet
    dataPin <- buildPin dataNet
    enablePin <- buildPin enableNet
    zoom (_esCurModule . A._moduleLogic idx) $ do
        A._logicKind %= \lk -> case lk of
            A.LkRam {} -> lk { A.lkRamWritePorts = A.lkRamWritePorts lk + 1 }
            _ -> traceShow ("tried to add write port to non-RAM", lk, idx, name) lk
        A._logicInputs %= \x -> x |> addrPin |> dataPin |> enablePin
    return (addrNet, dataNet)


evalExpr :: Expr -> ExtractM Value
evalExpr (ELit _) = VNet <$> buildDummyNet "<const>"
evalExpr (ERef name ty) = use (_esLocalScope . at name) >>= \x -> case x of
    Nothing -> traceShow ("unknown local variable", name, ty) $ return VError
    Just v -> return v
evalExpr (EField e name ty) = do
    val <- evalExpr e
    return $ valueField val name
  where
    valueField (VBundle fs) name = case M.lookup name fs of
        Just (v, _) -> v
        Nothing -> traceShow ("unknown field", name, fs) VError
    valueField (VDff r l) name = VDff (valueField r name) (valueField l name)
    valueField val name =
        traceShow ("field access of non-bundle", name, val) VError
evalExpr (EIndex e idx ty) = do
    val <- evalExpr e
    idxNet <- asNet =<< evalExpr idx
    case val of
        VVector n -> do
            outNet <- buildNet "<item>" 0 ty
            buildLogic A.LkExpr [n, idxNet] [outNet]
            return $ VNet outNet
        _ -> traceShow ("indexed into non-vector", e, idx) $ return VError
evalExpr (EIndexC e idx ty) = do
    val <- evalExpr e
    case val of
        VVector n -> do
            outNet <- buildNet ("<item " <> T.pack (show idx) <> ">") 0 ty
            buildLogic A.LkExpr [n] [outNet]
            return $ VNet outNet
        _ -> traceShow ("indexed into non-vector", e, idx) $ return VError
evalExpr (EMux cond then_ else_ ty) = do
    cond' <- asNet =<< evalExpr cond
    then_' <- asNet =<< evalExpr then_
    else_' <- asNet =<< evalExpr else_
    outNet <- buildNet "<mux>" 0 ty
    buildLogic (A.LkMux ("val" <| S.empty) 2) [cond', then_', else_'] [outNet]
    return $ VNet outNet
evalExpr (EValidIf cond then_ ty) = do
    cond' <- asNet =<< evalExpr cond
    then_' <- asNet =<< evalExpr then_
    else_' <- buildNet "<undef>" 0 ty
    outNet <- buildNet "<mux>" 0 ty
    buildLogic (A.LkMux ("val" <| S.empty) 2) [cond', then_', else_'] [outNet]
    return $ VNet outNet
evalExpr (EPrim op args _ ty) = do
    argNets <- mapM asNet =<< mapM evalExpr args
    outNet <- buildNet ("<" <> op <> ">") 0 ty
    buildLogic A.LkExpr argNets [outNet]
    return $ VNet outNet

-- Generate a new value of type `ty`, whose leaf fields are combined using an
-- `LkPack` node and output to `outNet`.  This is used for handling registers
-- of non-ground type.
packTy :: Text -> Ty -> A.NetId -> ExtractM Value
packTy name ty outNet = do
    (v, leaves) <- makeTypedValue name ty
    let (names, nets) = unzip leaves
    if length leaves == 1 then
        buildNetAlias (head nets) outNet
    else
        buildLogic (A.LkPack $ S.fromList names) nets [outNet]
    return v

unpackTy :: Text -> Ty -> A.NetId -> ExtractM Value
unpackTy name ty inNet = do
    (v, leaves) <- makeTypedValue name ty
    let (names, nets) = unzip leaves
    if length leaves == 1 then
        buildNetAlias inNet (head nets)
    else
        buildLogic (A.LkUnpack $ S.fromList names) [inNet] nets
    return v

-- Construct a `Value` that's an instance of `Ty`.  Also returns the net IDs of
-- all the leaf values, together with names.
makeTypedValue :: Text -> Ty -> ExtractM (Value, [(Text, A.NetId)])
makeTypedValue prefix (TBundle fs) = do
    (m, leaves) <- liftM mconcat $ forM fs $ \f -> do
        -- FIRRTL spec says memories contain only passive types.  I assume
        -- registers work the same way.
        when (fieldFlip f) $
            traceShowM ("unexpected flip field in bundle", f)
        (val, leaves) <- makeTypedValue (prefix <> "." <> fieldName f) (fieldTy f)
        return (M.singleton (fieldName f) (val, fieldFlip f), leaves)
    return (VBundle m, leaves)
makeTypedValue prefix (TVector ty _) = do
    (v, leaves) <- makeTypedValue (prefix <> ".items") ty
    let v' = case v of
            VNet n -> VVector n
            _ -> traceShow ("vector of non-primitive NYI", v) VError
    return (v', leaves)
makeTypedValue prefix ty = do
    n <- buildNet prefix 11 ty
    return (VNet n, [(prefix, n)])



moduleSig :: [Port] -> Ty
moduleSig ps = TBundle $ map (\p ->
    Field (portName p) (portTy p) (portDir p == Input)) ps

buildIfcNets :: Text -> Interface -> Ty -> ExtractM (Value, [A.NetId], [A.NetId])
buildIfcNets prefix (IfBundle ifcFields _) (TBundle tyFields) = do
    parts <- forM (zip (toList ifcFields) tyFields) $ \((ifcName, subIfc), fld) -> do
        when (ifcName /= fieldName fld) $
            traceShowM ("buildIfcNets: field name mismatch", ifcName, fieldName fld)
        (v, ins, outs) <- buildIfcNets (prefix <> "." <> ifcName) subIfc (fieldTy fld)
        return (M.singleton ifcName (v, fieldFlip fld), ins, outs)
    let (fs, ins, outs) = mconcat parts
    return (VBundle fs, ins, outs)
buildIfcNets prefix (IfPort dir _ _) ty
  | TUInt _ <- ty = buildLeafNet prefix dir ty VNet
  | TSInt _ <- ty = buildLeafNet prefix dir ty VNet
  | TFixed _ _ <- ty = buildLeafNet prefix dir ty VNet
  | TVector _ _ <- ty = buildLeafNet prefix dir ty VVector
  | TClock <- ty = buildLeafNet prefix dir ty VNet
buildIfcNets prefix ifc ty = do
    traceShowM ("don't know how to build interface nets", prefix, ifc, ty)
    return (VError, [], [])

buildLeafNet name dir ty ctor = do
    n <- buildNet name 11 ty
    if dir == Input then
        return (ctor n, [n], [])
    else
        return (ctor n, [], [n])


muxNets :: A.NetId -> A.NetId -> A.NetId -> ExtractM A.NetId
muxNets c t e = do
    outNet <- buildNet "mux_out" 0 TUnknown
    buildLogic (A.LkMux ("val" <| S.empty) 2) [c, t, e] [outNet]
    return outNet

muxNetsOpt :: A.NetId -> Maybe A.NetId -> Maybe A.NetId -> ExtractM A.NetId
muxNetsOpt c t e = do
    t' <- maybe (buildDummyNet "<undef>") return t
    e' <- maybe (buildDummyNet "<undef>") return e
    muxNets c t' e'

doConnect :: Value -> Value -> ExtractM (Map A.NetId (Maybe A.NetId))
doConnect l (VDff r _) = doConnect l r
doConnect (VDff _ l) r = doConnect l r
doConnect (VNet l) (VNet r) = return $ M.singleton l (Just r)
doConnect (VVector l) (VVector r) = return $ M.singleton l (Just r)
doConnect (VBundle lf) (VBundle rf) =
    liftM mconcat $
    mapM (\(a, b) -> doConnect a b) $
    M.elems $
    M.intersectionWithKey (\k (l, lFlip) (r, rFlip) ->
        (if lFlip /= rFlip then
            traceShow ("flip mismatch on field", k, lFlip, rFlip)
        else id) $ if lFlip then (r, l) else (l, r)) lf rf
doConnect lv rv = traceShow ("error: bad connection", lv, rv) $ return M.empty

doDisconnect :: Value -> ExtractM (Map A.NetId (Maybe A.NetId))
doDisconnect v = go v False
  where
    go :: Value -> Bool -> ExtractM (Map A.NetId (Maybe A.NetId))
    go (VNet l) f = return $ if f then M.empty else M.singleton l Nothing
    go (VVector l) f = return $ if f then M.empty else M.singleton l Nothing
    go (VBundle lf) f =
        liftM mconcat $
        mapM (\(l, lFlip) -> go l (if lFlip then not f else f)) $
        M.elems lf
    go (VDff _ l) f = go l f
    go (VRam idx ty) _ = do
        traceShowM ("error: tried to disconnect RAM", idx, ty)
        return M.empty
    go VError _ = return M.empty

makeConnections :: Map A.NetId (Maybe A.NetId) -> ExtractM ()
makeConnections m = forM_ (M.toList m) $ \(l, optR) -> case optR of
    Just r -> void $ buildNetAlias r l
    Nothing -> return ()

nameValue :: Text -> Value -> ExtractM Value
nameValue name v = go name v False
  where
    -- `nameValue` is used on the RHS of `wire` statements.  For unflipped leaf
    -- nodes, we alias the original net `n` to a new, named net `n'`.  For
    -- flipped nodes, we do the aliasing the other way around - `n'` flows into
    -- `n`.
    go name (VNet n) f = do
        ty <- use $ _esCurModule . A._moduleNet n . A._netTy
        n' <- buildNet' name 5 ty
        if f then buildNetAlias n' n else buildNetAlias n n'
        return $ VNet n'
    go name (VVector n) f = do
        ty <- use $ _esCurModule . A._moduleNet n . A._netTy
        n' <- buildNet' name 5 ty
        if f then buildNetAlias n' n else buildNetAlias n n'
        return $ VVector n'
    go name (VBundle fs) f' = do
        VBundle <$> M.traverseWithKey (\k (v, f) ->
            go (name <> "." <> k) v (f /= f') >>= \v' -> return (v', f)) fs
    go name (VDff r l) True =
        traceShow ("impossible: naming VDff in flipped context?", r, l) $
        return $ VDff r l
    go name (VDff r l) False = do
        r' <- go name r False
        l' <- go name l True
        return $ VDff r l
    go _ (VRam idx ty) _ = return $ VRam idx ty
    go _ VError _ = return VError
