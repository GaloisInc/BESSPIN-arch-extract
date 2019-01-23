{-# LANGUAGE RankNTypes #-}
module BESSPIN.ArchExtract.Extract where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.Array.ST
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

import Debug.Trace

import BESSPIN.ArchExtract.Architecture
import qualified BESSPIN.ArchExtract.Architecture as A
import BESSPIN.ArchExtract.Verilog
import qualified BESSPIN.ArchExtract.Verilog as V


extractArch :: V.Design -> A.Design
extractArch vDes =
    mapMods (\mod ->
        --filterLogics (\_ _ -> False) $
        --filterInsts (\_ i ->
            --not $ modInstName i `elem` map T.pack ["ra1reg"]) $
        --filterInsts (\_ i ->
        --    not $ modDeclName (designMods d `S.index` modInstId i)
        --        `elem` map T.pack ["mux2", "eqcmp"]) $
        filterInstsToLogic (\_ i ->
            not $ modDeclName (designMods d `S.index` modInstId i)
                --`elem` []) $
                `elem` map T.pack ["mux2", "eqcmp"]) $
                --`elem` map T.pack ["mux2", "eqcmp", "flopenr", "flopenrc"]) $
            --`elem` map T.pack ["adder"]) mod) $
        mod) $
    d
  where
    vMods = V.designModules vDes
    d = A.Design $ fmap (extractMod vMods) vMods

extractMod :: Seq V.Module -> V.Module -> A.ModDecl
extractMod vMods vMod =
    --filterLogics (\_ _ -> False) $
    filterLogics (\_ l -> logicKind l /= LkNetAlias) $
    -- Break up "uninteresting" nets early, before they can get merged with
    -- other nets.
    disconnect (\_ _ _ net ->
        let baseName = last $ T.splitOn (T.singleton '.') (netName net) in
        not $ baseName `elem` map T.pack ["clock", "clk", "reset"]) $
    -- Convert instantiations of undefined modules to logic.  (Otherwise we hit
    -- errors when trying to look up the port definitions.)
    filterInstsToLogic (\_ i -> modInstId i /= -1) $
    reconnectNets $
    A.ModDecl (moduleName vMod) inputs outputs insts logics nets
  where
    --instMap = buildInstMap interMap (moduleItems vMod)
    netParts = moduleNets vMods vMod
    (nets, netMap) = buildNets netParts
    (inputs, outputs) = convPorts netMap vMod
    insts = convInsts vMods netMap vMod
    logics = convItems vMods netMap vMod

mapMods :: (A.ModDecl -> A.ModDecl) -> A.Design -> A.Design
mapMods f d = A.Design $ fmap f $ designMods d


-- Building `modDeclNets` from `V.moduleDecls`

-- Info used to build a `Net` and its corresponding `NetMap` entry.
data NetParts = NetParts
    { netPartsName :: Text
    , netPartsNamePriority :: Int
    , netPartsOrigin :: NetOrigin
    }
    deriving (Show)

data NetOrigin =
    NoDecl { noDeclId :: Int } |
    -- noDeclId: index of the `InstDecl` in `moduleDecls`
    -- noPortIdx: index of the port in the instantiated module's `modulePorts`
    NoInstPort { noDeclId :: Int, noPortIdx :: Int }
    deriving (Show, Eq, Ord)

type NetMap = Map NetOrigin NetId

buildNets :: [NetParts] -> (Seq A.Net, NetMap)
buildNets nps = foldl f (S.empty, M.empty) nps
  where
    f (nets, netMap) (NetParts name prio origin) =
        let netId = NetId $ S.length nets in
        (nets |> Net name prio S.empty S.empty,
            M.insert origin netId netMap)

prioExtPort = 3
prioInstPort = 2
prioWire = 4
prioDcNet = -1

declNet :: Seq V.Module -> Int -> V.Decl -> [NetParts]
declNet vMods i (V.PortDecl name _) = [NetParts name prioExtPort $ NoDecl i]
declNet vMods i (V.ParamDecl name) = []
declNet vMods i (V.VarDecl name) = [NetParts name prioWire $ NoDecl i]
declNet vMods i (V.InstDecl name modId _) =
    let vMod = vMods `S.index` modId in
    zipWith (\j declId ->
            let portName = V.declName $ moduleDecls vMod `S.index` declId in
            NetParts (name <> T.pack "." <> portName) prioInstPort (NoInstPort i j))
        [0..] (modulePorts vMod)

moduleNets :: Seq V.Module -> V.Module -> [NetParts]
moduleNets vMods vMod = S.foldMapWithIndex (declNet vMods) (moduleDecls vMod)


-- Building `modDeclInputs`, `modDeclOutputs`, and `modDeclInsts` from
-- `V.moduleDecls`.

convPorts :: NetMap -> V.Module -> (Seq A.PortDecl, Seq A.PortDecl)
convPorts netMap vMod =
    mconcat $ map (\i ->
        let vPort = moduleDecls vMod `S.index` i in
        let port = A.PortDecl (V.declName vPort) (netMap M.! NoDecl i) in
        case portDeclDir vPort of
            V.Input -> (S.singleton port, S.empty)
            V.Output -> (S.empty, S.singleton port)
            V.InOut -> (S.singleton port, S.singleton port)
        ) (modulePorts vMod)

declInst :: Seq V.Module -> NetMap -> Int -> V.Decl -> Seq A.ModInst
declInst vMods netMap i (V.InstDecl name modId _) =
    let vMod = vMods `S.index` modId in
    let ins = [netMap M.! NoInstPort i j | (j, portIdx) <- zip [0..] (modulePorts vMod),
            portDeclDir (moduleDecls vMod `S.index` portIdx) `elem` [V.Input, V.InOut]] in
    let outs = [netMap M.! NoInstPort i j | (j, portIdx) <- zip [0..] (modulePorts vMod),
            portDeclDir (moduleDecls vMod `S.index` portIdx) `elem` [V.Output, V.InOut]] in
    S.singleton $ ModInst modId name (S.fromList ins) (S.fromList outs)
declInst _ _ _ _ = S.empty

convInsts :: Seq V.Module -> NetMap -> V.Module -> Seq A.ModInst
convInsts vMods netMap vMod = S.foldMapWithIndex (declInst vMods netMap) (moduleDecls vMod)


-- Building `modDeclLogics` from `V.moduleItems`

-- Get the origin info for every net used in `e`.
exprVars :: Expr -> Seq NetOrigin
exprVars e = go e
  where
    go (Var defId) = S.singleton $ NoDecl defId
    go (Index base idx) = go base <> goIdx idx
    go (MemIndex base idxs) = go base <> mconcat (map goIdx idxs)
    go (Const _) = S.empty
    go (Concat es) = mconcat (map go es)
    go (MultiConcat rep es) = go rep <> mconcat (map go es)
    go (IfExpr cond then_ else_) = go cond <> go then_ <> go else_
    go (Unary arg) = go arg
    go (Binary left right) = go left <> go right
    go (Field base _) = go base
    go (AssignPat rep es) = go rep <> mconcat (map go es)
    go UnknownExpr = S.empty

    goIdx (ISingle e) = go e
    goIdx (IRange l r) = go l <> go r

-- Get the `LogicKind` for an rvalue expression.
rvalKind :: Expr -> LogicKind
rvalKind (Var defId) = LkNetAlias
rvalKind _ = LkOther

mkLogic :: NetMap -> LogicKind -> Seq NetOrigin -> Seq NetOrigin -> Logic
mkLogic netMap kind ins outs = Logic kind (foldMap f ins) (foldMap f outs)
  where
    f def = case M.lookup def netMap of
        Nothing -> S.empty
        Just n -> S.singleton n

stmtLogic :: NetMap -> Stmt -> Seq Logic
stmtLogic netMap s = go S.empty s
  where
    -- `imp`: Implicit inputs to any generated `Logic`s, resulting from
    -- the conditions on enclosing `if`s and such.
    go :: Seq NetOrigin -> Stmt -> Seq Logic
    go imp (If cond then_ else_) =
        let imp' = imp <> exprVars cond in
        foldMap (go imp') then_ <> maybe S.empty (foldMap $ go imp') else_
    go imp (Case cond cases) =
        let imp' = imp <> exprVars cond in
        foldMap (\(_, ss) -> foldMap (go imp') ss) cases
    go imp (For inits cond steps body) =
        -- Not sure how much of `For` we really need to handle, but here's a
        -- conservative guess...
        let imp' = imp <> exprVars cond in
        foldMap (go imp) inits <>
        foldMap (go imp') steps <>
        foldMap (go imp') body
    go imp (NonBlockingAssign lval rval) =
        S.singleton $ mkLogic netMap LkOther (imp <> exprVars rval) (exprVars lval)
    go imp (BlockingAssign lval rval) =
        S.singleton $ mkLogic netMap LkOther (imp <> exprVars rval) (exprVars lval)
    go imp (BlockingUpdate lval) =
        -- Something like `i++`, where the "lvalue" is also used as part of the
        -- "rvalue".
        S.singleton $ mkLogic netMap LkOther (imp <> exprVars lval) (exprVars lval)

itemLogic :: Seq V.Module -> Seq V.Decl -> NetMap -> V.Item -> Seq Logic
itemLogic vMods vDecls netMap i = go i
  where
    go (InitVar varId e) = S.singleton $
        mkLogic netMap (rvalKind e) (exprVars e) (S.singleton $ NoDecl varId)
    go (InitInst i conns) = mconcat $ zipWith3 f [0..] (modulePorts vMod) conns
      where
        inst = vDecls `S.index` i
        vMod = vMods `S.index` instanceModId inst
        f j portIdx conn =
            let portNet = S.singleton $ NoInstPort i j in
            let connNets = exprVars conn in
            let inLogic = S.singleton $
                    mkLogic netMap (rvalKind conn) connNets portNet in
            let outLogic = S.singleton $
                    mkLogic netMap LkNetAlias portNet connNets in
            case V.portDeclDir $ moduleDecls vMod `S.index` portIdx of
                Input -> inLogic
                Output -> outLogic
                InOut -> inLogic <> outLogic
    go (ContAssign l r) = S.singleton $
        mkLogic netMap (rvalKind r) (exprVars r) (exprVars l)
    go (Always ss) = foldMap (stmtLogic netMap) ss
    go (Initial ss) = foldMap (stmtLogic netMap) ss

convItems :: Seq V.Module -> NetMap -> V.Module -> Seq Logic
convItems vMods netMap vMod =
    mconcat $ map (itemLogic vMods (moduleDecls vMod) netMap) $ moduleItems vMod


-- Rebuild the `netSources` and `netSinks` connection lists for all nets in
-- `mod`, based on the `Inputs` and `Outputs` lists for the module itself and
-- all its `ModInst`s and `Logic`s.
reconnectNets :: A.ModDecl -> A.ModDecl
reconnectNets mod = runST $ do
    sources <- newArray (NetId 0, NetId $ S.length (modDeclNets mod) - 1) S.empty
    sinks <- newArray (NetId 0, NetId $ S.length (modDeclNets mod) - 1) S.empty

    connect (modDeclInputs mod) portDeclNet sources ExtPort
    connect (modDeclOutputs mod) portDeclNet sinks ExtPort
    void $ flip S.traverseWithIndex (modDeclInsts mod) $ \i inst -> do
        connect (modInstInputs inst) id sinks (InstPort i)
        connect (modInstOutputs inst) id sources (InstPort i)
    void $ flip S.traverseWithIndex (modDeclLogics mod) $ \i inst -> do
        connect (logicInputs inst) id sinks (LogicPort i)
        connect (logicOutputs inst) id sources (LogicPort i)

    nets' <- flip S.traverseWithIndex (modDeclNets mod) $ \i net -> do
        netSources <- readArray sources (NetId i)
        netSinks <- readArray sinks (NetId i)
        return $ net { netSources = netSources, netSinks = netSinks }
    return $ mod { modDeclNets = nets' }

  where
    connect :: Seq a -> (a -> NetId) -> STArray s NetId (Seq Conn) -> (Int -> Conn) -> ST s ()
    connect ports portNet netConns mkConn =
        void $ flip S.traverseWithIndex ports $ \i p -> do
            let netId = portNet p
            conns <- readArray netConns netId
            let conns' = conns |> mkConn i
            writeArray netConns netId conns'

-- Rewrite all `NetId`s in the tree using the provided function.
mapNetIds :: (NetId -> NetId) -> A.ModDecl -> A.ModDecl
mapNetIds f mod = everywhere (mkT f) mod

-- Delete all `Net`s that fail the predicate `f`.  This changes the contents of
-- `modDeclNets`, so `NetId`s may change.  Raises an error if any of the
-- deleted nets is currently in use.
filterNets :: (NetId -> Net -> Bool) -> A.ModDecl -> A.ModDecl
filterNets f mod = mod' { modDeclNets = nets' }
  where
    -- Compute the new list of nets, along with the mapping from old `NetId`s
    -- to new ones.
    (nets', idMap) = S.foldlWithIndex g (S.empty, M.empty) (modDeclNets mod)
      where
        g (nets, idMap) idx net =
            if f (NetId idx) net then
                (nets |> net, M.insert (NetId idx) (NetId (S.length nets)) idMap)
            else
                (nets, idMap)

    mod' = flip mapNetIds mod $ \netId -> case M.lookup netId idMap of
        Just newId -> newId
        Nothing ->
            let name = netName $ modDeclNets mod `S.index` unwrapNetId netId in
            error $ "net " ++ show (T.unpack name) ++
                " (" ++ show netId ++ ") was deleted while still in use"

-- Remove each `LkNetAlias` `Logic` from `mod` by replacing one of its aliased
-- nets with the other.
mergeAliasedNets :: A.ModDecl -> A.ModDecl
mergeAliasedNets mod = filterLogics (\_ l -> logicKind l /= LkNetAlias) mod

-- Delete logic items rejected by predicate `f`.  When a logic item is deleted,
-- all of its input and output nets are merged into a single net, indicating
-- that data can flow freely from any input to any output.
filterLogics :: (Int -> Logic -> Bool) -> A.ModDecl -> A.ModDecl
filterLogics f mod =
    runNetMerge act (mod { modDeclLogics = logics' })
  where
    logics' = filterWithIndex f $ modDeclLogics mod
    act = do
        void $ flip S.traverseWithIndex (modDeclLogics mod) $ \idx l -> do
            when (not $ f idx l) $ do
                mergeNetSeq $ logicInputs l <> logicOutputs l

-- Delete module instantiations rejected by predicate `f`.  When an
-- instantiation is deleted, all of its input and output nets are merged into a
-- single net, indicating that data can flow freely from any input to any
-- output.
filterInsts :: (Int -> ModInst -> Bool) -> A.ModDecl -> A.ModDecl
filterInsts f mod =
    runNetMerge act (mod { modDeclInsts = insts' })
  where
    insts' = filterWithIndex f $ modDeclInsts mod
    act = do
        void $ flip S.traverseWithIndex (modDeclInsts mod) $ \idx inst -> do
            when (not $ f idx inst) $ do
                traceShow ("merge nets",
                    fmap (\id -> T.unpack $ netName $ modDeclNets mod `S.index` unwrapNetId id) $
                        modInstInputs inst <> modInstOutputs inst) $
                    mergeNetSeq $ modInstInputs inst <> modInstOutputs inst


-- Remove module instantiations rejected by predicate `f`, replacing them with
-- `Logic`s connected to the same nets.
filterInstsToLogic :: (Int -> ModInst -> Bool) -> A.ModDecl -> A.ModDecl
filterInstsToLogic f mod =
    reconnectNets $ mod { modDeclInsts = insts', modDeclLogics = logics' }
  where
    (insts', rejectedInsts) = S.foldMapWithIndex (\idx inst ->
            if f idx inst then
                (S.singleton inst, S.empty)
            else
                (S.empty, S.singleton inst))
        (modDeclInsts mod)
    logics' = modDeclLogics mod <> fmap instToLogic rejectedInsts
    instToLogic (A.ModInst _ _ ins outs) = Logic LkOther ins outs


-- Disconnect all connection points selected by predicate `f` from their
-- current nets.  Since each connection point must be connected to some net,
-- this function adds a new, otherwise-empty net for each disconnected `Conn`.
disconnect :: (Conn -> Side -> NetId -> Net -> Bool) -> A.ModDecl -> A.ModDecl
disconnect f mod = reconnectNets $ mod' { modDeclNets = nets' }
  where
    nets = modDeclNets mod
    (mod', nets') = runState (goMod mod) nets

    goMod :: A.ModDecl -> NetM A.ModDecl
    goMod (ModDecl name ins outs insts logics nets) =
        ModDecl <$> pure name <*> goPorts Source ins <*> goPorts Sink outs <*>
            goInsts insts <*> goLogics logics <*> pure nets

    goPorts side ports = S.traverseWithIndex (goPort side) ports
    goPort side idx (A.PortDecl name netId) =
        A.PortDecl <$> pure name <*>
            goNet (ExtPort idx) side netId (extPortName side name idx netId)

    goInsts :: Seq A.ModInst -> NetM (Seq A.ModInst)
    goInsts insts = S.traverseWithIndex goInst insts
    goInst idx (A.ModInst modId name ins outs) =
        A.ModInst <$> pure modId <*> pure name <*>
            goItemPorts Sink (InstPort idx) (instPortName name) ins <*>
            goItemPorts Source (InstPort idx) (instPortName name) outs
    goLogics :: Seq A.Logic -> NetM (Seq A.Logic)
    goLogics logics = S.traverseWithIndex goLogic logics
    goLogic idx (A.Logic kind ins outs) =
        A.Logic <$> pure kind <*>
            goItemPorts Sink (LogicPort idx) logicPortName ins <*>
            goItemPorts Source (LogicPort idx) logicPortName outs

    -- Handles both ModInst and Logic ports.
    goItemPorts :: Side -> (Int -> Conn) -> (Side -> Int -> NetId -> Text) ->
        Seq NetId -> NetM (Seq NetId)
    goItemPorts side mkConn mkName netIds =
        S.traverseWithIndex (goItemPort side mkConn mkName) netIds
    goItemPort side mkConn mkName idx netId =
        let conn = mkConn idx in
        let name = mkName side idx netId in
        goNet conn side netId name

    goNet conn side netId name =
        if f conn side netId (nets `S.index` unwrapNetId netId) then
            return netId
        else
            traceShow ("replace net", netName $ nets `S.index` unwrapNetId netId,
                netId, "with", name, "at", conn, side) $
            mkNet name prioDcNet

    dcName netId base = T.pack "dc$" <> base <> T.singleton '$' <>
        T.pack (show $ unwrapNetId netId)
    dotted parts = T.intercalate (T.singleton '.') parts
    extPortName side name idx netId =
        dcName netId $ dotted [T.pack "ext", T.pack $ show side, T.pack $ show idx, name]
    instPortName instName side portIdx netId =
        dcName netId $ dotted [instName, T.pack $ show side, T.pack $ show portIdx]
    logicPortName side portIdx netId =
        dcName netId $ dotted [T.pack "logic", T.pack $ show side, T.pack $ show portIdx]



type NetM a = State (Seq Net) a

mkNet :: Text -> Int -> NetM NetId
mkNet name prio = state $ \nets ->
    (NetId $ S.length nets, nets |> Net name prio S.empty S.empty)
    


type NetMergeLabel = Set (Int, NetId)
type NetMergePoint s = UF.Point s NetMergeLabel
type NetMergeM s a = StateT (STArray s NetId (NetMergePoint s)) (ST s) a

mergeNets :: NetId -> NetId -> NetMergeM s ()
mergeNets id1 id2 = do
    arr <- get
    pt1 <- lift $ readArray arr id1
    pt2 <- lift $ readArray arr id2
    lift $ UF.union' pt1 pt2 (\a b -> return $ a <> b)

mergeNetSeq :: Seq NetId -> NetMergeM s ()
mergeNetSeq ids = case S.viewl ids of
    S.EmptyL -> return ()
    id1 S.:< ids -> forM_ ids $ \id2 -> mergeNets id1 id2

runNetMerge :: (forall s. NetMergeM s ()) -> A.ModDecl -> A.ModDecl
runNetMerge m mod =
    reconnectNets $
    filterNets (\netId net -> substMap M.! netId == netId) $
    renameNets $
    mapNetIds (\netId -> substMap M.! netId) $
    mod
  where
    -- Map each net's ID to the ID of its replacement.  For nets that haven't
    -- been merged with anything, this maps the ID to itself.
    substMap :: Map NetId NetId
    -- Map from each non-replaced net's ID to its new name.
    nameMap :: Map NetId Text
    (substMap, nameMap) = runST $ do
        -- Build an array, containing one union-find "point" for each net.  The
        -- point descriptors are (-prio, id), so that taking the `min` of two
        -- descriptors gives the higher-priority one, or (in case of a tie) the
        -- one with higher ID.
        arr <- newArray_ (NetId 0, NetId $ S.length (modDeclNets mod) - 1)
            :: ST s (STArray s NetId (NetMergePoint s))
        (lo, hi) <- getBounds arr
        forM_ [lo .. hi] $ \netId -> do
            let net = modDeclNets mod `S.index` unwrapNetId netId
            pt <- UF.fresh $ Set.singleton (- netNamePriority net, netId)
            writeArray arr netId pt

        -- Run user action to merge some nets together.
        evalStateT m arr

        -- Build a map from each net to its union-find representative.
        substMap <- M.fromList <$> forM [lo .. hi] (\oldId -> do
            pt <- readArray arr oldId
            desc <- UF.descriptor pt
            let newId = snd $ Set.findMin desc
            return (oldId, newId))

        nameMap <- mconcat <$> forM [lo .. hi] (\oldId -> do
            pt <- readArray arr oldId
            desc <- UF.descriptor pt
            let newId = snd $ Set.findMin desc
            if newId == oldId then
                let newName = T.unlines $
                        map (\(_,id) -> netName $
                            modDeclNets mod `S.index` unwrapNetId id) $
                        Set.toList desc in
                return $ M.singleton newId newName
            else
                return M.empty
            )

        return (substMap, nameMap)

    renameNet idx n =
        let newName = case M.lookup (NetId idx) nameMap of
                Nothing -> netName n
                Just name -> name
        in
        n { netName = newName }

    renameNets mod = mod { modDeclNets = S.mapWithIndex renameNet $ modDeclNets mod }


filterWithIndex :: (Int -> a -> Bool) -> Seq a -> Seq a
filterWithIndex f s =
    S.foldMapWithIndex (\i x -> if f i x then S.singleton x else S.empty) s
