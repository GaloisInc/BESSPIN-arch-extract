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


extractArch :: [V.ModuleDecl] -> A.Design
extractArch vMods = A.Design $ S.fromList $ map (extractMod interMap) vMods
  where
    interMap = buildInterMap vMods

traceNets desc mod = trace (" ==== " ++ desc ++ " nets ====\n" ++ s) mod
  where
    s = concat $ map (\x -> show x ++ "\n") $ foldr (:) [] $ modDeclNets mod

extractMod :: InterMap -> V.ModuleDecl -> A.ModDecl
extractMod interMap vMod =
    --mergeAliasedNets $
    filterLogics (\_ _ -> False) $
    reconnectNets $
    A.ModDecl (moduleName vMod) inputs outputs insts logics nets
  where
    instMap = buildInstMap interMap (moduleItems vMod)
    netParts = moduleNets instMap vMod
    (nets, netMap) = buildNets netParts
    (inputs, outputs) = convModulePorts netMap vMod
    insts = convModuleInsts instMap netMap vMod
    logics = convModuleLogic netMap vMod




-- Public interface information, used when processing `ModInst`s.
data ModInter = ModInter
    { modInterId :: A.ModId
    , modInterPorts :: [V.PortDecl]
    }
    deriving (Show)

type InterMap = Map V.NodeId ModInter

buildInterMap :: [V.ModuleDecl] -> InterMap
buildInterMap mods = M.fromList $ zipWith f [0..] mods
  where
    f modId vMod = (moduleId vMod, ModInter modId (modulePorts vMod))


-- Port resolution data for module instances.

-- A map from `Instance.modItemId` to the `ModInter` for the instantiated
-- module.  This map always includes every `Instance` in the current module.
type InstMap = Map V.NodeId ModInter

buildInstMap :: InterMap -> [V.ModItem] -> InstMap
buildInstMap interMap items = M.fromList $ mapMaybe go items
  where
    go :: V.ModItem -> Maybe (V.NodeId, ModInter)
    go (Instance instId vModId name _ _) =
        case M.lookup vModId interMap of
            Nothing ->
                trace ("no declaration found for module instantiation " ++ T.unpack name ++
                    " (node " ++ show vModId ++ ")") $
                Just (instId, ModInter (-1) [])
            Just inter -> Just (instId, inter)
    go _ = Nothing


-- Info used to build a `Net` and its corresponding `NetMap` entry.
data NetParts = NetParts
    { netPartsName :: Text
    , netPartsNamePriority :: Int
    , netPartsOrigin :: NetOrigin
    }
    deriving (Show)

data NetOrigin = NoDef V.NodeId | NoInstPort V.NodeId Int
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
prioWire = 1

portNet :: V.PortDecl -> NetParts
portNet (V.PortDecl id name _ _) = NetParts name prioExtPort (NoDef id)

itemNets :: InstMap -> V.ModItem -> [NetParts]
itemNets instMap (Instance instId _ name _ _) = zipWith f (modInterPorts inter) [0..]
  where
    inter = instMap M.! instId
    f port idx = NetParts
        (name <> T.pack "." <> V.portDeclName port)
        prioInstPort
        (NoInstPort instId idx)
itemNets _ (VarDecl id name _ _ _) = [NetParts name prioWire (NoDef id)]
itemNets _ _ = []

moduleNets :: InstMap -> V.ModuleDecl -> [NetParts]
moduleNets instMap vMod =
    map portNet (modulePorts vMod) ++
    concat (map (itemNets instMap) (moduleItems vMod))


-- Module port handling

convPort :: NetMap -> V.PortDecl -> (Seq A.PortDecl, Seq A.PortDecl)
convPort netMap (V.PortDecl id name _ dir) =
    let port = A.PortDecl name (netMap M.! NoDef id) in
    case dir of
        Input -> (S.singleton port, S.empty)
        Output -> (S.empty, S.singleton port)
        InOut -> (S.singleton port, S.singleton port)

convPortVar :: NetMap -> V.ModItem -> (Seq A.PortDecl, Seq A.PortDecl)
convPortVar netMap (V.VarDecl id name _ dir _) =
    let port = A.PortDecl name (netMap M.! NoDef id) in
    case dir of
        Just Input -> (S.singleton port, S.empty)
        Just Output -> (S.empty, S.singleton port)
        Just InOut -> (S.singleton port, S.singleton port)
        Nothing -> (S.empty, S.empty)
convPortVar _ _ = (S.empty, S.empty)

convModulePorts :: NetMap -> V.ModuleDecl -> (Seq A.PortDecl, Seq A.PortDecl)
convModulePorts netMap vMod =
    mconcat (map (convPort netMap) $ modulePorts vMod) <>
    mconcat (map (convPortVar netMap) $ moduleItems vMod)


-- Module instantiation handling (`ModInst` only, no `Logic`)

convInst :: InstMap -> NetMap -> V.ModItem -> Maybe A.ModInst
convInst instMap netMap (Instance instId _ name _ portConns) =
    Just $ A.ModInst (modInterId inter) name inputs outputs
  where
    inter = instMap M.! instId
    (inputs, outputs) = mconcat $ zipWith3 f (modInterPorts inter) portConns [0..]
    f decl conn idx =
        let netId = netMap M.! NoInstPort instId idx in
        case portDeclDir decl of
            Input -> (S.singleton netId, S.empty)
            Output -> (S.empty, S.singleton netId)
            InOut -> (S.singleton netId, S.singleton netId)
convInst _ _ _ = Nothing

convModuleInsts :: InstMap -> NetMap -> V.ModuleDecl -> Seq A.ModInst
convModuleInsts instMap netMap vMod =
    S.fromList $ mapMaybe (convInst instMap netMap) $ moduleItems vMod


-- Logic element handling.  Includes logic statements, variable initializers,
-- and port connection expressions.

-- Get the origin info for every net used in `e`.
exprVars :: Expr -> Seq NetOrigin
exprVars e = go e
  where
    go (Var defId) = S.singleton $ NoDef defId
    go (Index base idx) = go base <> goIdx idx
    go (MemIndex base idxs) = go base <> mconcat (map goIdx idxs)
    go (Const _) = S.empty
    go (Concat es) = mconcat (map go es)
    go (MultiConcat rep es) = go rep <> mconcat (map go es)
    go (IfExpr cond then_ else_) = go cond <> go then_ <> go else_
    go (Unary _ arg) = go arg
    go (Binary _ left right) = go left <> go right
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
    go imp (Block ss) = mconcat (map (go imp) ss)
    go imp (If cond then_ else_) =
        let imp' = imp <> exprVars cond in
        go imp' then_ <> maybe S.empty (go imp') else_
    go imp (Case cond cases) =
        let imp' = imp <> exprVars cond in
        mconcat (map (go imp' . snd) cases)
    go imp (For inits cond steps body) =
        -- Not sure how much of `For` we really need to handle, but here's a
        -- conservative guess...
        let imp' = imp <> exprVars cond in
        mconcat (map (go imp') inits) <>
        mconcat (map (go imp') steps) <>
        go imp' body
    go imp (NonBlockingAssign lval rval) =
        S.singleton $ mkLogic netMap LkOther (imp <> exprVars rval) (exprVars lval)
    go imp (BlockingAssign lval rval) =
        S.singleton $ mkLogic netMap LkOther (imp <> exprVars rval) (exprVars lval)
    go imp (BlockingUpdate lval _) =
        -- Something like `i++`, where the "lvalue" is also used as part of the
        -- "rvalue".
        S.singleton $ mkLogic netMap LkOther (imp <> exprVars lval) (exprVars lval)
    go imp NullStmt = S.empty
    go imp UnknownStmt = S.empty

itemLogic :: NetMap -> ModItem -> Seq Logic
itemLogic netMap i = go i
  where
    go (Instance instId _ _ _ portConns) =
        S.fromList $ zipWith f portConns [0..]
      where
        f conn idx = mkLogic netMap
            (rvalKind conn) (exprVars conn) (S.singleton $ NoInstPort instId idx)
    go (VarDecl id _ _ _ init) = case init of
        Nothing -> S.empty
        Just e -> S.singleton $
            mkLogic netMap (rvalKind e) (exprVars e) (S.singleton $ NoDef id)
    go (ContAssign _ lval rval) = S.singleton $
        mkLogic netMap (rvalKind rval) (exprVars rval) (exprVars lval)
    go (Always s) = stmtLogic netMap s
    go (Initial s) = stmtLogic netMap s

convModuleLogic :: NetMap -> V.ModuleDecl -> Seq Logic
convModuleLogic netMap vMod = mconcat $ map (itemLogic netMap) $ moduleItems vMod


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


type NetMergeM s a = StateT (STArray s NetId (UF.Point s (Int, NetId))) (ST s) a

mergeNets :: NetId -> NetId -> NetMergeM s ()
mergeNets id1 id2 = do
    arr <- get
    pt1 <- lift $ readArray arr id1
    pt2 <- lift $ readArray arr id2
    lift $ UF.union' pt1 pt2 (\a b -> return $ min a b)

mergeNetSeq :: Seq NetId -> NetMergeM s ()
mergeNetSeq ids = case S.viewl ids of
    S.EmptyL -> return ()
    id1 S.:< ids -> forM_ ids $ \id2 -> mergeNets id1 id2

runNetMerge :: (forall s. NetMergeM s ()) -> A.ModDecl -> A.ModDecl
runNetMerge m mod =
    reconnectNets $
    filterNets (\netId net -> substMap M.! netId == netId) $
    mapNetIds (\netId -> substMap M.! netId) $
    mod
  where
    -- Map each net's ID to the ID of its replacement.  For nets that haven't
    -- been merged with anything, this maps the ID to itself.
    substMap :: Map NetId NetId
    substMap = runST $ do
        -- Build an array, containing one union-find "point" for each net.  The
        -- point descriptors are (-prio, id), so that taking the `min` of two
        -- descriptors gives the higher-priority one, or (in case of a tie) the
        -- one with higher ID.
        arr <- newArray_ (NetId 0, NetId $ S.length (modDeclNets mod) - 1)
            :: ST s (STArray s NetId (UF.Point s (Int, NetId)))
        (lo, hi) <- getBounds arr
        forM_ [lo .. hi] $ \netId -> do
            let net = modDeclNets mod `S.index` unwrapNetId netId
            pt <- UF.fresh (- netNamePriority net, netId)
            writeArray arr netId pt

        -- Run user action to merge some nets together.
        evalStateT m arr

        -- Build a map from each net to its union-find representative.
        M.fromList <$> forM [lo .. hi] (\oldId -> do
            pt <- readArray arr oldId
            (_, newId) <- UF.descriptor pt
            return (oldId, newId))


filterWithIndex :: (Int -> a -> Bool) -> Seq a -> Seq a
filterWithIndex f s =
    S.foldMapWithIndex (\i x -> if f i x then S.singleton x else S.empty) s
