{-# LANGUAGE RankNTypes #-}
module BESSPIN.ArchExtract.Simplify where

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


mapMods :: (ModDecl -> ModDecl) -> Design -> Design
mapMods f d = Design $ fmap f $ designMods d


-- Rebuild the `netSources` and `netSinks` connection lists for all nets in
-- `mod`, based on the `Inputs` and `Outputs` lists for the module itself and
-- all its `ModInst`s and `Logic`s.
reconnectNets :: ModDecl -> ModDecl
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
mapNetIds :: (NetId -> NetId) -> ModDecl -> ModDecl
mapNetIds f mod = everywhere (mkT f) mod

-- Delete all `Net`s that fail the predicate `f`.  This changes the contents of
-- `modDeclNets`, so `NetId`s may change.  Raises an error if any of the
-- deleted nets is currently in use.
filterNets :: (NetId -> Net -> Bool) -> ModDecl -> ModDecl
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
mergeAliasedNets :: ModDecl -> ModDecl
mergeAliasedNets mod = filterLogics (\_ l -> logicKind l /= LkNetAlias) mod

-- Delete logic items rejected by predicate `f`.  When a logic item is deleted,
-- all of its input and output nets are merged into a single net, indicating
-- that data can flow freely from any input to any output.
filterLogics :: (Int -> Logic -> Bool) -> ModDecl -> ModDecl
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
filterInsts :: (Int -> ModInst -> Bool) -> ModDecl -> ModDecl
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
filterInstsToLogic :: (Int -> ModInst -> Bool) -> ModDecl -> ModDecl
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
    instToLogic (ModInst _ _ ins outs) = Logic LkOther ins outs


-- Disconnect all connection points selected by predicate `f` from their
-- current nets.  Since each connection point must be connected to some net,
-- this function adds a new, otherwise-empty net for each disconnected `Conn`.
disconnect :: (Conn -> Side -> NetId -> Net -> Bool) -> ModDecl -> ModDecl
disconnect f mod = reconnectNets $ mod' { modDeclNets = nets' }
  where
    nets = modDeclNets mod
    (mod', nets') = runState (goMod mod) nets

    goMod :: ModDecl -> NetM ModDecl
    goMod (ModDecl name ins outs insts logics nets) =
        ModDecl <$> pure name <*> goPorts Source ins <*> goPorts Sink outs <*>
            goInsts insts <*> goLogics logics <*> pure nets

    goPorts side ports = S.traverseWithIndex (goPort side) ports
    goPort side idx (PortDecl name netId) =
        PortDecl <$> pure name <*>
            goNet (ExtPort idx) side netId (extPortName side name idx netId)

    goInsts :: Seq ModInst -> NetM (Seq ModInst)
    goInsts insts = S.traverseWithIndex goInst insts
    goInst idx (ModInst modId name ins outs) =
        ModInst <$> pure modId <*> pure name <*>
            goItemPorts Sink (InstPort idx) (instPortName name) ins <*>
            goItemPorts Source (InstPort idx) (instPortName name) outs
    goLogics :: Seq Logic -> NetM (Seq Logic)
    goLogics logics = S.traverseWithIndex goLogic logics
    goLogic idx (Logic kind ins outs) =
        Logic <$> pure kind <*>
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

prioDcNet = -1



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

runNetMerge :: (forall s. NetMergeM s ()) -> ModDecl -> ModDecl
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
