{-# LANGUAGE RankNTypes #-}
module BESSPIN.ArchExtract.Simplify where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.Array.ST
import Data.Foldable
import Data.Function (on)
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


mapMods :: (Module a -> Module a) -> Design a -> Design a
mapMods f d = Design $ fmap f $ designMods d


-- Rebuild the `netSources` and `netSinks` connection lists for all nets in
-- `mod`, based on the `Inputs` and `Outputs` lists for the module itself and
-- all its `Logic`s.
reconnectNets :: Module a -> Module a
reconnectNets mod = runST $ do
    sources <- newArray (NetId 0, NetId $ S.length (moduleNets mod) - 1) S.empty
    sinks <- newArray (NetId 0, NetId $ S.length (moduleNets mod) - 1) S.empty

    connect (moduleInputs mod) portNet sources ExtPort
    connect (moduleOutputs mod) portNet sinks ExtPort
    void $ flip S.traverseWithIndex (moduleLogics mod) $ \i inst -> do
        connect (logicInputs inst) pinNet sinks (LogicPort i)
        connect (logicOutputs inst) pinNet sources (LogicPort i)

    nets' <- flip S.traverseWithIndex (moduleNets mod) $ \i net -> do
        netSources <- readArray sources (NetId i)
        netSinks <- readArray sinks (NetId i)
        return $ net { netSources = netSources, netSinks = netSinks }
    return $ mod { moduleNets = nets' }

  where
    connect :: Seq a -> (a -> NetId) -> STArray s NetId (Seq Conn) -> (Int -> Conn) -> ST s ()
    connect ports portNet netConns mkConn =
        void $ flip S.traverseWithIndex ports $ \i p -> do
            let netId = portNet p
            conns <- readArray netConns netId
            let conns' = conns |> mkConn i
            writeArray netConns netId conns'

-- Rewrite all `NetId`s in the tree using the provided function.
mapNetIds :: (NetId -> NetId) -> Module a -> Module a
mapNetIds f mod =
    let anns = gatherAnn mod
        mod' = everywhere (mkT f) $ constAnn () mod in
    scatterAnn anns mod'

-- Delete all `Net`s that fail the predicate `f`.  This changes the contents of
-- `moduleNets`, so `NetId`s may change.  Raises an error if any of the
-- deleted nets is currently in use.
filterNets :: (NetId -> Net a -> Bool) -> Module a -> Module a
filterNets f mod = mod' { moduleNets = nets' }
  where
    -- Compute the new list of nets, along with the mapping from old `NetId`s
    -- to new ones.
    (nets', idMap) = S.foldlWithIndex g (S.empty, M.empty) (moduleNets mod)
      where
        g (nets, idMap) idx net =
            if f (NetId idx) net then
                (nets |> net, M.insert (NetId idx) (NetId (S.length nets)) idMap)
            else
                (nets, idMap)

    mod' = flip mapNetIds mod $ \netId -> case M.lookup netId idMap of
        Just newId -> newId
        Nothing ->
            let name = netName $ moduleNets mod `S.index` unwrapNetId netId in
            error $ "net " ++ show (T.unpack name) ++
                " (" ++ show netId ++ ") was deleted while still in use"

-- Disconnect all connection points selected by predicate `f` from their
-- current nets.  Since each connection point must be connected to some net,
-- this function adds a new, otherwise-empty net for each disconnected `Conn`.
disconnect :: Monoid a => (Conn -> Side -> NetId -> Net a -> Bool) -> Module a -> Module a
disconnect f mod = reconnectNets $ mod' { moduleNets = nets' }
  where
    nets = moduleNets mod
    (mod', nets') = runState (goMod mod) nets

    goMod :: Monoid a => Module a -> NetM a (Module a)
    goMod (Module name ins outs logics nets) =
        Module <$> pure name <*> goPorts Source ins <*> goPorts Sink outs <*>
            goLogics logics <*> pure nets

    goPorts side ports = S.traverseWithIndex (goPort side) ports
    goPort side idx (Port name netId ty) = Port
        <$> pure name
        <*> goNet (ExtPort idx) side netId (extPortName side name idx netId)
        <*> pure ty

    goPins idx side pins = S.traverseWithIndex (goPin idx side) pins
    goPin idx side pinIdx (Pin netId ty) = Pin
        <$> goNet (LogicPort idx pinIdx) side netId (logicPortName side pinIdx netId)
        <*> pure ty

    goLogics :: Monoid a => Seq (Logic a) -> NetM a (Seq (Logic a))
    goLogics logics = S.traverseWithIndex goLogic logics
    goLogic :: Monoid a => Int -> Logic a -> NetM a (Logic a)
    goLogic idx (Logic kind ins outs ann) =
        Logic <$> pure kind
            <*> goPins idx Sink ins
            <*> goPins idx Source outs
            <*> pure ann

    goNet conn side netId name =
        let net = nets `S.index` unwrapNetId netId in
        if f conn side netId net then
            return netId
        else
            mkNet name prioDcNet (netTy net)

    dcName netId base = T.pack "dc$" <> base <> T.singleton '$' <>
        T.pack (show $ unwrapNetId netId)
    dotted parts = T.intercalate (T.singleton '.') parts
    extPortName side name idx netId =
        dcName netId $ dotted [T.pack "ext", T.pack $ show side, T.pack $ show idx, name]
    logicPortName side portIdx netId =
        dcName netId $ dotted [T.pack "logic", T.pack $ show side, T.pack $ show portIdx]

prioDcNet = -1

type NetM a b = State (Seq (Net a)) b

mkNet :: Monoid a => Text -> Int -> Ty -> NetM a NetId
mkNet name prio ty = state $ \nets ->
    (NetId $ S.length nets, nets |> Net name prio S.empty S.empty ty mempty)


-- Remove each `LkNetAlias` `Logic` from `mod` by replacing one of its aliased
-- nets with the other.
mergeAliasedNets :: Module a -> Module a
mergeAliasedNets mod =
    substNetsByMap substMap revMap $
    filterLogics (\_ l -> logicKind l /= LkNetAlias) $
    mod
  where
    (substMap, revMap) = runNetMerge mod $ do
        forM_ (moduleLogics mod) $ \l ->
            when (logicKind l == LkNetAlias) $ do
                -- Sanity checks
                when (S.length (logicInputs l) /= 1 || S.length (logicOutputs l) /= 1) $
                    traceShowM ("warning: malformed net alias",
                        "inputs", logicInputs l, "outputs", logicOutputs l)
                let allNets = fmap pinNet $ logicInputs l <> logicOutputs l
                let tys = fmap (\i -> netTy $ mod `moduleNet` i) allNets
                when (not (S.null tys) && not (all (== tys `S.index` 0) tys)) $
                    traceShowM ("warning: illtyped net alias",
                        "nets", allNets, "tys", tys)
                -- Actually merge the nets
                mergeNetSeq allNets

-- Delete logic items rejected by predicate `f`.  Has no effect on nets, except
-- to disconnect them from each deleted logic.
filterLogics :: (Int -> Logic a -> Bool) -> Module a -> Module a
filterLogics f mod = mod { moduleLogics = filterWithIndex f $ moduleLogics mod }


type NetMergePoint s = UF.Point s (Seq NetId)
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

-- Replace nets as indicated by the `substMap` and `revMap` produced by
-- `runNetMerge`.
substNetsByMap :: Map NetId NetId -> Map NetId (Seq NetId) -> Module a -> Module a
substNetsByMap substMap revMap mod =
    reconnectNets $
    filterNets (\netId net -> substMap M.! netId == netId) $
    renameNets $
    mapNetIds (\netId -> substMap M.! netId) $
    mod
  where
    nameFromIds ids = name
      where
        cands = fmap (\i -> (i, mod `moduleNet` i)) ids
        sortedCands = S.unstableSortBy (compare `on` \(i,n) ->
            (-netNamePriority n, i)) cands
        name = T.unlines $ toList $ fmap (\(i,n) -> netName n) sortedCands

    renameNet idx net | Just ids <- M.lookup (NetId idx) revMap =
        net { netName = nameFromIds ids }
    renameNet idx net = net

    renameNets mod = mod { moduleNets = S.mapWithIndex renameNet $ moduleNets mod }

-- Run a `NetMergeM` action on a module, returning a map of net substitutions.
-- The first output maps each `NetId` in `mod` to the representative NetId of
-- its equivalence class, and the second maps each representative of a
-- nontrivial class to the set of NetIds in the class.
runNetMerge :: Module a -> (forall s. NetMergeM s ()) -> (Map NetId NetId, Map NetId (Seq NetId))
runNetMerge mod m = runST $ do
    -- Build an array, containing one union-find "point" for each net.  The
    -- point descriptors are lists of NetIds, .
    arr <- newArray_ (NetId 0, NetId $ S.length (moduleNets mod) - 1)
        :: ST s (STArray s NetId (NetMergePoint s))
    (lo, hi) <- getBounds arr
    forM_ [lo .. hi] $ \netId -> do
        pt <- UF.fresh $ S.singleton netId
        writeArray arr netId pt

    -- Run user action to merge some nets together.
    evalStateT m arr

    -- Build a map from each net to its union-find representative.
    substMap <- M.fromList <$> forM [lo .. hi] (\oldId -> do
        pt <- readArray arr oldId
        desc <- UF.descriptor pt
        let newId = desc `S.index` 0
        return (oldId, newId))

    revMap <- mconcat <$> forM [lo .. hi] (\oldId -> do
        pt <- readArray arr oldId
        desc <- UF.descriptor pt
        let newId = desc `S.index` 0
        if S.length desc > 1 && newId == oldId then
            return $ M.singleton newId desc
        else
            return M.empty
        )

    return (substMap, revMap)


filterWithIndex :: (Int -> a -> Bool) -> Seq a -> Seq a
filterWithIndex f s =
    S.foldMapWithIndex (\i x -> if f i x then S.singleton x else S.empty) s
