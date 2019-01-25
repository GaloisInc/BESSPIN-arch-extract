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


mapMods :: (Module a -> Module a) -> Design a -> Design a
mapMods f d = Design $ fmap f $ designMods d


-- Rebuild the `netSources` and `netSinks` connection lists for all nets in
-- `mod`, based on the `Inputs` and `Outputs` lists for the module itself and
-- all its `ModInst`s and `Logic`s.
reconnectNets :: Module a -> Module a
reconnectNets mod = runST $ do
    sources <- newArray (NetId 0, NetId $ S.length (moduleNets mod) - 1) S.empty
    sinks <- newArray (NetId 0, NetId $ S.length (moduleNets mod) - 1) S.empty

    connect (moduleInputs mod) portNet sources ExtPort
    connect (moduleOutputs mod) portNet sinks ExtPort
    void $ flip S.traverseWithIndex (moduleLogics mod) $ \i inst -> do
        connect (logicInputs inst) id sinks (LogicPort i)
        connect (logicOutputs inst) id sources (LogicPort i)

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

-- Remove each `LkNetAlias` `Logic` from `mod` by replacing one of its aliased
-- nets with the other.
mergeAliasedNets :: Module a -> Module a
mergeAliasedNets mod = filterLogics (\_ l -> logicKind l /= LkNetAlias) mod

-- Delete logic items rejected by predicate `f`.  When a logic item is deleted,
-- all of its input and output nets are merged into a single net, indicating
-- that data can flow freely from any input to any output.
filterLogics :: (Int -> Logic a -> Bool) -> Module a -> Module a
filterLogics f mod =
    runNetMerge act (mod { moduleLogics = logics' })
  where
    logics' = filterWithIndex f $ moduleLogics mod
    act = do
        void $ flip S.traverseWithIndex (moduleLogics mod) $ \idx l -> do
            when (not $ f idx l) $ do
                mergeNetSeq $ logicInputs l <> logicOutputs l

-- Replace each module instantiation rejected by `f` with an `LkOther` `Logic`
-- connected to the same nets.
filterInstsToLogic :: (Int -> Logic a -> Inst -> Bool) -> Module a -> Module a
filterInstsToLogic f mod =
    reconnectNets $ mod { moduleLogics = logics' }
  where
    logics' = S.mapWithIndex (\i logic ->
        if not $ check i logic then logic { logicKind = LkOther } else logic) (moduleLogics mod)
    check i logic = case logicKind logic of
        LkInst inst -> f i logic inst
        _ -> True


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
    goPort side idx (Port name netId) =
        Port <$> pure name <*>
            goNet (ExtPort idx) side netId (extPortName side name idx netId)

    goLogics :: Monoid a => Seq (Logic a) -> NetM a (Seq (Logic a))
    goLogics logics = S.traverseWithIndex goLogic logics
    goLogic :: Monoid a => Int -> Logic a -> NetM a (Logic a)
    goLogic idx (Logic kind ins outs ann) =
        Logic <$> pure kind
            <*> S.traverseWithIndex (go Sink) ins
            <*> S.traverseWithIndex (go Source) outs
            <*> pure ann
      where
        go side portIdx netId = goNet (LogicPort idx portIdx) side netId $
            logicPortName side portIdx netId

    goNet conn side netId name =
        if f conn side netId (nets `S.index` unwrapNetId netId) then
            return netId
        else
            mkNet name prioDcNet

    dcName netId base = T.pack "dc$" <> base <> T.singleton '$' <>
        T.pack (show $ unwrapNetId netId)
    dotted parts = T.intercalate (T.singleton '.') parts
    extPortName side name idx netId =
        dcName netId $ dotted [T.pack "ext", T.pack $ show side, T.pack $ show idx, name]
    logicPortName side portIdx netId =
        dcName netId $ dotted [T.pack "logic", T.pack $ show side, T.pack $ show portIdx]

prioDcNet = -1



type NetM a b = State (Seq (Net a)) b

mkNet :: Monoid a => Text -> Int -> NetM a NetId
mkNet name prio = state $ \nets ->
    (NetId $ S.length nets, nets |> Net name prio S.empty S.empty mempty)
    


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

runNetMerge :: (forall s. NetMergeM s ()) -> Module a -> Module a
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
        arr <- newArray_ (NetId 0, NetId $ S.length (moduleNets mod) - 1)
            :: ST s (STArray s NetId (NetMergePoint s))
        (lo, hi) <- getBounds arr
        forM_ [lo .. hi] $ \netId -> do
            let net = moduleNets mod `S.index` unwrapNetId netId
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
                            moduleNets mod `S.index` unwrapNetId id) $
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

    renameNets mod = mod { moduleNets = S.mapWithIndex renameNet $ moduleNets mod }


filterWithIndex :: (Int -> a -> Bool) -> Seq a -> Seq a
filterWithIndex f s =
    S.foldMapWithIndex (\i x -> if f i x then S.singleton x else S.empty) s
