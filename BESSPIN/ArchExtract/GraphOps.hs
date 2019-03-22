{-# LANGUAGE TemplateHaskell #-}
module BESSPIN.ArchExtract.GraphOps where

import Control.Monad
import Control.Monad.State
import Data.Foldable
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
import Lens.Micro.Platform

import Debug.Trace

import BESSPIN.ArchExtract.Architecture


data Node = NInput | NOutput | NLogic Int | NNet NetId
    deriving (Show, Eq, Ord)

-- Find the set of all nodes for which at least one path in the "forward"
-- direction reaches one of the `targets`.  
--
-- `revNodes`: gets a list of edges pointing in the "reverse" direction from a
-- given node.
coverAnyPath :: (Node -> [Node]) -> [Node] -> Set Node
coverAnyPath revNodes targets = covered
  where
    -- Idea: walk backward from each target.  Every node visited has a forward
    -- path to a target.

    -- `acc` is the set of nodes already visited.
    go acc n
        | n `Set.member` acc = acc
        | otherwise = foldl go (Set.insert n acc) (revNodes n)

    covered = foldl go Set.empty targets


-- Find the set of all nodes for which every path in the "forward" direction to
-- one of the `exits` passes through at least one of the `targets`.
coverAllPaths :: (Node -> [Node]) -> Set Node -> Set Node -> [Node] -> Set Node
coverAllPaths revNodes allNodes targets exits = covered
  where
    -- Idea: walk backward from each exit.  If we hit a target, stop.
    -- Otherwise, every node visited has a path to an exit that *doesn't* go
    -- through a target.

    -- `acc` is the union of "previously-visited nodes" and `targets`.  That
    -- means we can decide whether to skip a node using only one membership
    -- check, not two.  For computing the final answer, we subtract out
    -- `targets` before using it.
    go acc n
        | n `Set.member` acc = acc
        | otherwise = foldl go (Set.insert n acc) (revNodes n)

    uncoveredPlusTargets = foldl go targets exits
    covered = (allNodes `Set.difference` uncoveredPlusTargets) `Set.union` targets


splitNodeSet :: Set Node -> (Set Int, Set NetId)
splitNodeSet nodes = (logics, nets)
  where
    (_ioNodes, logicNetNodes) = Set.spanAntitone (\n ->
        case n of NInput -> True; NOutput -> True; _ -> False) nodes
    (logicNodes, netNodes) = Set.spanAntitone (\n ->
        case n of NLogic _ -> True; _ -> False) logicNetNodes

    logics = Set.mapMonotonic (\n ->
        case n of NLogic x -> x; _ -> error "expected NLogic") logicNodes
    nets = Set.mapMonotonic (\n ->
        case n of NNet x -> x; _ -> error "expected NNet") netNodes

mergeNodeSet :: (Set Int, Set NetId) -> Set Node
mergeNodeSet (logics, nets) = logicNodes `Set.union` netNodes
  where
    logicNodes = Set.mapMonotonic NLogic logics
    netNodes = Set.mapMonotonic NNet nets


allModNodes mod =
    -- Note: this use of fromAscList depends on the order of constructors of
    -- `Node`.
    Set.fromAscList $
        map NLogic [0 .. S.length (moduleLogics mod) - 1] ++
        map (NNet . NetId) [0 .. S.length (moduleNets mod) - 1]

getInputs _ NInput = []
getInputs mod NOutput =
    map (NNet . portNet) $ toList $ moduleOutputs mod
getInputs mod (NLogic i) =
    map (NNet . pinNet) $ toList $ logicInputs $ mod `moduleLogic` i
getInputs mod (NNet i) =
    map go $ toList $ netSources $ mod `moduleNet` i
  where
    go (ExtPort _) = NInput
    go (LogicPort i _) = NLogic i

getOutputs mod NInput =
    map (NNet . portNet) $ toList $ moduleInputs mod
getOutputs _ NOutput = []
getOutputs mod (NLogic i) =
    map (NNet . pinNet) $ toList $ logicOutputs $ mod `moduleLogic` i
getOutputs mod (NNet i) =
    map go $ toList $ netSinks $ mod `moduleNet` i
  where
    go (ExtPort _) = NOutput
    go (LogicPort i _) = NLogic i

inputRoots mod =
    map NLogic (S.foldMapWithIndex goLogic $ moduleLogics mod) ++
    map (NNet . NetId) (S.foldMapWithIndex goNet $ moduleNets mod) ++
    [NInput]
  where
    goLogic i logic = if S.null $ logicInputs logic then [i] else []
    goNet i net = if S.null $ netSources net then [i] else []

outputRoots mod =
    map NLogic (S.foldMapWithIndex goLogic $ moduleLogics mod) ++
    map (NNet . NetId) (S.foldMapWithIndex goNet $ moduleNets mod) ++
    [NOutput]
  where
    goLogic i logic = if S.null $ logicOutputs logic then [i] else []
    goNet i net = if S.null $ netSinks net then [i] else []


-- Find the set of nodes that are "fully downstream" of `targets` in `mod`.  A
-- node is "fully downstream" if all of its inputs (transitively) originate in
-- `targets`.
downstreamFull :: (Set Int, Set NetId) -> Module a -> (Set Int, Set NetId)
downstreamFull splitTargets mod = splitNodeSet $
    coverAllPaths (getOutputs mod) (allModNodes mod) (mergeNodeSet splitTargets) (inputRoots mod)

upstreamFull :: (Set Int, Set NetId) -> Module a -> (Set Int, Set NetId)
upstreamFull splitTargets mod = splitNodeSet $
    coverAllPaths (getInputs mod) (allModNodes mod) (mergeNodeSet splitTargets) (outputRoots mod)

downstreamPartial :: (Set Int, Set NetId) -> Module a -> (Set Int, Set NetId)
downstreamPartial splitTargets mod = splitNodeSet $
    coverAnyPath (getOutputs mod) (Set.toList $ mergeNodeSet splitTargets)

upstreamPartial :: (Set Int, Set NetId) -> Module a -> (Set Int, Set NetId)
upstreamPartial splitTargets mod = splitNodeSet $
    coverAnyPath (getInputs mod) (Set.toList $ mergeNodeSet splitTargets)



-- Find the set of nodes that are fully enclosed by the `targets` - that is,
-- the nodes which have no connection (in any direction) to `mod`'s inputs or
-- outputs except through the `targets`.
enclosed :: (Set Int, Set NetId) -> (Set Int, Set NetId) ->
    Module a -> (Set Int, Set NetId)
enclosed targets exclude mod = splitNodeSet $
    coverAllPaths
        (\n -> getInputs mod n ++ getOutputs mod n)
        (allModNodes mod)
        (mergeNodeSet targets)
        ([NInput, NOutput] ++ Set.toList (mergeNodeSet exclude))


-- Strongly-connected components

data SccState = SccState
    { _sccVerts :: Seq SccVertex
    , _sccStk :: [Int]
    , _sccNextIdx :: Int
    , _sccSccs :: [Set Int]
    }

data SccVertex = SccVertex
    { _vertIndex :: Maybe Int
    , _vertLowLink :: Int
    , _vertOnStk :: Bool
    }

makeLenses ''SccState
makeLenses ''SccVertex

-- Label the strongly-connected components of a graph.
labelSccs :: (Int -> [Int]) -> Int -> [Set Int]
labelSccs succs numVerts = evalState top initState
  where
    initVertex = SccVertex Nothing (-1) False
    initState = SccState (S.fromFunction numVerts $ const initVertex) [] 0 []

    top :: State SccState [Set Int]
    top = do
        forM_ [0 .. numVerts - 1] $ \v -> do
            visited <- use $ sccVerts . singular (ix v) . vertIndex . to isJust
            when (not visited) $ go v

        use sccSccs

    go :: Int -> State SccState ()
    go v = do
        vIdx <- use sccNextIdx
        sccNextIdx %= (+ 1)
        sccStk %= (v:)
        zoom (sccVerts . ix v) $ do
            vertIndex .= Just vIdx
            vertLowLink .= vIdx
            vertOnStk .= True

        forM_ (succs v) $ \w -> do
            wIdx <- use $ sccVerts . singular (ix w) . vertIndex
            case wIdx of
                Nothing -> do
                    go w
                    vll <- use $ sccVerts . singular (ix v) . vertLowLink
                    wll <- use $ sccVerts . singular (ix w) . vertLowLink
                    sccVerts . ix v . vertLowLink .= min vll wll
                Just wIdx -> do
                    wOnStk <- use $ sccVerts . singular (ix w) . vertOnStk
                    when wOnStk $ do
                        vll <- use $ sccVerts . singular (ix v) . vertLowLink
                        sccVerts . ix v . vertLowLink .= min vll wIdx


        (vs, (_v : stk)) <- use $ sccStk . to (break (== v))
        sccStk .= stk
        let sccVs = v : vs
        forM_ sccVs $ \w -> sccVerts . ix w . vertOnStk .= False
        sccSccs %= (Set.fromList sccVs :)


-- Topological sort

topoSort :: (Int -> [Int]) -> Int -> Seq Int
topoSort succs root = S.reverse $ evalState (go root) Set.empty
  where
    go :: Int -> State (Set Int) (Seq Int)
    go cur = do
        seen <- get
        if Set.member cur seen then return S.empty else do
        put $ Set.insert cur seen
        rest <- mconcat <$> mapM go (succs cur)
        return $ rest |> cur
