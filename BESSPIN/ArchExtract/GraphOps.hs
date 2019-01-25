module BESSPIN.ArchExtract.GraphOps where

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

import Debug.Trace

import BESSPIN.ArchExtract.Architecture


data Node = NInput | NOutput | NInst Int | NLogic Int | NNet NetId
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


splitNodeSet :: Set Node -> (Set Int, Set Int, Set NetId)
splitNodeSet nodes = (insts, logics, nets)
  where
    (_ioNodes, instLogicNetNodes) = Set.spanAntitone (\n ->
        case n of NInput -> True; NOutput -> True; _ -> False) nodes
    (instNodes, logicNetNodes) = Set.spanAntitone (\n ->
        case n of NInst _ -> True; _ -> False) instLogicNetNodes
    (logicNodes, netNodes) = Set.spanAntitone (\n ->
        case n of NLogic _ -> True; _ -> False) logicNetNodes

    insts = Set.mapMonotonic (\n ->
        case n of NInst x -> x; _ -> error "expected NInst") instNodes
    logics = Set.mapMonotonic (\n ->
        case n of NLogic x -> x; _ -> error "expected NLogic") logicNodes
    nets = Set.mapMonotonic (\n ->
        case n of NNet x -> x; _ -> error "expected NNet") netNodes

mergeNodeSet :: (Set Int, Set Int, Set NetId) -> Set Node
mergeNodeSet (insts, logics, nets) = instNodes `Set.union` logicNodes `Set.union` netNodes
  where
    instNodes = Set.mapMonotonic NInst insts
    logicNodes = Set.mapMonotonic NLogic logics
    netNodes = Set.mapMonotonic NNet nets


allModNodes mod =
    -- Note: this use of fromAscList depends on the order of constructors of
    -- `Node`.
    Set.fromAscList $
        map NInst [0 .. S.length (modDeclInsts mod) - 1] ++
        map NLogic [0 .. S.length (modDeclLogics mod) - 1] ++
        map (NNet . NetId) [0 .. S.length (modDeclNets mod) - 1]

getInputs _ NInput = []
getInputs mod NOutput =
    map (NNet . portDeclNet) $ toList $ modDeclOutputs mod
getInputs mod (NInst i) =
    map NNet $ toList $ modInstInputs $ modDeclInsts mod `S.index` i
getInputs mod (NLogic i) =
    map NNet $ toList $ logicInputs $ modDeclLogics mod `S.index` i
getInputs mod (NNet (NetId i)) =
    map go $ toList $ netSources $ modDeclNets mod `S.index` i
  where
    go (ExtPort _) = NInput
    go (InstPort i _) = NInst i
    go (LogicPort i _) = NLogic i

getOutputs mod NInput =
    map (NNet . portDeclNet) $ toList $ modDeclInputs mod
getOutputs _ NOutput = []
getOutputs mod (NInst i) =
    map NNet $ toList $ modInstOutputs $ modDeclInsts mod `S.index` i
getOutputs mod (NLogic i) =
    map NNet $ toList $ logicOutputs $ modDeclLogics mod `S.index` i
getOutputs mod (NNet (NetId i)) =
    map go $ toList $ netSinks $ modDeclNets mod `S.index` i
  where
    go (ExtPort _) = NOutput
    go (InstPort i _) = NInst i
    go (LogicPort i _) = NLogic i

inputRoots mod =
    map NInst (S.foldMapWithIndex goInst $ modDeclInsts mod) ++
    map NLogic (S.foldMapWithIndex goLogic $ modDeclLogics mod) ++
    map (NNet . NetId) (S.foldMapWithIndex goNet $ modDeclNets mod) ++
    [NInput]
  where
    goInst i inst = if S.null $ modInstInputs inst then [i] else []
    goLogic i logic = if S.null $ logicInputs logic then [i] else []
    goNet i net = if S.null $ netSources net then [i] else []

outputRoots mod =
    map NInst (S.foldMapWithIndex goInst $ modDeclInsts mod) ++
    map NLogic (S.foldMapWithIndex goLogic $ modDeclLogics mod) ++
    map (NNet . NetId) (S.foldMapWithIndex goNet $ modDeclNets mod) ++
    [NOutput]
  where
    goInst i inst = if S.null $ modInstOutputs inst then [i] else []
    goLogic i logic = if S.null $ logicOutputs logic then [i] else []
    goNet i net = if S.null $ netSinks net then [i] else []


-- Find the set of nodes that are "fully downstream" of `targets` in `mod`.  A
-- node is "fully downstream" if all of its inputs (transitively) originate in
-- `targets`.
downstreamFull :: (Set Int, Set Int, Set NetId) -> ModDecl -> (Set Int, Set Int, Set NetId)
downstreamFull splitTargets mod = splitNodeSet $
    coverAllPaths (getOutputs mod) (allModNodes mod) (mergeNodeSet splitTargets) (inputRoots mod)

upstreamFull :: (Set Int, Set Int, Set NetId) -> ModDecl -> (Set Int, Set Int, Set NetId)
upstreamFull splitTargets mod = splitNodeSet $
    coverAllPaths (getInputs mod) (allModNodes mod) (mergeNodeSet splitTargets) (outputRoots mod)

downstreamPartial :: (Set Int, Set Int, Set NetId) -> ModDecl -> (Set Int, Set Int, Set NetId)
downstreamPartial splitTargets mod = splitNodeSet $
    coverAnyPath (getOutputs mod) (Set.toList $ mergeNodeSet splitTargets)

upstreamPartial :: (Set Int, Set Int, Set NetId) -> ModDecl -> (Set Int, Set Int, Set NetId)
upstreamPartial splitTargets mod = splitNodeSet $
    coverAnyPath (getInputs mod) (Set.toList $ mergeNodeSet splitTargets)



-- Find the set of nodes that are fully enclosed by the `targets` - that is,
-- the nodes which have no connection (in any direction) to `mod`'s inputs or
-- outputs except through the `targets`.
enclosed :: (Set Int, Set Int, Set NetId) -> (Set Int, Set Int, Set NetId) ->
    ModDecl -> (Set Int, Set Int, Set NetId)
enclosed targets exclude mod = splitNodeSet $
    coverAllPaths
        (\n -> getInputs mod n ++ getOutputs mod n)
        (allModNodes mod)
        (mergeNodeSet targets)
        ([NInput, NOutput] ++ Set.toList (mergeNodeSet exclude))



