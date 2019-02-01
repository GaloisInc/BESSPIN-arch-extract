module BESSPIN.ArchExtract.Aggregate where

import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable

import Debug.Trace

import BESSPIN.ArchExtract.Architecture
import BESSPIN.ArchExtract.GraphOps
import BESSPIN.ArchExtract.Simplify


-- Within module `modId`, collect a set of `Logic`s and `Net`s, create a new
-- `Module` declaration containing only those items, and replace the original
-- items with an instantiation of that new module.
--
-- The set of items to include in the new module is defined by `boundaryNets`
-- and `excludeNets`.  Roughly speaking, the goal is to include a
-- self-contained collection of logic that lies "inside" the `boundaryNets`,
-- without including any of the `excludeNets`.
--
-- Concretely: we include all the `boundaryNets`, plus any net or logic that is
-- only reachable from the original module's input/output ports by passing
-- through a boundary net.  The `excludeNets` are added to the input and output
-- ports, so that anything reachable from an `excludeNet` without passing
-- through a `boundaryNet` is excluded from the set.
--
-- With these rules, it should always be the case that every port of the
-- generated module is based on one of the boundary nets.  That means users can
-- set the module interface by providing appropriate boundary nets, and by
-- default (with empty `excludeNets`), every bit of logic that can possibly go
-- inside that interface will be included in the new module.
aggregateModule :: Monoid a => ModId -> Set NetId -> Set NetId -> Design a -> Design a
aggregateModule modId boundaryNets excludeNets d =
    d { designMods = (S.update modId mod' $ designMods d) |> newMod }
  where
    mod = d `designMod` modId
    newModId = S.length $ designMods d

    (takenLogics, takenNets) = enclosed
        (Set.empty, boundaryNets)
        (Set.empty, excludeNets)
        mod

    connTaken (ExtPort _) = False
    connTaken (LogicPort i _) = i `Set.member` takenLogics

    inferNetTy i net = TSimVal -- TODO

    -- List of nets that will become input ports.  These are nets that pass
    -- data from a non-taken logic to a taken one.
    inPorts :: Seq Port
    inPorts = S.foldMapWithIndex (\i net -> do
            guard $ NetId i `Set.member` takenNets
            guard $ any (not . connTaken) (netSources net)
            guard $ any connTaken (netSinks net)
            let ty = inferNetTy i net
            return $ Port (head $ T.lines $ netName net) (NetId i) ty
        ) (moduleNets mod)
    outPorts = S.foldMapWithIndex (\i net -> do
            guard $ NetId i `Set.member` takenNets
            guard $ any connTaken (netSources net)
            guard $ any (not . connTaken) (netSinks net)
            let ty = inferNetTy i net
            return $ Port (head $ T.lines $ netName net) (NetId i) ty
        ) (moduleNets mod)

    inPins = fmap (\p -> Pin (portNet p) (portTy p)) inPorts
    outPins = fmap (\p -> Pin (portNet p) (portTy p)) outPorts

    (takenLogicItems, leftLogicItems) =
        partitionWithIndex (\i _ -> Set.member i takenLogics) $ moduleLogics mod

    mod' = reconnectNets $ mod
        { moduleLogics = leftLogicItems |> newInst
        }

    modName = T.pack "agg" -- TODO
    instName = T.pack "agg" -- TODO

    newMod = reconnectNets $
        Module modName inPorts outPorts leftLogicItems (moduleNets mod)
    newInst = Logic (LkInst $ Inst newModId instName) inPins outPins mempty

partitionWithIndex :: (Int -> a -> Bool) -> Seq a -> (Seq a, Seq a)
partitionWithIndex f xs =
    S.foldMapWithIndex (\i x ->
        if f i x then (S.singleton x, S.empty) else (S.empty, S.singleton x)) xs
