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


-- Within module `modId`, collect a set of `ModInst` and `Logic` items, create
-- a new `ModDecl` containing only those items, and replace the original items
-- with an instantiation of that new module.
--
-- The ports of the new module are exactly the nets given in the `inputs` and
-- `outputs` sets.  The set of `ModInst` and `Logic` items to include is
-- inferred from the inputs and outputs: all items that are both "downstream of
-- inputs" and "upstream of outputs" are included in the modulde.  The
-- downstream-of-inputs predicate is defined as follows:
--
--   1. (Base case) Each net in the `inputs` set is downstream-of-inputs.
--   2. A logic or instance item is downstream-of-inputs if each of its input
--      nets is downstream-of-inputs.
--   3. A net is downstream-of-inputs if each of its source logics or instances
--      is downstream-of-inputs.
--
-- The upstream-of-outputs works the same but in the reverse direction.
--
-- The effect of these rules is to include as many insts and logics as
-- possible, without changing the semantics of the design and without expanding
-- the new module's input or output ports beyond those listed in `inputs` and
-- `outputs`.
aggregateModule :: ModId -> Set NetId -> Set NetId -> Design -> Design
aggregateModule modId boundaryNets excludeNets d =
    d { designMods = (S.update modId mod' $ designMods d) |> newMod }
  where
    mod = designMods d `S.index` modId
    newModId = S.length $ designMods d

    (takenInsts, takenLogics, takenNets) = enclosed
        (Set.empty, Set.empty, boundaryNets)
        (Set.empty, Set.empty, excludeNets)
        mod

    connTaken (ExtPort _) = False
    connTaken (InstPort i _) = i `Set.member` takenInsts
    connTaken (LogicPort i _) = i `Set.member` takenLogics

    -- List of nets that will become input ports.  These are nets that pass
    -- data from a non-taken inst/logic to a taken one.
    inPorts = S.foldMapWithIndex (\i net ->
        if NetId i `Set.member` takenNets &&
                any (not . connTaken) (netSources net) &&
                any connTaken (netSinks net)
            then S.singleton $ NetId i else S.empty) (modDeclNets mod)
    outPorts = S.foldMapWithIndex (\i net ->
        if NetId i `Set.member` takenNets &&
                any connTaken (netSources net) &&
                any (not . connTaken) (netSinks net)
            then S.singleton $ NetId i else S.empty) (modDeclNets mod)

    mod' = reconnectNets $ mod
        { modDeclInsts = (deleteByIndex takenInsts $ modDeclInsts mod) |> newInst
        , modDeclLogics = deleteByIndex takenLogics $ modDeclLogics mod
        }

    modName = T.pack "agg"
    instName = T.pack "agg"

    newMod = reconnectNets $ ModDecl modName
        (fmap (\netId ->
            let net = modDeclNets mod `S.index` unwrapNetId netId in
            PortDecl (head $ T.lines $ netName net) netId) inPorts)
        (fmap (\netId ->
            let net = modDeclNets mod `S.index` unwrapNetId netId in
            PortDecl (head $ T.lines $ netName net) netId) outPorts)
        S.empty
        S.empty
        (modDeclNets mod)
    newInst = ModInst newModId instName inPorts outPorts

-- Delete every entry in `s` whose index appears in `idxs`.
deleteByIndex :: Set Int -> Seq a -> Seq a
deleteByIndex idxs s = S.foldMapWithIndex (\i x ->
    if not $ i `Set.member` idxs then S.singleton x else S.empty) s
