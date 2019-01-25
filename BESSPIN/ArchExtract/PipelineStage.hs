module BESSPIN.ArchExtract.PipelineStage where

import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid hiding ((<>))
import Data.Semigroup
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

data Only a = OBottom | Only a | OTop
    deriving (Show)

instance Semigroup (Only a) where
    OBottom <> x = x
    x <> OBottom = x
    _ <> _ = OTop

instance Monoid (Only a) where
    mempty = OBottom
    mappend = (<>)

onlyValue (Only x) = Just x
onlyValue _ = Nothing


labelPipelineStages :: [(Text -> Bool, Maybe Int)] -> Module a -> Module (Maybe Int)
labelPipelineStages rules mod =
    traceShow ("boundaries", stageBoundaries) $
    traceShow ("contents", stageContents) $
    traceShow ("logic labels", logicLabels) $
    traceShow ("net labels", netLabels) $
    mod { moduleLogics = logics, moduleNets = nets }
  where
    checkRules [] t = Nothing
    checkRules ((f, x) : rs) t =
        if any f (T.lines t) then x else checkRules rs t

    -- The set of nets initially assigned to each stage.  These are used as
    -- boundary sets for `enclosed`.
    stageBoundaries :: Map Int (Set NetId)
    stageBoundaries = S.foldlWithIndex (\m i net ->
        case checkRules rules $ netName net of
            Nothing -> m
            Just stage -> M.insertWith (<>) stage (Set.singleton $ NetId i) m
        ) M.empty (moduleNets mod)

    -- The full sets of logics and nets collected for each stage.  Note it may
    -- be possible for the same item to appear in multiple stages' sets.
    stageContents :: Map Int (Set Int, Set NetId)
    stageContents = flip M.mapWithKey stageBoundaries $ \stage boundary ->
        let exclude = Set.unions $ M.elems $ M.delete stage stageBoundaries in
        enclosed (Set.empty, boundary) (Set.empty, exclude) mod

    logicLabels :: Map Int Int
    logicLabels = M.mapMaybe onlyValue $ M.unionsWith (<>) $
        map (\(stage, (logics, _)) -> M.fromSet (\_ -> Only stage) logics) $
        M.toList stageContents

    netLabels :: Map NetId Int
    netLabels = M.mapMaybe onlyValue $ M.unionsWith (<>) $
        map (\(stage, (_, nets)) -> M.fromSet (\_ -> Only stage) nets) $
        M.toList stageContents

    logics = S.mapWithIndex (\i logic -> 
        logic { logicAnn = M.lookup i logicLabels }) (moduleLogics mod)
    nets = S.mapWithIndex (\i net -> 
        net { netAnn = M.lookup (NetId i) netLabels }) (moduleNets mod)
