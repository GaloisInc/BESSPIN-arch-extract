{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Gen.ModuleTree where

import Control.Monad
import Control.Monad.State
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Sequence (Seq, (|>), (<|))
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL

import Debug.Trace

import Data.GraphViz.Attributes.Complete hiding (portName)
import qualified Data.GraphViz.Attributes.HTML as H
import Data.GraphViz.Commands
import Data.GraphViz.Types
import Data.GraphViz.Types.Generalised

import BESSPIN.ArchExtract.Architecture
import qualified BESSPIN.ArchExtract.Config as Config


-- Build a map (keyed on module IDs) indicating the number of times each module
-- in the design is instantiated inside `m`.
countInsts :: Module a -> Map Int Int
countInsts m = M.fromListWith (+) $ foldMap go $ toList $ moduleLogics m
  where
    go (Logic { logicKind = LkInst inst }) = [(instModId inst, 1)]
    go _ = []

countInstsRec :: Design a -> Int -> Map Int Int
countInstsRec d root = evalState (go root) M.empty
  where
    -- Compute total instance counts for `modId`, or fetch them from the cache.
    -- Counts for a module include one instance of the module itself.
    go :: Int -> State (Map Int (Map Int Int)) (Map Int Int)
    go modId = do
        cached <- gets $ M.lookup modId
        case cached of
            Just x -> return x
            Nothing -> do
                let m = d `designMod` modId
                -- Store a blackhole value to detect cycles.
                modify $ M.insert modId $
                    error $ "cyclic module instantiation: " ++ show (moduleName m)
                let localCounts = countInsts m
                recCountList <- forM (M.toList localCounts) $ \(instModId, instCount) ->
                    fmap (* instCount) <$> go instModId
                let recCounts = M.unionsWith (+) (M.singleton modId 1 : recCountList)
                modify $ M.insert modId recCounts
                return recCounts

graphModuleTree :: Config.ModuleTree -> Design a -> DotGraph Text
graphModuleTree cfg d =
    DotGraph False True (Just $ Str "module-tree") (
        nodes <> edges
        |> GA (GraphAttrs [RankDir FromLeft, RankSep [2.0]])
        )
  where
    rootName = Config.moduleTreeRootModule cfg
    optRootId = S.findIndexL (\m -> moduleName m == rootName) (designMods d)
    rootId = fromMaybe (error $ "root module not found: " ++ show rootName) optRootId

    nodeCounts = countInstsRec d rootId
    nodes = fmap (\(idx, count) ->
        let m = d `designMod` idx in
        DN $ DotNode ("mod" <> T.pack (show idx))
            [Label $ StrLabel $ TL.fromStrict $ T.unlines [moduleName m, T.pack (show count)]]
        ) (S.fromList $ M.toList nodeCounts)

    edges = foldMap (\idx ->
        let m = d `designMod` idx in
        let counts = countInsts m in
        fmap (\(j, count) ->
            DE $ DotEdge ("mod" <> T.pack (show idx)) ("mod" <> T.pack (show j))
                [Label $ StrLabel $ TL.pack $ show count]
            ) (S.fromList $ M.toList counts)
        ) (M.keys nodeCounts)
