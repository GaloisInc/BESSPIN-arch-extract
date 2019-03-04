-- Helpers for injecting global parameters and threading them through to
-- specific target modules.
module BESSPIN.ArchExtract.GlobalParam where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Writer
import Data.Array.ST
import Data.Foldable
import Data.Generics
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro
import Lens.Micro.Platform

import Debug.Trace

import BESSPIN.ArchExtract.Architecture


-- For each entry (mod, name) in `gs`, inject a parameter called `name` into
-- the module with ID `mod`.  Also injects the parameter into all modules that
-- instantiate `mod`, recursively.  If parameters with identical names are
-- injected into parent and child modules, the parent will have only one copy
-- of the parameter, which will be passed down to the child.
injectGlobals :: [(Int, Text, Maybe ConstExpr)] -> Design a -> (Design a, Seq (Map Text Int))
injectGlobals gs d = (d & _designMods .~ mods', paramIdxMap)
  where
    baseParamMap :: Map Int (Map Text (Maybe ConstExpr))
    baseParamMap = M.fromListWith M.union $ map (\(m,t,d) -> (m, M.singleton t d)) gs

    -- Mutual recursion + laziness: paramMap calls calcParams to build its
    -- entries, and calcParams looks up the paramMap entries for child elements
    -- (based on `moduleChildren` output).  This terminates as long as the
    -- design has no cycles among modules.
    calcParams :: Int -> Map Text (Maybe ConstExpr)
    calcParams i = M.unions $
        fromMaybe M.empty (M.lookup i baseParamMap) :
        map (paramMap `S.index`) (Set.toList $ moduleChildren $ d `designMod` i)

    -- For each module, the names of the parameters to add to this module.
    paramMap :: Seq (Map Text (Maybe ConstExpr))
    paramMap = S.fromList $ map calcParams [0 .. S.length (designMods d) - 1]

    (mods', paramIdxMap) = S.unzip $
        S.zipWith (addModuleParams paramMap) paramMap $ designMods d


-- Add new parameters named `ps` to module `m`.  Also updates instantiations to
-- pass through parameters to child modules - `pm` indicates which parameters
-- each child requires.
addModuleParams :: Seq (Map Text (Maybe ConstExpr)) -> Map Text (Maybe ConstExpr) ->
    Module a -> (Module a, Map Text Int)
addModuleParams pm ps m =
    ( m & _moduleParams %~ (<> newParams) & _moduleLogics %~ fmap goLogic
    , M.fromList $ zip (M.keys ps) [baseIdx..]
    )
  where
    newParams = S.fromList $ map (\(name, dfl) -> Param name dfl PkNormal) $ M.toList ps

    goLogic l = case logicKind l of
        LkInst inst -> l { logicKind = LkInst $ goInst inst }
        _ -> l

    -- Starting index of the new parameters added to `m`.
    baseIdx = S.length $ moduleParams m

    goInst inst = inst & _instParams %~ (<> newExprs)
      where
        childParams = M.keys $ pm `S.index` instModId inst
        newExprs = S.fromList $ map (\name ->
            -- Every new parameter of the child module must be a parameter of
            -- the parent (`m`) as well, so `findIndex` should never fail.
            let idx = baseIdx + M.findIndex name ps in
            Just $ EParam dummySpan idx) childParams


moduleChildren :: Module a -> Set Int
moduleChildren m = flip foldMap (moduleLogics m) $ \l -> case logicKind l of
    LkInst inst -> Set.singleton $ instModId inst
    _ -> Set.empty
