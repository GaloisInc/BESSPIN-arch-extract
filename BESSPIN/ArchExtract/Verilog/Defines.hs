module BESSPIN.ArchExtract.Verilog.Defines where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.Array.ST
import Data.Foldable
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
import Data.Word
import Lens.Micro.Platform
import Text.Read (readMaybe)

import Debug.Trace
import Data.List

import BESSPIN.ArchExtract.Architecture
import BESSPIN.ArchExtract.Verilog.Raw (FileInfo(..))
import BESSPIN.ArchExtract.GlobalParam


-- TODO: copied from Rewrite
spanLo :: Span -> Word32
spanLo (Span lo _) = lo

spanHi :: Span -> Word32
spanHi (Span _ hi) = hi

-- Convert references to Verilog preprocessor macros into parameters.
--
-- We recognize macros by their spans.  Verific doesn't record source locations
-- of macro bodies, only their call sites.  If the text of an expression begins
-- with a backtick, then that expression in the AST actually came from a macro
-- invocation.  
--
-- Note we need source text for every file to do this analysis.
convertDefines :: Data a => [FileInfo] -> [Text] -> Design a -> Design a
convertDefines fs srcs d = d''
  where
    -- Location of every backtick.  If an expr span starts at any of these
    -- locations, it points to a preprocessor macro.
    backticks = Set.unions $
        zipWith (\(FileInfo _ sp) t -> indexBackticks (spanLo sp) t) fs srcs


    -- Tables for looking up source text at a location.

    -- Maps span start positions to the full span and the file number.
    fileMap :: Map Word32 (Span, Int)
    fileMap = M.fromList $
        zipWith (\f@(FileInfo _ sp) i -> (spanLo sp, (sp, i))) fs [0..]

    srcs' = S.fromList srcs

    spanText :: Span -> Text
    spanText sp@(Span lo hi) = fromMaybe (error $ "failed to get text for " ++ show sp) $ do
        (_, (Span lo' hi', fi)) <- M.lookupLT lo fileMap
        guard $ lo' <= lo && hi <= hi'
        let src = srcs' `S.index` fi
        Just $ T.take (fromIntegral $ hi - lo) $ T.drop (fromIntegral $ lo - lo') src

    -- For each module, collect spans of all IntLits that start on a backtick.
    backtickLitSpans :: Seq (Map Span ConstExpr)
    backtickLitSpans = fmap (everything (<>) (M.empty `mkQ` go)) (designMods d)
      where
        go (EIntLit sp i)
          | spanLo sp `Set.member` backticks =
            M.singleton sp (EIntLit dummySpan i)
        go _ = M.empty

    -- Extract span text and get param name for each backticked span (across
    -- all modules)
    newParamName :: Map Span Text
    newParamName = M.fromSet (\sp -> T.tail $ spanText sp) $
        Set.unions $ map M.keysSet $ toList backtickLitSpans

    -- Info to pass to `injectGlobals`, specifying which globals we want to
    -- inject into which modules.
    injectGlobalSpec = S.foldMapWithIndex (\idx spans ->
        [(idx, name, Just dfl) | (name, dfl) <- M.toList $ M.mapKeys (newParamName M.!) spans])
        backtickLitSpans

    -- Inject new global params.  Also computes `paramMap`, which maps name to
    -- param index in each module (for new params only).
    paramMap :: Seq (Map Text Int)
    (d', paramMap) = injectGlobals injectGlobalSpec d

    -- Rewrite IntLits with matching spans to use new params.
    d'' = d' & _designMods %~ S.mapWithIndex (\idx m -> everywhere (mkT $ go idx) m)
      where
        go idx (EIntLit sp _)
          | Just paramName <- M.lookup sp newParamName,
            Just paramIdx <- M.lookup paramName (paramMap `S.index` idx) =
            EParam sp paramIdx
        go _ e = e

-- Get the positions of all backticks in `t`.  `base` is added to every index,
-- so setting `base` to `spanLo fileSpan` gives positions in global space.
indexBackticks :: Word32 -> Text -> Set Word32
indexBackticks base t = Set.fromList $
    map snd $ filter ((== '`') . fst) $ zip (T.unpack t) [base..]
