{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Rewrite where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
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
import qualified Data.Text.IO as T
import Data.Word
import Lens.Micro
import Lens.Micro.Platform

import Debug.Trace

import BESSPIN.ArchExtract.Architecture
import qualified BESSPIN.ArchExtract.Config as Config
import BESSPIN.ArchExtract.Constraints
import BESSPIN.ArchExtract.Verilog.Raw (Span(..), FileInfo(..))


overrideSpan :: Design a -> OverrideOrigin -> Maybe Span
overrideSpan d (OoLocal mIdx pIdx) =
    spanOf <$> paramDefault p
  where
    m = d `designMod` mIdx
    p = m `moduleParam` pIdx
overrideSpan d (OoInst mIdx lIdx pIdx) =
    spanOf <$> e
  where
    m = d `designMod` mIdx
    l = m `moduleLogic` lIdx
    LkInst inst = logicKind l
    e = instParams inst `S.index` pIdx

-- TODO: copied from ParamSMT
flattenConstraintsForDesign :: Config.Rewrite -> Design a -> FlatConstraints
flattenConstraintsForDesign cfg d = simplifyConstraints $ flattenConstraints d rootId
  where
    rootName = Config.rewriteRootModule cfg
    optRootId = S.findIndexL (\m -> moduleName m == rootName) (designMods d)
    rootId = fromMaybe (error $ "root module not found: " ++ show rootName) optRootId

runRewrite :: Config.Rewrite -> Design a -> [FileInfo] -> IO ()
runRewrite cfg d fs = rewriteFiles fs rws
  where
    fc = flattenConstraintsForDesign cfg d
    originMap = M.fromList $ toList $ fcOverrides fc
    rws = fromMaybe [] $ mconcat $ map (\(overName, newText) -> do
        origin <- M.lookup overName originMap
        span <- overrideSpan d origin
        Just [(span, newText)]) (Config.rewriteOverrides cfg) 


asSubspan :: Span -> Span -> Maybe Span
asSubspan (Span lo1 hi1) (Span lo2 hi2)
  | lo1 <= lo2 && hi2 <= hi1 = Just $ Span (lo2 - lo1) (hi2 - lo1)
  | otherwise = Nothing

spanLo :: Span -> Word32
spanLo (Span lo _) = lo

spanHi :: Span -> Word32
spanHi (Span _ hi) = hi

data Chunk = Orig Span | New Text
    deriving (Show)

-- Index rewrites by file, and convert global spans to file-level spans.
groupRewrites :: [FileInfo] -> [(Span, Text)] -> [(FileInfo, [(Span, Text)])]
groupRewrites fs rws = rwList
  where
    -- Maps the start position of each file to the `FileInfo`.
    fileMap = M.fromList $ map (\f@(FileInfo _ span) -> (spanLo span, f)) fs

    go :: Map Word32 (Map Span Text) -> (Span, Text) -> Map Word32 (Map Span Text)
    go m (sp, t)
      | Just (fileLo, FileInfo name fileSpan) <- M.lookupLT (spanLo sp) fileMap,
        Just sub <- asSubspan fileSpan sp =
        M.alter (\m' -> Just $ M.insert sub t $ fromMaybe M.empty m') fileLo m
    go m _ = m

    rwMap = foldl go M.empty rws
    rwList = [(fileMap M.! k, M.toList v) | (k, v) <- M.toList rwMap]

linearize :: FileInfo -> [(Span, Text)] -> [Chunk]
linearize (FileInfo _ fsp) rws = go 0 rws
  where
    go cur [] = maybeOrig cur end $ []
    go cur ((Span lo hi, t) : rws) = maybeOrig cur lo $ New t : go hi rws

    end = spanHi fsp

    maybeOrig lo hi cs
      | lo < hi = Orig (Span lo hi) : cs
      | otherwise = cs

newText :: Text -> [Chunk] -> Text
newText t cs = mconcat $ go 0 t cs
  where
    go cur t [] = []
    go cur t (Orig sp : cs) = t' : go (spanHi sp) rest cs
      where
        fromLo = T.drop (fromIntegral $ spanLo sp - cur) t
        t' = T.take (fromIntegral $ spanHi sp - spanLo sp) fromLo
        rest = T.drop (fromIntegral $ spanHi sp - spanLo sp) fromLo
    go cur t (New t' : cs) = t' : go cur t cs

-- Rewrite `f` using `rws`.  The spans in `rws` should be file-relative.
rewriteFile :: FileInfo -> [(Span, Text)] -> IO ()
rewriteFile f@(FileInfo path _) rws = do
    text <- T.readFile $ T.unpack path
    let text' = newText text $ linearize f rws
    --T.writeFile (T.unpack path) text'
    putStrLn $ "would rewrite " ++ T.unpack path ++ ": ----------"
    T.putStrLn text'
    putStrLn $ "end rewritten " ++ T.unpack path ++ " ----------"
    mapM_ print rws

-- Rewrite `fs` using `rws`.  The spans in `rws` should be relative to the
-- global address space defined by `fs`.
rewriteFiles :: [FileInfo] -> [(Span, Text)] -> IO ()
rewriteFiles fs rws = mapM_ (uncurry rewriteFile) $ groupRewrites fs rws
