{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Main where

import Control.Monad
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word

import Language.Clafer
import Data.GraphViz.Attributes.Colors
import Data.GraphViz.Attributes.Colors.X11

import BESSPIN.ArchExtract.Verilog.FromRaw
import BESSPIN.ArchExtract.Verilog.Extract (extractArch)
import qualified BESSPIN.ArchExtract.Verilog.Decode as D
import BESSPIN.ArchExtract.Architecture
--import BESSPIN.ArchExtract.Gen.Clafer
import BESSPIN.ArchExtract.Gen.Graphviz

import BESSPIN.ArchExtract.Aggregate
import BESSPIN.ArchExtract.GraphOps
import BESSPIN.ArchExtract.PipelineStage


instsNamed ns mod = S.foldMapWithIndex go (moduleLogics mod)
  where
    ns' = Set.fromList $ map T.pack ns
    go i (Logic (LkInst inst) _ _ _) =
        if Set.member (instName inst) ns' then Set.singleton i else Set.empty
    go _ _ = Set.empty

netsNamed ns mod = S.foldMapWithIndex go (moduleNets mod)
  where
    ns' = Set.fromList $ map T.pack ns
    go i net = if not $ Set.null $ Set.intersection ns' (Set.fromList $ T.lines $ netName net)
        then Set.singleton $ NetId i else Set.empty

main = do
    bs <- BS.readFile "out.cbor"
    (raw, modIds) <- case D.deserialize bs of
            Left errs -> do
                putStrLn ("error decoding verilog AST:\n" ++ errs)
                error $ "decoding error"
            Right x -> return x
    mapM_ print $ M.toList raw
    let v = fromRaw raw modIds
    let a = extractArch v

    let mod = a `designMod` 3

    let nets1 = flip netsNamed mod $
            [ "RA1D", "RA2D", "RA1E", "RA2E", "WA3E", "WA3M", "WA3W"
            , "Match_1D_E", "Match_2D_E"
            , "Match_1E_M", "Match_2E_M"
            , "Match_1E_W", "Match_2E_W"
            , "StallM", "FlushM"
            ]

    let exc1 = flip netsNamed mod $
            [ "wa3mreg.en" ]


    let nets = flip netsNamed mod $
            [ "RegFileRzD", "ModeOneHotD"
            , "RA1_4b_D", "RA2_4b_D", "WA3_4b_D"
            , "RA1D", "RA2D", "WA3D"
            ]

    let exc = flip netsNamed mod $
            []

    let a' = aggregateModule 3 nets1 exc1 a
    let a = aggregateModule 3 nets exc a'

    let mod = designMods a `S.index` 3

{-
    let mod' = labelPipelineStages
            [ (("D_E" `T.isSuffixOf`), Nothing)
            , (("E_M" `T.isSuffixOf`), Nothing)
            , (("E_W" `T.isSuffixOf`), Nothing)
            , (("F" `T.isSuffixOf`), Just 1)
            , (("D" `T.isSuffixOf`), Just 2)
            , (("E" `T.isSuffixOf`), Just 3)
            , (("M" `T.isSuffixOf`), Just 4)
            , (("W" `T.isSuffixOf`), Just 5)
            ] mod
    let mod = mod'


    let (ie, le, ne) = (Set.empty, Set.empty, Set.empty)
    --let (ie, le, ne) = enclosed (Set.empty, Set.empty, nets) (Set.empty, Set.empty, exc) mod
    let color xe k =
            if Set.member k xe then Just $ RGB 200 0 200
            else Nothing

    let mod' = mod
    let mod = mapAnn (\stage -> Ann $ case stage of
            Just 1 -> Just $ X11Color Red
            Just 2 -> Just $ X11Color Orange
            Just 3 -> Just $ X11Color Green
            Just 4 -> Just $ X11Color Turquoise
            Just 5 -> Just $ X11Color Purple
            Nothing -> Nothing) mod'

    let g = graphModule a
            (defaultCfg
                { cfgDrawNets = True
                , cfgDrawOnesidedNets = False
                , cfgDrawLogics = True
                , cfgDedupEdges = True
                , cfgPrefix = moduleName mod
                })
            mod
    putStrLn $ T.unpack $ moduleName mod
    writeFile ("out/" ++ T.unpack (moduleName mod) ++ ".dot") $ printGraphviz g
    -}

    let a' = Design $ fmap (
            mapAnn (\stage -> Ann $ case stage of
                Just 1 -> Just $ X11Color Red
                Just 2 -> Just $ X11Color Orange
                Just 3 -> Just $ X11Color Green
                Just 4 -> Just $ X11Color Turquoise
                Just 5 -> Just $ X11Color Purple
                Nothing -> Nothing)
            .
            labelPipelineStages
                [ (("D_E" `T.isSuffixOf`), Nothing)
                , (("E_M" `T.isSuffixOf`), Nothing)
                , (("E_W" `T.isSuffixOf`), Nothing)
                , (("F" `T.isSuffixOf`), Just 1)
                , (("D" `T.isSuffixOf`), Just 2)
                , (("E" `T.isSuffixOf`), Just 3)
                , (("M" `T.isSuffixOf`), Just 4)
                , (("W" `T.isSuffixOf`), Just 5)
                ]) $ designMods a
    let a = a'

    forM_ (designMods a) $ \mod -> do
        let g = graphModule a
                (defaultCfg
                    { cfgDrawNets = True
                    , cfgDrawOnesidedNets = False
                    , cfgDrawLogics = True
                    , cfgDedupEdges = False
                    , cfgShortenNetNames = False
                    , cfgPrefix = moduleName mod
                    })
                mod
        putStrLn $ T.unpack $ moduleName mod
        writeFile ("out/" ++ T.unpack (moduleName mod) ++ ".dot") $ printGraphviz g
