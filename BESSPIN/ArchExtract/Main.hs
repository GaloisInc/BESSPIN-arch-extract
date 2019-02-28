{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Main where

import Control.Monad
import qualified Data.ByteString.Lazy as BS
import Data.Foldable
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Word
import System.Directory
import System.Environment

import Language.Clafer
import Language.Clafer.Front.PrintClafer
import Data.GraphViz.Attributes.Colors
import Data.GraphViz.Attributes.Colors.X11

import qualified BESSPIN.ArchExtract.Config as Config
import BESSPIN.ArchExtract.Verilog.FromRaw
import BESSPIN.ArchExtract.Verilog.Extract (extractArch)
import qualified BESSPIN.ArchExtract.Verilog.Decode as D
import BESSPIN.ArchExtract.Verilog.Print
import BESSPIN.ArchExtract.Architecture
import BESSPIN.ArchExtract.Constraints
import BESSPIN.ArchExtract.NameMap
import BESSPIN.ArchExtract.Print
import BESSPIN.ArchExtract.Gen.Clafer
import BESSPIN.ArchExtract.Gen.Graphviz
import BESSPIN.ArchExtract.Gen.ModuleTree
import BESSPIN.ArchExtract.Gen.ParamSMT
import BESSPIN.ArchExtract.Gen.ParamClafer

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

loadNameMap nm = do
    fromFile <- case Config.nameMapFile nm of
        Nothing -> return []
        Just f -> do
            t <- T.readFile $ T.unpack f
            return $ parseNameMap t

    let fromEntries = map (\(k,v) -> (parseFilter k, v)) $ Config.nameMapEntries nm

    -- Inline entries take precedence over ones read from the file.
    return $ fromEntries ++ fromFile

main = do
    args <- getArgs
    config <- case args of
        [] -> do
            exists <- doesFileExist "arch-extract.toml"
            if exists then
                Config.parse <$> T.readFile "arch-extract.toml"
            else
                return Config.defaultConfigWithClafer
        [path] -> Config.parse <$> T.readFile path
        _ -> error "too many arguments - expected only a config file path"

    a <- case Config.configInput config of
        Config.VerilogInput vCfg -> do
            bs <- BS.readFile $ T.unpack $ Config.verilogSourceFile vCfg

            (raw, modIds) <- case D.deserialize bs of
                    Left errs -> do
                        putStrLn ("error decoding verilog AST:\n" ++ errs)
                        error $ "decoding error"
                    Right x -> return x

            let blackboxNames = Set.fromList $ Config.verilogBlackboxModules vCfg

            let v = fromRaw raw blackboxNames modIds
            writeFile "verilog.txt" $ T.unpack $ printVerilog v
            return $ extractArch vCfg v

    writeFile "arch.txt" $ T.unpack $ printArchitecture a

    nameMap <- loadNameMap $ Config.configNameMap config
    putStrLn "name map:"
    mapM_ print nameMap
    putStrLn "end name map"

    let a' = addConstraintsForConfig (Config.configConstraints config) a
    let a = a'
    let aMapped = applyNameMap nameMap a'

    case Config.configGraphvizOutput config of
        Nothing -> return ()
        Just g -> do
            let a = aMapped
            let a' = Design $ fmap (mapAnn (\_ -> defaultAnn)) $ designMods a
            let a = a'
            let modsByName = M.fromList $ map (\m -> (moduleName m, m)) $
                    toList $ designMods a
            let mods = case Config.graphvizRenderModules g of
                    Nothing -> designMods a
                    Just ns -> S.fromList $ map (\n -> case M.lookup n modsByName of
                        Nothing -> error $ "no such module " ++ show n
                        Just x -> x) ns

            let dir = T.unpack $ Config.graphvizOutDir g
            createDirectoryIfMissing False dir

            forM_ mods $ \mod -> do
                putStrLn " ----------------------"
                putStrLn $ T.unpack $ moduleName mod
                putStrLn " ----------------------"
                g <- graphModule a g mod
                writeFile (dir ++ "/" ++ T.unpack (moduleName mod) ++ ".dot") $
                    printGraphviz g

    case Config.configModuleTreeOutput config of
        Nothing -> return ()
        Just mt -> do
            let a = aMapped
            let g = graphModuleTree mt a
            let path = T.unpack $ Config.moduleTreeOutFile mt
            writeFile path $ printGraphviz g

    case Config.configClaferOutput config of
        Nothing -> return ()
        Just c -> do
            let modIdsByName = M.fromList $ map (\(m,i) -> (moduleName m, i)) $
                    zip (toList $ designMods a) [0..]
            let roots = Config.claferRootModules c
            let rootIds = map (\n -> case M.lookup n modIdsByName of
                    Nothing -> error $ "no such module " ++ show n
                    Just i -> i) roots
            let cfr = genClafer c a rootIds
            print ("clafer + ref counts", countClafers cfr)
            let path = T.unpack $ Config.claferOutFile c
            writeFile path $ render $ prt 0 $ cfr

    case Config.configSMTOutput config of
        Nothing -> return ()
        Just s -> do
            let path = T.unpack $ Config.smtOutFile s
            T.writeFile path $ genSmt' s a

            groupParameters s a

    case Config.configParamClaferOutput config of
        Nothing -> return ()
        Just pc -> do
            let path = T.unpack $ Config.paramClaferOutFile pc
            let cfr = genParamClafer' pc a
            writeFile path $ render $ prt 0 $ cfr


{-
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
-}

{-
    let a' = aggregateModule 3 nets1 exc1 a
    let a = aggregateModule 3 nets exc a'
-}

    --let mod = designMods a `S.index` 3

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

    {-
    let a' = Design $ fmap (
            mapAnn (\stage -> defaultAnn
                { annColor = case stage of
                    Just 1 -> Just $ X11Color Red
                    Just 2 -> Just $ X11Color Orange
                    Just 3 -> Just $ X11Color Green
                    Just 4 -> Just $ X11Color Turquoise
                    Just 5 -> Just $ X11Color Purple
                    Nothing -> Nothing
                , annPipelineStage = subtract 1 <$> stage
                })
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
    -}


{-
    --let mod = a `designMod` 3
    --do
    forM_ (designMods a) $ \mod -> do
        putStrLn " ----------------------"
        putStrLn $ T.unpack $ moduleName mod
        putStrLn " ----------------------"
        g <- graphModule a
                (defaultCfg
                    { cfgDrawNets = True
                    , cfgDrawOnesidedNets = False
                    , cfgDrawLogics = True
                    , cfgDedupEdges = True
                    , cfgShortenNetNames = False
                    , cfgPrefix = moduleName mod
                    , cfgPipelineStages = 0
                    })
                mod
        writeFile ("out/" ++ T.unpack (moduleName mod) ++ ".dot") $ printGraphviz g
-}
