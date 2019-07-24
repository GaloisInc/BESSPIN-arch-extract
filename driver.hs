{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable
import Data.Generics
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
--import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR
import qualified Codec.CBOR.Write as CBOR
import System.Directory
import System.Environment
import System.Exit
import System.IO

import BESSPIN.ArchExtract.Architecture
import qualified BESSPIN.ArchExtract.Config as Config
import qualified BESSPIN.ArchExtract.Main as ArchMain
import qualified BESSPIN.ArchExtract.Verilog as V
import BESSPIN.ArchExtract.Verilog.Raw (FileInfo(..))
import qualified BESSPIN.ArchExtract.BSV as BSV
import qualified BESSPIN.ArchExtract.Chisel as Chisel
import qualified BESSPIN.FeatureExtract.Verilog.Preprocess.Lexer as VPP
import qualified BESSPIN.FeatureExtract.Verilog.Preprocess.Parser as VPP
import qualified BESSPIN.ArchExtract.Gen.Graphviz as G
import qualified BESSPIN.ArchExtract.NameMap as NM


readConfig path = Config.parse <$> T.readFile path

listSourcesForGroups cfg grps = do
    let srcCfgs = if null grps then allSrcs else map getSrc grps
    liftM concat $ forM srcCfgs $ \src -> case src of
        Config.VerilogSrc vCfg -> V.listSources vCfg
        Config.BSVSrc bCfg -> BSV.listSources bCfg
  where
    allSrcs = M.elems $ Config.configSrcs cfg
    getSrc g = case M.lookup (T.pack g) (Config.configSrcs cfg) of
        Just x -> x
        Nothing -> error $ "source group " ++ show g ++ " not found"

getPpFlagsInFiles :: [FilePath] -> IO (Set Text)
getPpFlagsInFiles files = do
    ess <- mapM VPP.readEvents files
    return $ collectFlags ess Set.\\ collectDefines ess
  where
    collectFlags es = everything (<>) (Set.empty `mkQ` go) es
      where
        go (VPP.Branch name _ _) = Set.singleton name

    collectDefines es = everything (<>) (Set.empty `mkQ` go) es
      where
        go (VPP.Define name _) = Set.singleton name
        go _ = Set.empty

listPpFlagsInFiles :: [FilePath] -> IO [Text]
listPpFlagsInFiles files = Set.toList <$> getPpFlagsInFiles files

doesFileUseAnyPpFlag :: Set Text -> FilePath -> IO Bool
doesFileUseAnyPpFlag flags file = do
    evts <- VPP.readEvents file
    return $ everything (||) (False `mkQ` goB `extQ` goE) evts
  where
    goB (VPP.Branch t _ _) = Set.member t flags

    goE (VPP.TickRef t) = Set.member t flags
    goE _ = False

loadArchitecture :: Config.Config -> IO (Design (), [FileInfo])
loadArchitecture cfg = do
    (a, fi) <- case M.elems $ Config.configSrcs cfg of
        [] -> error "expected at least one source section"
        [Config.VerilogSrc vCfg] -> V.readAndExtract vCfg
        [Config.BSVSrc bCfg] -> BSV.readAndExtract bCfg
        [Config.ChiselSrc cCfg] -> Chisel.readAndExtract cCfg
        (_ : _ : _) -> error "support for multiple source sections is NYI"
    nm <- NM.loadNameMap $ Config.configNameMap cfg
    return (NM.applyNameMap nm a, fi)

graphArchitecture :: Config.Graphviz -> Design () -> IO ()
graphArchitecture gCfg arch = do
    arch <- return $ Design $ fmap (mapAnn (\_ -> G.defaultAnn)) $ designMods arch
    let modsByName = M.fromList $ map (\m -> (moduleName m, m)) $
            toList $ designMods arch
    let mods = case Config.graphvizRenderModules gCfg of
            Nothing -> designMods arch
            Just ns -> S.fromList $ map (\n -> case M.lookup n modsByName of
                Nothing -> error $ "no such module " ++ show n
                Just x -> x) ns

    let dir = T.unpack $ Config.graphvizOutDir gCfg
    createDirectoryIfMissing True dir

    let outputMods = S.filter (\m -> moduleKind m /= MkExtern) mods

    forM_ outputMods $ \mod -> do
        g <- G.graphModule arch gCfg mod
        writeFile (dir ++ "/" ++ T.unpack (moduleName mod) ++ ".dot") $
            G.printGraphviz g

    putStrLn $ "wrote " ++ show (length outputMods) ++ " graphviz files to " ++ dir ++ "/"

main = do
    args <- getArgs
    (configPath, cmd, args) <- case args of
        (cfg : cmd : args) -> return (cfg, cmd, args)
        [] -> do
            hPutStrLn stderr "usage: driver <config.toml> <cmd...>"
            exitWith $ ExitFailure 1

    cfg <- readConfig configPath

    case (cmd, args) of
        ("list-srcs", grps) ->
            listSourcesForGroups cfg grps >>= mapM_ putStrLn
        ("list-pp-flags", grps) ->
            listSourcesForGroups cfg grps >>= listPpFlagsInFiles >>= mapM_ T.putStrLn
        ("list-pp-flag-users", args) ->
            let (grps, flags) = case break (== "--") args of
                    (grps, "--" : flags) -> (grps, flags)
                    (flags, []) -> ([], flags) in
            let flagSet = Set.fromList $ map T.pack flags in
            listSourcesForGroups cfg grps >>=
                filterM (doesFileUseAnyPpFlag flagSet) >>=
                mapM_ putStrLn

        ("visualize", []) -> do
            gCfg <- case Config.configGraphvizOutput cfg of
                Just x -> return x
                Nothing -> error "`visualize` command requires a [graphviz] config section"
            (arch, _) <- loadArchitecture cfg
            graphArchitecture gCfg arch
        ("visualize", _) -> error "usage: driver <config.toml> visualize"

        ("old-arch-extract", []) -> ArchMain.mainWithConfig cfg
        ("old-arch-extract", _) -> error "usage: driver <config.toml> old-arch-extract"

        ("bsv-test", _) -> case M.elems $ Config.configSrcs cfg of
            [Config.BSVSrc bCfg] -> BSV.testAst bCfg
            _ -> error "expected a bsv src section"

        ("chisel-test", _) -> case M.elems $ Config.configSrcs cfg of
            [Config.ChiselSrc cCfg] -> Chisel.testAst cCfg
            _ -> error "expected a chisel src section"

        ("bsv-list-package-names", []) -> case M.elems $ Config.configSrcs cfg of
            [Config.BSVSrc bCfg] -> BSV.listPackageNames bCfg >>= mapM_ T.putStrLn
            _ -> error "expected a bsv src section"
        ("bsv-list-package-names", _) ->
            error "usage: driver <config.toml> bsv-list-package-names"

        -- Merge multiple BSV CBOR AST files into one.
        ("bsv-merge-cbor", files) -> do
            ps <- liftM concat $ forM files $ \file -> do
                putStrLn $ "reading " ++ file
                bs <- BSL.readFile file
                let t = case CBOR.deserialiseFromBytes CBOR.decodeTerm bs of
                        Left e -> error $ file ++ ": " ++ show e
                        Right (_, t) -> t
                let (tag, items) = case t of
                        CBOR.TList (tag : items) -> (tag, items)
                        CBOR.TListI (tag : items) -> (tag, items)
                        _ -> error $ file ++ ": expected CBOR list"
                case tag of
                    CBOR.TString "Packages" -> return items
                    _ -> return [t]

            let merged = CBOR.TList [CBOR.TString "Packages", CBOR.TList ps]
            let bs = CBOR.toLazyByteString $ CBOR.encodeTerm merged
            BSL.writeFile "out.cbor" bs
            putStrLn $ "wrote merged ASTs to out.cbor"

        ("check-config", flagStrs) -> do
            let flags = map T.pack flagStrs
            usedFlagSets <- forM (M.elems $ Config.configSrcs cfg) $ \src -> case src of
                Config.VerilogSrc vCfg ->
                    error $ "check-config verilog support is not yet implemented"
                Config.BSVSrc bCfg -> BSV.checkConfig bCfg flags
            let usedFlags = Set.unions usedFlagSets
            let allFlags = Set.fromList flags
            let ignoredFlags = allFlags Set.\\ usedFlags
            when (not $ Set.null ignoredFlags) $ do
                putStrLn $ show (Set.size ignoredFlags) ++ " features were ignored:"
                mapM_ T.putStrLn $ Set.toList ignoredFlags
                exitFailure


        (cmd, _) -> error $ "unknown command " ++ show cmd
        
