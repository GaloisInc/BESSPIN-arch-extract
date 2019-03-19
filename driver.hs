import Control.Monad
import Data.Generics
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment
import System.Exit
import qualified System.FilePath.Glob as Glob
import System.IO

import qualified BESSPIN.ArchExtract.Config as Config
import qualified BESSPIN.ArchExtract.Main as ArchMain
import qualified BESSPIN.FeatureExtract.Verilog.Preprocess.Lexer as VPP
import qualified BESSPIN.FeatureExtract.Verilog.Preprocess.Parser as VPP



readConfig path = Config.parse <$> T.readFile path

listVerilogSources vCfg = do
    let pats = map (Glob.compile . T.unpack) $ Config.verilogSrcFiles vCfg
    files <- Glob.globDir pats "."
    return $ concat $ map sort files

listSourcesForGroups cfg grps = do
    let srcCfgs = if null grps then allSrcs else map getSrc grps
    liftM concat $ forM srcCfgs $ \src -> case src of
        Config.VerilogSrc vCfg -> listVerilogSources vCfg
  where
    allSrcs = M.elems $ Config.configSrcs cfg
    getSrc g = case M.lookup (T.pack g) (Config.configSrcs cfg) of
        Just x -> x
        Nothing -> error $ "source group " ++ show g ++ " not found"

readPpEvents :: FilePath -> IO [VPP.Event]
readPpEvents file = do
    text <- T.readFile file
    let toks = VPP.tokenize text
    let evts = VPP.parseEvents toks
    return evts


getPpFlagsInFiles :: [FilePath] -> IO (Set Text)
getPpFlagsInFiles files = do
    ess <- mapM readPpEvents files
    return $ collectFlags ess Set.\\ collectDefines ess
  where
    collectFlags es = everything (<>) (Set.empty `mkQ` go) es
      where
        go (VPP.Branch name _ _) = Set.singleton name
        go _ = Set.empty

    collectDefines es = everything (<>) (Set.empty `mkQ` go) es
      where
        go (VPP.Define name _) = Set.singleton name
        go _ = Set.empty

listPpFlagsInFiles :: [FilePath] -> IO [Text]
listPpFlagsInFiles files = Set.toList <$> getPpFlagsInFiles files

doesFileUseAnyPpFlag :: Set Text -> FilePath -> IO Bool
doesFileUseAnyPpFlag flags file = do
    evts <- readPpEvents file
    return $ everything (||) (False `mkQ` goB `extQ` goE) evts
  where
    goB (VPP.Branch t _ _) = Set.member t flags

    goE (VPP.TickRef t) = Set.member t flags
    goE _ = False

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
        ("old-arch-extract", []) -> ArchMain.mainWithConfig cfg
        
