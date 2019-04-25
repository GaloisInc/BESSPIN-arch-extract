{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.BSV
( listSources
, readAndExtract
, testAst
, listPackageNames
) where

import Data.ByteString (ByteString)
import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.Generics
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import qualified System.FilePath.Glob as Glob
import System.IO
import System.IO.Temp
import System.Process

import qualified BESSPIN.ArchExtract.Architecture as A
import BESSPIN.ArchExtract.Verilog.Raw (FileInfo(..))
import qualified BESSPIN.ArchExtract.Config as Config
import BESSPIN.ArchExtract.BSV.Decode
import BESSPIN.ArchExtract.BSV.Raw
import BESSPIN.ArchExtract.BSV.PrintRaw
import BESSPIN.ArchExtract.BSV.RaiseRaw
import BESSPIN.ArchExtract.BSV.Extract

import BESSPIN.ArchExtract.Print


-- Ensure that `bsvAstDir` is set, by filling it in with a temporary directory
-- if it's `Nothing`.
withAstDir :: Config.BSV -> (Config.BSV -> IO a) -> IO a
withAstDir cfg act
  | Nothing <- Config.bsvAstDir cfg =
    withSystemTempDirectory "bsv-ast" $ \dirPath ->
        act (cfg { Config.bsvAstDir = Just $ T.pack dirPath })
  | otherwise = act cfg

astDirPath cfg
  | Just path <- Config.bsvAstDir cfg = T.unpack path
  | otherwise = error "astDirPath: bsvAstDir is unset (called outside withAstDir)"

bsvRootPackage cfg = T.takeWhile (/= '.') $ Config.bsvRootModule cfg

findPackageSrc :: Text -> [FilePath] -> FilePath
findPackageSrc pkg srcs =
    fromMaybe (error $ "couldn't find source file for package " ++ show pkg) $
    find (\path -> pkg == pathPackage path) srcs

pathPackage :: FilePath -> Text
pathPackage p = T.pack $ dropExtension (takeFileName p)

buildPackageMap :: [FilePath] -> Map Text FilePath
buildPackageMap paths = M.fromList [(pathPackage p, p) | p <- paths]


-- Monad for CBOR package loading.  Most operations in this module (even ones
-- that just list source files) require loading package ASTs, and we'd like to
-- avoid repeating that work when possible.
type LoadM a = StateT (Map Text Package) IO a


-- Load a CBOR file from `path`, and add each package it contains to the cache.
loadCbor :: FilePath -> LoadM (Map Text Package)
loadCbor path = do
    cborBs <- lift $ readMaybeGzippedFile path
    pkgs <- case deserialize cborBs of
        Left err -> error $ T.unpack err
        Right x -> return x
    let pkgMap = M.fromList [(idName $ packageId p, p) | p <- pkgs]
    modify $ \m -> pkgMap `M.union` m
    return pkgMap

-- Load a package AST from the cache or from disk.
loadPackage :: Text -> Maybe FilePath -> LoadM Package
loadPackage pkgName optPath = do
    cached <- gets $ M.lookup pkgName
    case cached of
        Just x -> return x
        Nothing -> case optPath of
            Nothing -> error $ "package " ++ show pkgName ++
                " not in cache, and no path was provided"
            Just path -> do
                newPkgs <- loadCbor path
                case M.lookup pkgName newPkgs of
                    Just x -> return x
                    Nothing -> error $ "failed to find package " ++ show pkgName ++
                        " in AST file " ++ show path

getLoadedPackage :: Text -> LoadM Package
getLoadedPackage pkgName = do
    cached <- gets $ M.lookup pkgName
    case cached of
        Just x -> return x
        Nothing -> error $ "package " ++ show pkgName ++ " is not loaded"

-- Get the dependencies of a package.  If the package is not loaded and `path`
-- is provided, it will be loaded from disk.
packageDeps :: Text -> Maybe FilePath -> LoadM [Text]
packageDeps pkgName optPath = do
    pkg <- loadPackage pkgName optPath
    return $ map idName $ packageImports pkg


-- Check if the processed AST files are up to date.  Returns `True` if they
-- are, or `False` if they need to be rebuilt.
checkAstTimestamps :: Config.BSV -> IO Bool
checkAstTimestamps cfg = do
    let inputsList = astDirPath cfg </> "inputs.txt"
    exists <- doesFileExist inputsList
    if not exists then return False else do
    lastUpdate <- getModificationTime inputsList
    inputPaths <- lines <$> readFile inputsList
    foldM (\ok path -> if not ok then return False else do
        exists <- doesFileExist path
        if not exists then return False else do
        inputTime <- getModificationTime path
        return (inputTime <= lastUpdate)) True inputPaths

-- Ensure the processed AST files are up to date.  Most other `LoadM` actions
-- require this to be run first (otherwise they may read stale data).
updateAstFiles :: Config.BSV -> LoadM ()
updateAstFiles cfg = do
    ok <- lift $ checkAstTimestamps cfg
    when (not ok) $ do
        lift $ createDirectoryIfMissing True (astDirPath cfg)

        allSources <- lift $ listAllSources cfg
        let allSourceDirs = dedup $ map takeDirectory allSources
        let searchPath = intercalate ":" allSourceDirs ++ ":+"
        let pkgMap = buildPackageMap allSources
        let rootSrc = pkgMap M.! bsvRootPackage cfg

        -- Run the exporter to generate CBOR AST files for the design.  CBOR
        -- files will be placed in `astDirPath cfg`.
        exporter <- lift $ fromMaybe "besspin-arch-extract-export-bsv" <$>
            lookupEnv "BESSPIN_ARCH_EXTRACT_EXPORT_BSV"
        lift $ callProcess exporter
            (map T.unpack (Config.bsvBscFlags cfg) ++
                ["-u", "-p", searchPath, "-bdir", astDirPath cfg, rootSrc])

        -- Update `inputs.txt` with the list of inputs that were used.  We do
        -- this by walking the package dependency graph
        let pkgCborMap = M.mapWithKey (\k _ ->
                astDirPath cfg </> T.unpack k <.> "cbor") pkgMap
        visitedPkgNames <- walkPackages pkgCborMap (bsvRootPackage cfg)
        let visitedSources = map (\pn -> pkgMap M.! pn) $ Set.toList visitedPkgNames
        lift $ writeFile (astDirPath cfg </> "inputs.txt") $ unlines $ visitedSources

  where
    -- Deduplicate while retaining the original order.
    -- `dedup [2,1,2,3] == [2,1,3]`
    dedup xs = go Set.empty xs
      where
        go seen [] = []
        go seen (x : xs)
          | x `Set.member` seen = go seen xs
          | otherwise = x : go (Set.insert x seen) xs


-- Walk the package dependency graph, finding all packages (transitively)
-- imported by `pkgName`.
walkPackages :: Map Text FilePath -> Text -> LoadM (Set Text)
walkPackages srcMap pkgName = go Set.empty pkgName
  where
    go :: Set Text -> Text -> LoadM (Set Text)
    go seen pkgName
      | Set.member pkgName seen = return seen
      -- Skip packages not in `srcMap`.  These are probably library packages.
      | not $ M.member pkgName srcMap = return seen
      | otherwise = do
        deps <- packageDeps pkgName (M.lookup pkgName srcMap)
        foldM go (Set.insert pkgName seen) deps


-- List all sources that might be used in the design by expanding globs in the
-- `bsvSrcFiles` config option.
listAllSources :: Config.BSV -> IO [FilePath]
listAllSources cfg = do
    let pats = map (Glob.compile . T.unpack) $ Config.bsvSrcFiles cfg
    files <- Glob.globDir pats "."
    return $ concat $ map sort files

-- List the BSV sources actually used in the current design, reading them from
-- the cached `inputs.txt`.  This only produces good results if
-- `updateAstFiles` has already been run.
listSourcesCached :: Config.BSV -> LoadM [FilePath]
listSourcesCached cfg =
    lift $ lines <$> readFile (astDirPath cfg </> "inputs.txt")

-- External API for listing BSV sources.  Internal callers that have already
-- run `updateAstFiles` should use `listSourcesCached` instead.
listSources :: Config.BSV -> IO [FilePath]
listSources cfg = runLoadM cfg $ \cfg -> listSourcesCached cfg


listLibraryCbors :: IO [FilePath]
listLibraryCbors = do
    lister <- fromMaybe "besspin-arch-extract-list-bsv-libraries" <$>
        lookupEnv "BESSPIN_ARCH_EXTRACT_LIST_BSV_LIBRARIES"

    lines <$> readProcess lister [] ""


-- Load all packages in the design, including library packages.
loadDesign :: Config.BSV -> LoadM [Package]
loadDesign cfg = do
    libMap <- lift $ buildPackageMap <$> listLibraryCbors
    designSrcMap <- buildPackageMap <$> listSourcesCached cfg
    let designMap = M.mapWithKey (\k _ ->
            astDirPath cfg </> T.unpack k <.> "cbor") designSrcMap

    let pkgMap = designMap <> libMap
    pkgNames <- walkPackages pkgMap (bsvRootPackage cfg)
    mapM getLoadedPackage $ Set.toList pkgNames


-- Run a `LoadM` action.  This runs `updateAstFiles` before `act`, so all
-- `LoadM` actions will work normally.
runLoadM :: Config.BSV -> (Config.BSV -> LoadM a) -> IO a
runLoadM cfg act = withAstDir cfg $ \cfg' -> flip evalStateT M.empty $ do
    updateAstFiles cfg'
    act cfg'


testAst :: Config.BSV -> IO ()
testAst cfg = do
    pkgs <- runLoadM cfg $ \cfg -> loadDesign cfg

    let pkgs' = numberNodes $ raiseRaw pkgs
    let er = extractDesign' cfg pkgs'
    let pkgs'' = annotateNodes (erNodeErrors er) pkgs'

    forM_ pkgs'' $ \pkg -> do
        putStrLn $ T.unpack $ "\ncontents of package " <> idName (packageId pkg) <> ":"
        forM_ (packageDefs pkg) $ \def -> do
            let name = idName $ defId def
            let go c = if fromEnum c < 128 then T.singleton c
                    else "<" <> T.pack (show $ fromEnum c) <> ">"
            putStrLn $ T.unpack $ T.concatMap go name

    forM_ pkgs'' $ \pkg -> do
        putStrLn $ "\n\n --- package " ++ T.unpack (idName $ packageId pkg) ++ " ---"
        putStrLn $ T.unpack $ printBSV pkg
        putStrLn $ " --- end package " ++ T.unpack (idName $ packageId pkg) ++ " ---\n"
        hFlush stdout

    T.putStrLn $ " --- begin error report ---"
    forM_ (M.toList $ erModuleErrors er) $ \(modName, errs) -> do
        forM_ errs $ \err ->
            T.putStrLn $ "error: " <> modName <> ": " <> err
        T.putStrLn ""
        hFlush stdout
    T.putStrLn $ " --- end error report ---"

    let d = erDesign er
    putStrLn $ "extracted " ++ show (S.length $ A.designMods d) ++ " modules"
    T.putStrLn $ printArchitecture d

readMaybeGzippedFile :: FilePath -> IO BSL.ByteString
readMaybeGzippedFile fp
  | ".gz" `isSuffixOf` fp = do
    (Nothing, Just stdout, Nothing, ph) <- createProcess $
        (proc "gunzip" ["-c", fp]) { std_out = CreatePipe }
    hSetBinaryMode stdout True
    BSL.hGetContents stdout
  | otherwise = BSL.readFile fp

readAndExtract :: Config.BSV -> IO (A.Design (), [FileInfo])
readAndExtract cfg = runLoadM cfg $ \cfg -> do
    pkgs <- loadDesign cfg
    return (extractDesign cfg $ raiseRaw pkgs, [])

listPackageNames :: Config.BSV -> IO [Text]
listPackageNames cfg = runLoadM cfg $ \cfg -> do
    pkgs <- loadDesign cfg
    return $ map (idName . packageId) pkgs


numberNodes x = evalState (everywhereM (mkM goStmt `extM` goExpr) x) 1
  where
    next = do
        x <- get
        modify (+ 1)
        return x

    goStmt (SBind p t e _) = next >>= \nid -> return $ SBind p t e nid
    goStmt (SBind' e _) = next >>= \nid -> return $ SBind' e nid
    goStmt (SNote x) = return $ SNote x

    goExpr (ELet d e _ msgs) = next >>= \nid -> return $ ELet d e nid msgs
    goExpr e = return e

annotateNodes m x = everywhere (mkT goStmts `extT` goExpr) x
  where
    goStmts [] = []
    goStmts (s:ss) = case s of
        SBind _ _ _ nid -> s : map SNote (get nid) ++ ss
        SBind' _ nid -> s : map SNote (get nid) ++ ss
        _ -> s : ss

    goExpr (ELet d e nid msgs) = ELet d e nid (msgs ++ get nid)
    goExpr e = e

    get nid = toList $ fromMaybe S.empty $ M.lookup nid m
