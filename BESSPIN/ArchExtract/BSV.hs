{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.BSV where

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
import System.Exit
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

testAst :: Config.BSV -> IO ()
testAst cfg = do
    pkgs <- numberNodes <$> loadPackages cfg

    let er = extractDesign' cfg pkgs
    let pkgs' = annotateNodes (erNodeErrors er) pkgs

    forM_ pkgs' $ \pkg -> do
        putStrLn $ "\n\n --- package " ++ T.unpack (idName $ packageId pkg) ++ " ---"
        putStrLn $ T.unpack $ printBSV pkg
        putStrLn $ " --- end package " ++ T.unpack (idName $ packageId pkg) ++ " ---\n"

    T.putStrLn $ " --- begin error report ---"
    forM_ (M.toList $ erModuleErrors er) $ \(modName, errs) -> do
        forM_ errs $ \err ->
            T.putStrLn $ "error: " <> modName <> ": " <> err
        T.putStrLn ""
    T.putStrLn $ " --- end error report ---"

    T.putStrLn $ printArchitecture $ extractDesign cfg pkgs

readMaybeGzippedFile :: FilePath -> IO BSL.ByteString
readMaybeGzippedFile fp
  | ".gz" `isSuffixOf` fp = do
    (Nothing, Just stdout, Nothing, ph) <- createProcess $
        (proc "gunzip" ["-c", fp]) { std_out = CreatePipe }
    hSetBinaryMode stdout True
    BSL.hGetContents stdout
  | otherwise = BSL.readFile fp

loadPackages :: Config.BSV -> IO [Package]
loadPackages cfg = do
    cborBs <- readMaybeGzippedFile $ T.unpack $ Config.bsvAstFile cfg
    case deserialize cborBs of
        Left err -> error $ T.unpack err
        Right x -> return $ raiseRaw x

readAndExtract :: Config.BSV -> IO (A.Design (), [FileInfo])
readAndExtract cfg = do
    pkgs <- loadPackages cfg
    return (extractDesign cfg pkgs, [])

listPackageNames :: Config.BSV -> IO [Text]
listPackageNames cfg = do
    pkgs <- loadPackages cfg
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
