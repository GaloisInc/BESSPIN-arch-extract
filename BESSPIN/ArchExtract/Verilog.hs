module BESSPIN.ArchExtract.Verilog where

import Data.ByteString (ByteString)
import Control.Monad
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import System.Directory
import System.Exit
import qualified System.FilePath.Glob as Glob
import System.IO.Temp
import System.Process

import qualified BESSPIN.ArchExtract.Architecture as A
import qualified BESSPIN.ArchExtract.Config as Config
import qualified BESSPIN.ArchExtract.Verilog.AST as V
import qualified BESSPIN.ArchExtract.Verilog.Decode as D
import BESSPIN.ArchExtract.Verilog.Extract
import BESSPIN.ArchExtract.Verilog.FromRaw
import qualified BESSPIN.ArchExtract.Verilog.Raw as VR
import BESSPIN.ArchExtract.Verilog.Raw (FileInfo(..), RawAst(..))

listSources :: Config.Verilog -> IO [FilePath]
listSources cfg = do
    let pats = map (Glob.compile . T.unpack) $ Config.verilogSrcFiles cfg
    files <- Glob.globDir pats "."
    return $ concat $ map sort files

-- Run `exporter` on `srcs`, writing a CBOR AST to `dest`.
exportAst :: [FilePath] -> FilePath -> IO ()
exportAst srcs dest = callProcess "./exporter" ("-o" : dest : srcs)

needsRebuild :: [FilePath] -> FilePath -> IO Bool
needsRebuild inputs output = do
    exists <- doesFileExist output
    if not exists then return True else do
    outputTime <- getModificationTime output
    foldM (\need input -> do
        if need then return True else do
        inputTime <- getModificationTime input
        return $ inputTime > outputTime) False inputs

getRawAst :: Config.Verilog -> IO VR.RawAst
getRawAst cfg = do
    srcFiles <- listSources cfg
    cborBs <- case Config.verilogAstFile cfg of
        Nothing -> withSystemTempFile "ast.cbor" $ \tempPath tempHandle -> do
            exportAst srcFiles tempPath
            BS.hGetContents tempHandle
        Just astPath -> do
            rebuild <- needsRebuild srcFiles (T.unpack astPath)
            when rebuild $ exportAst srcFiles (T.unpack astPath)
            BS.readFile (T.unpack astPath)
    case D.deserialize $ BSL.fromStrict cborBs of
        Left err -> do
            putStrLn "error decoding verilog AST:\n"
            putStrLn err
            exitWith $ ExitFailure 1
        Right x -> return x

readAst :: Config.Verilog -> IO (V.Design, [FileInfo])
readAst cfg = do
    RawAst nodeMap modIds fileInfos <- getRawAst cfg
    let blackboxNames = Set.fromList $ Config.verilogBlackboxModules cfg
    let v = fromRaw nodeMap blackboxNames modIds
    return (v, fileInfos)

extract :: Config.Verilog -> V.Design -> A.Design ()
extract cfg vDes = extractArch cfg vDes

readAndExtract :: Config.Verilog -> IO (A.Design (), [FileInfo])
readAndExtract cfg = do
    (vDes, fileInfos) <- readAst cfg
    return $ (extract cfg vDes, fileInfos)
