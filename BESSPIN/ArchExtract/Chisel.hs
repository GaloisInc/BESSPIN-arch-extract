module BESSPIN.ArchExtract.Chisel where

import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (UTCTime)
import System.Directory
import System.Environment
import System.FilePath
import System.Process

import qualified BESSPIN.ArchExtract.Architecture as A
import qualified BESSPIN.ArchExtract.Config as Config
import BESSPIN.ArchExtract.Chisel.FIRRTL.AST
import BESSPIN.ArchExtract.Chisel.FIRRTL.Decode
import BESSPIN.ArchExtract.Chisel.FIRRTL.Print (printFIRRTL)
import BESSPIN.ArchExtract.Chisel.Extract
import BESSPIN.ArchExtract.Print
import BESSPIN.ArchExtract.Verilog.Raw (FileInfo)

testAst :: Config.Chisel -> IO ()
testAst cfg = do
    bs <- BSL.readFile $ T.unpack $ Config.chiselAstFile cfg
    circ <- case deserialize bs of
        Left err -> error $ T.unpack err
        Right x -> return x

    putStrLn $ "loaded " ++ show (length (circuitModules circ)) ++ " modules"

    T.putStrLn $ printFIRRTL circ

    let d = extractDesign cfg circ
    putStrLn $ "extracted " ++ show (S.length $ A.designMods d) ++ " modules"
    T.putStrLn $ printArchitecture d

maybeTimestamp :: FilePath -> IO (Maybe UTCTime)
maybeTimestamp path = do
    exists <- doesFileExist path
    if exists then Just <$> getModificationTime path else return Nothing

checkAstTimestamp :: FilePath -> FilePath -> IO Bool
checkAstTimestamp src ast = do
    srcTime <- maybeTimestamp src
    astTime <- maybeTimestamp ast
    case (srcTime, astTime) of
        (Nothing, _) -> error $ "source file " ++ src ++ " not found"
        (Just st, Nothing) -> return False
        -- AST file must be newer than source.
        (Just st, Just at) -> return $ at > st

updateAstFile :: Config.Chisel -> FilePath -> FilePath -> IO ()
updateAstFile _cfg src ast = do
    ok <- checkAstTimestamp src ast
    when (not ok) $ do
        exporter <- fromMaybe "besspin-arch-extract-export-firrtl" <$>
            lookupEnv "BESSPIN_ARCH_EXTRACT_EXPORT_FIRRTL"
        callProcess exporter [src, ast]

readAndExtract :: Config.Chisel -> IO (A.Design (), [FileInfo])
readAndExtract cfg = do
    let astPath = T.unpack $ Config.chiselAstFile cfg
    case Config.chiselSourceFile cfg of
        Just srcPath -> updateAstFile cfg (T.unpack srcPath) astPath
        Nothing -> return ()

    bs <- BSL.readFile astPath
    circ <- case deserialize bs of
        Left err -> error $ T.unpack err
        Right x -> return x

    return (extractDesign cfg circ, [])
