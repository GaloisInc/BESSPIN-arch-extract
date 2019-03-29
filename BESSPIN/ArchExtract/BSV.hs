module BESSPIN.ArchExtract.BSV where

import Data.ByteString (ByteString)
import Control.Monad
import Data.List
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
    cborBs <- BSL.readFile $ T.unpack $ Config.bsvAstFile cfg
    rawAst <- case deserialize cborBs of
        Left err -> error $ T.unpack err
        Right x -> return x

    forM_ rawAst $ \pkg -> do
        putStrLn $ "\n\n --- package " ++ T.unpack (idName $ packageId pkg) ++ " ---"
        putStrLn $ T.unpack $ printBSV $ raiseRaw pkg
        putStrLn $ " --- end package " ++ T.unpack (idName $ packageId pkg) ++ " ---\n"

    T.putStrLn $ printArchitecture $ extractDesign (raiseRaw rawAst)

readAndExtract :: Config.BSV -> IO (A.Design (), [FileInfo])
readAndExtract cfg = do
    cborBs <- BSL.readFile $ T.unpack $ Config.bsvAstFile cfg
    rawAst <- case deserialize cborBs of
        Left err -> error $ T.unpack err
        Right x -> return x
    return (extractDesign (raiseRaw rawAst), [])
