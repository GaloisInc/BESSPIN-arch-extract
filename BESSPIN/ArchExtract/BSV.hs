module BESSPIN.ArchExtract.BSV where

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

import qualified BESSPIN.ArchExtract.Config as Config
import BESSPIN.ArchExtract.BSV.Decode
import BESSPIN.ArchExtract.BSV.Raw
import BESSPIN.ArchExtract.BSV.PrintRaw
import BESSPIN.ArchExtract.BSV.RaiseRaw

testAst :: Config.BSV -> IO ()
testAst cfg = do
    cborBs <- BSL.readFile $ T.unpack $ Config.bsvAstFile cfg
    rawAst <- case deserialize cborBs of
        Left err -> error $ T.unpack err
        Right x -> return x
    putStrLn $ T.unpack $ printBSV $ raiseRaw rawAst

