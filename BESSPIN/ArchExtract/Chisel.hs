module BESSPIN.ArchExtract.Chisel where

import qualified Data.ByteString.Lazy as BSL
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified BESSPIN.ArchExtract.Architecture as A
import qualified BESSPIN.ArchExtract.Config as Config
import BESSPIN.ArchExtract.Chisel.FIRRTL.AST
import BESSPIN.ArchExtract.Chisel.FIRRTL.Decode
import BESSPIN.ArchExtract.Chisel.FIRRTL.Print (printFIRRTL)
import BESSPIN.ArchExtract.Chisel.Extract
import BESSPIN.ArchExtract.Print

testAst :: Config.Chisel -> IO ()
testAst cfg = do
    bs <- BSL.readFile $ T.unpack $ Config.chiselAstFile cfg
    circ <- case deserialize bs of
        Left err -> error $ T.unpack err
        Right x -> return x

    putStrLn $ "loaded " ++ show (length (circuitModules circ)) ++ " modules"

    --T.putStrLn $ printFIRRTL circ

    let d = extractDesign cfg circ
    putStrLn $ "extracted " ++ show (S.length $ A.designMods d) ++ " modules"
    T.putStrLn $ printArchitecture d


