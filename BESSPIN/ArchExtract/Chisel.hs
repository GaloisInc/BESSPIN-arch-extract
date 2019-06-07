module BESSPIN.ArchExtract.Chisel where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import qualified BESSPIN.ArchExtract.Config as Config
import BESSPIN.ArchExtract.Chisel.FIRRTL.AST
import BESSPIN.ArchExtract.Chisel.FIRRTL.Decode

testAst :: Config.Chisel -> IO ()
testAst cfg = do
    bs <- BSL.readFile $ T.unpack $ Config.chiselAstFile cfg
    circ <- case deserialize bs of
        Left err -> error $ T.unpack err
        Right x -> return x

    putStrLn $ "loaded " ++ show (length (circuitModules circ)) ++ " modules"
