module BESSPIN.ArchExtract.Main where

import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word

import Codec.CBOR.Read

import Language.Clafer

import BESSPIN.ArchExtract.Verilog
import BESSPIN.ArchExtract.Gen.Clafer
import BESSPIN.ArchExtract.Gen.Graphviz


main = do
    bs <- BS.readFile "out.cbor"
    let (extra, v) = case deserialiseFromBytes (decodeListOf decodeModule) bs of
                Left errs -> error $ show errs
                Right x -> x
    putStrLn $ printGraphviz $ genGraphviz v
    --case compiled $ genClafer v of
    --    Left errs -> mapM_ print errs
    --    Right outs -> mapM_ (putStrLn . outputCode) outs
