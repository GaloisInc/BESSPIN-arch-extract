module BESSPIN.ArchExtract.Main where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word

import Language.Clafer

import BESSPIN.ArchExtract.Verilog
import BESSPIN.ArchExtract.Architecture
import BESSPIN.ArchExtract.Extract
import qualified BESSPIN.ArchExtract.Decode as D
import BESSPIN.ArchExtract.Gen.Clafer
import BESSPIN.ArchExtract.Gen.Graphviz


main = do
    bs <- BS.readFile "out.cbor"
    v <- case D.deserialize (D.listOf D.moduleDecl) bs of
            Left errs -> do
                putStrLn ("error decoding verilog AST:\n" ++ errs)
                error $ "decoding error"
            Right x -> return x
    let arch = extractArch v
    let g = graphModule arch
            (defaultCfg
                { cfgDrawNets = True
                , cfgDrawOnesidedNets = False
                , cfgHideNamedNets = Set.fromList
                    [ T.pack "clock"
                    , T.pack "clk"
                    , T.pack "reset"
                    ]
                })
            (designMods arch `S.index` 0)
    putStrLn $ printGraphviz g
    --print $ "parsed " ++ show (length v) ++ " modules"
    --putStrLn $ printGraphviz $ genGraphviz v
    --case compiled $ genClafer v of
    --    Left errs -> mapM_ print errs
    --    Right outs -> mapM_ (putStrLn . outputCode) outs
