module BESSPIN.ArchExtract.Main where

import Control.Monad
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word

import Language.Clafer

import BESSPIN.ArchExtract.Verilog.Raw
import BESSPIN.ArchExtract.Verilog.FromRaw
import BESSPIN.ArchExtract.Architecture
import BESSPIN.ArchExtract.Extract
import qualified BESSPIN.ArchExtract.Decode as D
--import BESSPIN.ArchExtract.Gen.Clafer
import BESSPIN.ArchExtract.Gen.Graphviz


main = do
    bs <- BS.readFile "out.cbor"
    (raw, modIds) <- case D.deserialize bs of
            Left errs -> do
                putStrLn ("error decoding verilog AST:\n" ++ errs)
                error $ "decoding error"
            Right x -> return x
    mapM_ print $ M.toList raw
    let v = fromRaw raw modIds
    let a = extractArch v
    print a
    forM_ (designMods a) $ \mod -> do
        let g = graphModule a
                (defaultCfg
                    { cfgDrawNets = True
                    , cfgDrawOnesidedNets = False
                    , cfgDrawLogics = False
                    , cfgDedupEdges = False
                    , cfgPrefix = modDeclName mod
                    {- , cfgHideNamedNets = Set.fromList
                        [ T.pack "clock"
                        , T.pack "clk"
                        , T.pack "reset"
                        ]
                        -}
                    })
                mod
        putStrLn $ T.unpack $ modDeclName mod
        writeFile ("out/" ++ T.unpack (modDeclName mod) ++ ".dot") $ printGraphviz g
