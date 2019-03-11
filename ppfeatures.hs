{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as P
import Data.Char
import Data.Data
import Data.Generics
import Data.Ix
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable
import Lens.Micro.Platform

import BESSPIN.FeatureExtract.Verilog.Preprocess.Lexer
import BESSPIN.FeatureExtract.Verilog.Preprocess.Parser


collectFlags es = everything (<>) (Set.empty `mkQ` go) es
  where
    go (Branch name _ _) = Set.singleton name
    go _ = Set.empty

collectDefines es = everything (<>) (Set.empty `mkQ` go) es
  where
    go (Define name _) = Set.singleton name
    go _ = Set.empty

main = do
    text <- T.getContents
    let toks = tokenize text
    print toks
    let evts = parseEvents toks
    print evts
    print $ collectFlags evts
    print $ collectDefines evts
    print $ collectFlags evts Set.\\ collectDefines evts
