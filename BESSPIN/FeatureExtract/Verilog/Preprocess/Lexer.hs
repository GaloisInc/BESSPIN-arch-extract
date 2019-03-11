{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module BESSPIN.FeatureExtract.Verilog.Preprocess.Lexer where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as P
import Data.Char
import Data.Data
import Data.Ix
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable
import Lens.Micro.Platform


data Token =
      Word Text
    | Sym Char
    | Comment Text
    | StrLit Text
    | TickWord Text
    | Eol
    deriving (Show, Data)

word = Word <$> takeWhile1 (inClass "a-zA-Z0-9_$")

sym = Sym <$> anyChar

lineComment = do
    "//"
    t <- P.takeWhile (not . (== '\n'))
    return $ Comment $ "//" <> t

data CommentState = CsIdle | CsStar | CsDone

blockComment = do
    "/*"
    t <- scan CsIdle (\s c -> case (s, c) of
        (_, '*') -> Just CsStar
        (CsStar, '/') -> Just CsDone
        (CsDone, _) -> Nothing
        _ -> Just CsIdle)
    return $ Comment $ "/*" <> t

comment = choice [lineComment, blockComment]

strChar = choice
    [ satisfy (not . inClass "\\\"") >> return ()
    , "\\" >> choice
        [ satisfy (not . inClass "0-9x") >> return ()
        , satisfy (inClass "0-9") >> anyChar >> anyChar >> return ()
        , "x" >> anyChar >> anyChar >> return ()
        ]
    ]

strLit = (StrLit . fst) <$> match ("\"" >> strChar `manyTill` "\"")

tickWord = "`" >> TickWord <$> takeWhile1 (inClass "a-zA-Z0-9_$")

whitespace = P.skipWhile (\c -> isSpace c && c /= '\n')

token = whitespace >> choice
    [ word
    , comment
    , strLit
    , tickWord
    , endOfLine >> return Eol
    , sym
    ]

tokens = many token -- <* whitespace <* endOfInput

--tokenize :: Text -> [Token]
tokenize t = go $ parse tokens t
  where
    go (Fail i ctxs msg) = error $ T.unpack $
        "parse error at `" <> T.take 10 i <> "`...: " <>
            T.intercalate "; " (map T.pack $ msg : ctxs)
    go (Partial k) = go (k "")
    go (Done _ x) = x
    --Left e -> error e
    --Right x -> x
