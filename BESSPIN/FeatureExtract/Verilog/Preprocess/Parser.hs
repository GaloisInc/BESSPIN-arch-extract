{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module BESSPIN.FeatureExtract.Verilog.Preprocess.Parser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Data
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
import Text.Parsec
import qualified Text.Parsec as P
import Text.Parsec.Pos

import Debug.Trace

import BESSPIN.FeatureExtract.Verilog.Preprocess.Lexer (Token(..))


-- `Branch name val evts`: If the definition status of preprocessor macro
-- `name` matches `val`, then process `evts`.
data Branch = Branch Text Bool [Event]
    deriving (Show, Data)

data Event =
      TickRef Text
    | Cond [Branch] (Maybe [Event])
    | Define Text [Token]
    | OtherDirective Text
    deriving (Show, Data)


type EventP = Parsec [Token] ()

matchToken :: (Token -> Maybe a) -> EventP a
matchToken f = P.token show (const $ newPos "<??>" 0 0) f

tickWord = matchToken (\t -> case t of TickWord w -> Just w; _ -> Nothing)
    <?> "tickword"
word = matchToken (\t -> case t of Word w -> Just w; _ -> Nothing)
    <?> "word"
eol = matchToken (\t -> case t of Eol -> Just (); _ -> Nothing)
    <?> "end of line"

exactTickWord w = matchToken (\t -> case t of TickWord w' | w' == w -> Just (); _ -> Nothing)
    <?> "`" ++ T.unpack w

restOfLine = anyToken `manyTill` eol

skipBoring = skipMany $ matchToken $ \t -> case t of
    TickWord _ -> Nothing
    _ -> Just ()

blockEnd = choice
    [ exactTickWord "elsif"
    , exactTickWord "else"
    , exactTickWord "endif"
    ]

event :: EventP Event
event = do
    choice
        [ condEvent
        , defineEvent
        , otherDirective
        , TickRef <$> tickWord
        ]

defineEvent = do
    exactTickWord "define"
    name <- word
    body <- restOfLine
    return $ Define name body

otherDirective :: EventP Event
otherDirective = matchToken $ \t -> case t of
    TickWord w | Set.member w builtinMacroNames -> Just $ OtherDirective w
    _ -> Nothing

block :: EventP [Event]
block = skipBoring >> choice
    [ lookAhead blockEnd >> return []
    , event >>= \e -> (e :) <$> block
    , eof >> return []
    ]

condEvent :: EventP Event
condEvent = do
    dir <- choice
        [ exactTickWord "ifdef" >> return True
        , exactTickWord "ifndef" >> return False
        ]
    name <- word
    ifBranch <- Branch name dir <$> block
    skipBoring

    otherBranches <- P.many $ exactTickWord "elsif" >> do
        name <- word
        Branch name True <$> block <* skipBoring

    elseBranch <- optionMaybe $ exactTickWord "else" >> block
    skipBoring

    exactTickWord "endif"
    return $ Cond (ifBranch : otherBranches) elseBranch


builtinMacroNames = Set.fromList
    [ "__FILE__"
    , "__LINE__"
    , "begin_keywords"
    , "celldefine"
    , "default_nettype"
    , "define"
    , "else"
    , "elsif"
    , "end_keywords"
    , "endcelldefine"
    , "endif"
    , "ifdef"
    , "ifndef"
    , "include"
    , "line"
    , "nounconnected_drive"
    , "pragma"
    , "resetall"
    , "timescale"
    , "unconnected_drive"
    , "undef"
    , "undefineall"
    ]


parseEvents :: [Token] -> [Event]
parseEvents ts = case runParser block () "<tokens>" ts of
    Left e -> error $ show e
    Right x -> x
