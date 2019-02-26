{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Constraints.Parser
    ( module BESSPIN.ArchExtract.Constraints.Parser
    , UnArithOp(..)
    , BinArithOp(..)
    , BinCmpOp(..)
    ) where

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
import Data.Typeable
import Lens.Micro.Platform

import BESSPIN.ArchExtract.Architecture (UnArithOp(..), BinArithOp(..), BinCmpOp(..))


-- `Dotted ["x", "y"] "z"` = `x.y.z`.
data Dotted = Dotted [Text] Text
    deriving (Show)

-- Similar to Architecture.ConstExpr, but doesn't require resolving names,
-- making parsing easier.
data Expr =
      EIntLit Int
    | EDotted Dotted
    | EUnArith UnArithOp Expr
    | EBinArith BinArithOp Expr Expr
    | EBinCmp BinCmpOp Expr Expr
    | ERangeSize Expr Expr
    | ESize Dotted
    deriving (Show)

whitespace = skipWhile isHorizontalSpace
ident = P.takeWhile (\c -> isAlphaNum c || c == '_')

-- By convention, every named parser (including ones passed as arguments)
-- consumes trailing whitespace.

dotted :: Parser Dotted
dotted = do
    xs <- ident `sepBy1` "."
    whitespace
    return $ Dotted (init xs) (last xs)

expr :: Parser Expr
expr = e8
  where
    -- Subparsers are numbered by the row (1-based) where each operator appears
    -- in the SystemVerilog operator precedence table (Table 11-2 in IEEE
    -- Standard 1800-2012).  Many rows are omitted because we don't yet support
    -- all operators.

    e8 = assocl e7 EBinCmp $ choice
        [ "==" >> return BEq
        , "!=" >> return BNe
        ] <* whitespace

    e7 = assocl e5 EBinCmp $ choice
        [ "<=" >> return BLe
        , "<" >> return BLt
        , ">=" >> return BGe
        , ">" >> return BGt
        ] <* whitespace

    e5 = assocl e4 EBinArith $ choice
        [ "+" >> return BAdd
        , "-" >> return BSub
        ] <* whitespace

    e4 = assocl e1 EBinArith $ choice
        [ "*" >> return BMul
        ] <* whitespace

    e1 = choice
        [ "(" *> expr <* ")" <* whitespace
        , callExpr
        , EIntLit <$> signed decimal <* whitespace
        , EDotted <$> dotted
        ]

    assocl e f op = e >>= go
      where
        go cur = more cur <|> return cur
        more cur = (f <$> op <*> pure cur <*> e) >>= go

callExpr :: Parser Expr
callExpr = do
    "$"
    func <- ident <* whitespace
    "(" >> whitespace
    e <- case T.unpack func of
        "rangesize" -> ERangeSize <$> expr <* "," <* whitespace <*> expr
        "size" -> ESize <$> dotted
        "clog2" -> EUnArith UClog2 <$> expr
        "ispow2" -> EUnArith UIsPow2 <$> expr
    ")" >> whitespace
    return e


-- TODO: copied from NameMap
parse' :: Parser a -> Text -> Either Text a
parse' p t = go $ parse (p <* endOfInput) t
  where
    go (Fail i ctxs msg) = Left $
        "parse error at `" <> T.take 10 i <> "`...: " <>
            T.intercalate "; " (map T.pack $ msg : ctxs)
    go (Partial k) = go (k "")
    go (Done _ x) = Right x

parseConstraint' :: Text -> Either Text Expr
parseConstraint' t = parse' expr t

parseConstraint :: Text -> Expr
parseConstraint t = case parseConstraint' t of
    Left e -> error $ T.unpack e
    Right x -> x
