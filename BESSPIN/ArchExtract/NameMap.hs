{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.NameMap where

import Control.Monad.State
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

import BESSPIN.ArchExtract.Architecture


data Filter = Filter
    { fName :: Maybe Text
    , fType :: Maybe Text
    , fParentName :: Maybe Text
    , fNodeKind :: Maybe NodeKind
    }
    deriving (Show)

data NodeInfo = NodeInfo
    { nName :: Text
    , nType :: Maybe Text
    , nParentName :: Maybe Text
    , nKind :: NodeKind
    }
    deriving (Show)

data NodeKind =
      NkModule
    | NkParam
    | NkPort
    | NkInst
    | NkReg
    | NkDff
    | NkRam
    | NkNet
    deriving (Show, Eq)

matchMaybe :: Eq a => Maybe a -> Maybe a -> Bool
matchMaybe Nothing _ = True
matchMaybe (Just filt) (Just t) = t == filt
matchMaybe (Just _) Nothing = False

matchFilter :: NodeInfo -> Filter -> Bool
matchFilter n f =
    matchMaybe (fName f) (Just $ nName n) &&
    matchMaybe (fType f) (nType n) &&
    matchMaybe (fParentName f) (nParentName n) &&
    matchMaybe (fNodeKind f) (Just $ nKind n)

mapName :: NodeInfo -> [(Filter, Text)] -> Text
mapName n [] = nName n
mapName n ((f, newName) : fs)
  | matchFilter n f = newName
  | otherwise = mapName n fs


renameModule :: [(Filter, Text)] -> Module a -> Module a
renameModule fs m = m { moduleName = mapName ni fs }
  where
    ni = NodeInfo (moduleName m) Nothing Nothing NkModule

renameParam :: Module a -> [(Filter, Text)] -> Param -> Param
renameParam m fs p = p { paramName = mapName ni fs }
  where
    ni = NodeInfo (paramName p) Nothing (Just $ moduleName m) NkParam

renamePort :: Module a -> [(Filter, Text)] -> Port -> Port
renamePort m fs p = p { portName = mapName ni fs }
  where
    ni = NodeInfo (portName p) Nothing (Just $ moduleName m) NkPort

renameLogic :: Design a -> Module a -> [(Filter, Text)] -> Logic a -> Logic a
renameLogic d m fs l = l { logicKind = renameLogicKind d m fs $ logicKind l }

renameLogicKind :: Design a -> Module a -> [(Filter, Text)] -> LogicKind -> LogicKind
renameLogicKind d m fs (LkInst inst) = LkInst $ inst { instName = mapName ni fs }
  where ni = NodeInfo (instName inst)
            (Just $ moduleName $ d `designMod` instModId inst)
            (Just $ moduleName m) NkInst
renameLogicKind d m fs (LkRegister name) = LkRegister $ mapName ni fs
  where ni = NodeInfo name Nothing (Just $ moduleName m) NkReg
renameLogicKind d m fs lk@(LkDFlipFlop {}) = lk { lkDffName = mapName ni fs }
  where ni = NodeInfo (lkDffName lk) Nothing (Just $ moduleName m) NkDff
renameLogicKind d m fs lk@(LkRam {}) = lk { lkRamName = mapName ni fs }
  where ni = NodeInfo (lkRamName lk) Nothing (Just $ moduleName m) NkRam
renameLogicKind d m fs lk = lk

renameNet :: Module a -> [(Filter, Text)] -> Net a -> Net a
renameNet m fs n = n { netName = mapName ni fs }
  where
    ni = NodeInfo (netName n) Nothing (Just $ moduleName m) NkNet


applyNameMapToModule :: Design a -> [(Filter, Text)] -> Module a -> Module a
applyNameMapToModule d fs m = m
    & renameModule fs
    & _moduleParams %~ fmap (renameParam m fs)
    & _moduleInputs %~ fmap (renamePort m fs)
    & _moduleOutputs %~ fmap (renamePort m fs)
    & _moduleLogics %~ fmap (renameLogic d m fs)
    & _moduleNets %~ fmap (renameNet m fs)

applyNameMap :: [(Filter, Text)] -> Design a -> Design a
applyNameMap fs d = d
    & _designMods %~ fmap (applyNameMapToModule d fs)




ident = P.takeWhile (\c -> isAlphaNum c || c == '_')
whitespace = skipWhile isHorizontalSpace

pNodeKind = choice
    [ "any" >> return Nothing
    , "module" >> return (Just NkModule)
    , "param" >> return (Just NkParam)
    , "port" >> return (Just NkPort)
    , "inst" >> return (Just NkInst)
    , "reg" >> return (Just NkReg)
    , "dff" >> return (Just NkDff)
    , "ram" >> return (Just NkRam)
    , "net" >> return (Just NkNet)
    ]

pFilterName = choice
    [ "*" >> return Nothing
    , Just <$> ident
    ]

-- Examples:
--
--      module mux4         (module declaration named `mux4`)
--      inst top.leg        (instance `leg` in module `top`)
--      any *.clk           (anything named `clk`)
--      inst *.*:flopenrc   (any instantiation of module `flopenrc`)
pFilter :: Parser Filter
pFilter = do
    kind <- pNodeKind <* whitespace
    name1 <- pFilterName
    name2 <- option Nothing $ "." >> pFilterName
    ty <- option Nothing $ ":" >> pFilterName

    -- `a` -> name `a`, parent `Nothing`
    -- `a.b` -> name `b`, parent `Just a`
    let (name, parentName) = case name2 of
            Nothing -> (name1, Nothing)
            Just name2 -> (Just name2, name1)

    return $ Filter name ty parentName kind

pEntry :: Parser (Filter, Text)
pEntry = do
    filt <- pFilter <* whitespace
    "->" >> whitespace
    newName <- P.takeWhile (not . isEndOfLine)
    return (filt, newName)

pNameMap :: Parser [(Filter, Text)]
pNameMap = pEntry `sepBy` skipMany1 endOfLine <* skipMany endOfLine

parse' :: Parser a -> Text -> Either Text a
parse' p t = go $ parse (p <* endOfInput) t
  where
    go (Fail i ctxs msg) = Left $
        "parse error at `" <> T.take 10 i <> "`...: " <>
            T.intercalate "; " (map T.pack $ msg : ctxs)
    go (Partial k) = go (k "")
    go (Done _ x) = Right x

parseNameMap' :: Text -> Either Text [(Filter, Text)]
parseNameMap' t = parse' pNameMap t

parseNameMap :: Text -> [(Filter, Text)]
parseNameMap t = case parseNameMap' t of
    Left e -> error $ T.unpack e
    Right x -> x

parseFilter' :: Text -> Either Text Filter
parseFilter' t = parse' pFilter t

parseFilter :: Text -> Filter
parseFilter t = case parseFilter' t of
    Left e -> error $ T.unpack e
    Right x -> x
