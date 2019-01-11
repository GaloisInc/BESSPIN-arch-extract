-- Verilog AST and parsing from CBOR
module BESSPIN.ArchExtract.Verilog where

import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T

import Codec.CBOR.Decoding
import Codec.CBOR.Term


-- Verilog AST

type NodeId = Word

data Module = Module
    { moduleId :: NodeId
    , moduleName :: Text
    , moduleItems :: [ModItem]
    }
    deriving (Show)

data ModItem =
    ModInst
    { modItemId :: NodeId
    , modInstModId :: NodeId
    , modInstName :: Text
    }
    deriving (Show)


-- Internal types, used only during CBOR parsing

data InstId = InstId
    { instIdName :: Text
    }
    deriving (Show)


-- Parsing helpers

decodeListOf :: Decoder s a -> Decoder s [a]
decodeListOf a = do
    mLen <- decodeListLenOrIndef
    case mLen of
        Nothing -> do
            let go = do
                    stop <- decodeBreakOr
                    if stop then return [] else (:) <$> a <*> go
            go
        Just n -> replicateM n a

decodeFlatListOf :: Decoder s [a] -> Decoder s [a]
decodeFlatListOf a = do
    mLen <- decodeListLenOrIndef
    case mLen of
        Nothing -> do
            let go = do
                    stop <- decodeBreakOr
                    if stop then return [] else (++) <$> a <*> go
            go
        Just n -> concat <$> replicateM n a

skipNode = decodeTerm >> return ()


-- Verilog AST parsing from CBOR

decodeNodeId = decodeWord

decodeNode :: (NodeId -> String -> Decoder s a) -> Decoder s a
decodeNode f = do
    mLen <- decodeListLenOrIndef
    id <- decodeNodeId
    clsName <- T.unpack <$> decodeString
    node <- f id clsName
    case mLen of
        Nothing -> do
            ok <- decodeBreakOr
            when (not ok) $ error $ "expected end of " ++ clsName ++ " node"
        Just _ -> return ()
    return node

decodeModule = decodeNode $ \id clsName -> do
    name <- decodeString
    skipNode -- GetId()
    items <- decodeFlatListOf decodeModItems
    return $ Module id name items

-- Returns a list because we flatten some kinds of items during decoding
decodeModItems :: Decoder s [ModItem]
decodeModItems = decodeNode $ \id clsName -> do
    case clsName of
        "N7Verific23VeriModuleInstantiationE" -> do
            modId <- decodeNodeId
            insts <- decodeListOf decodeInstId
            return $ map (\i -> ModInst id modId (instIdName i)) insts
        _ -> return []

decodeInstId :: Decoder s InstId
decodeInstId = decodeNode $ \id clsName -> do
    name <- decodeString
    skipNode -- GetPortConnects
    return $ InstId name
