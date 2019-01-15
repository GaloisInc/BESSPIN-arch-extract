{-# LANGUAGE RankNTypes #-}
module BESSPIN.ArchExtract.Decode where

import Control.Monad
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word

import Debug.Trace

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR

import BESSPIN.ArchExtract.Verilog



type Stack = [[CBOR.Term]]

newtype DecodeM a = DecodeM { runDecodeM :: Stack -> Either String (a, Stack) }

instance Monad DecodeM where
    f >>= k = DecodeM $ \stk -> do
        (x, stk') <- runDecodeM f stk
        runDecodeM (k x) stk'
    return x = DecodeM $ \stk -> Right (x, stk)
    fail msg = DecodeM $ \stk -> Left msg

instance Applicative DecodeM where
    pure = return
    (<*>) = ap

instance Functor DecodeM where
    fmap = liftM


-- Primitive `DecodeM` operations

match :: (CBOR.Term -> Either String a) -> DecodeM a
match f = DecodeM $ \stk -> case stk of
    [] -> Left "empty stack"
    [] : _ -> Left "read past end of list"
    (x : terms) : stk -> f x >>= \x' -> Right (x', terms : stk)

skip :: DecodeM ()
skip = DecodeM $ \stk -> case stk of
    [] -> Left "empty stack"
    [] : _ -> Left "read past end of list"
    (x : terms) : stk -> Right ((), terms : stk)

skipRest :: DecodeM ()
skipRest = DecodeM $ \stk -> case stk of
    [] -> Left "empty stack"
    _ : stk -> Right ((), [] : stk)

pushList :: DecodeM ()
pushList = DecodeM $ \stk -> case stk of
    [] -> Left "empty stack"
    [] : _ -> Left "read past end of list"
    (t : terms) : stk -> case t of
        CBOR.TList ts -> Right ((), (ts : terms : stk))
        CBOR.TListI ts -> Right ((), (ts : terms : stk))
        _ -> Left $ "expected list, but got " ++ describe t

popList :: DecodeM ()
popList = DecodeM $ \stk -> case stk of
    [] -> Left "empty stack"
    [] : stk -> Right ((), stk)
    terms : stk -> Left $ show (length terms) ++ " unparsed items at end of list"

peek :: DecodeM (Maybe CBOR.Term)
peek = DecodeM $ \stk -> case stk of
    [] -> Left "empty stack"
    [] : _ -> Right (Nothing, stk)
    (x : _) : _ -> Right (Just x, stk)

mapErr :: (String -> String) -> DecodeM a -> DecodeM a
mapErr f m = DecodeM $ \stk -> case runDecodeM m stk of
    Left err -> Left $ f err
    Right x -> Right x


-- Slightly higher-level parsing ops

describe t = case t of
    CBOR.TInt _ -> "integer"
    CBOR.TInteger _ -> "integer"
    CBOR.TBytes _ -> "bytes"
    CBOR.TBytesI _ -> "bytes"
    CBOR.TString _ -> "string"
    CBOR.TStringI _ -> "string"
    CBOR.TList _ -> "list"
    CBOR.TListI _ -> "list"
    CBOR.TMap _ -> "map"
    CBOR.TMapI _ -> "map"
    CBOR.TTagged _ _ -> "tagged"
    CBOR.TBool _ -> "bool"
    CBOR.TNull -> "null"
    CBOR.TSimple _ -> "simple"
    CBOR.THalf _ -> "float"
    CBOR.TFloat _ -> "float"
    CBOR.TDouble _ -> "float"

text = match $ \t -> case t of
    CBOR.TString t -> return t
    CBOR.TStringI t -> return $ TL.toStrict t
    t -> Left $ "expected string, but got " ++ describe t

string = T.unpack <$> text

integer = match $ \t -> case t of
    CBOR.TInt i -> return $ toInteger i
    CBOR.TInteger i -> return i
    t -> Left $ "expected integer, but got " ++ describe t

word :: DecodeM Word
word = fromInteger <$> integer

list f = do
    pushList
    x <- f
    popList
    return x

traceScope :: Show b => b -> DecodeM a -> DecodeM a
traceScope loc m = mapErr (\err -> "at " ++ show loc ++ ", " ++ err) m

-- Check if we're at the end of the current list.
eol :: DecodeM Bool
eol = (== Nothing) <$> peek


-- Helpers


listOf :: DecodeM a -> DecodeM [a]
listOf f = list go
  where go = do
            atEol <- eol
            if atEol then return [] else (:) <$> f <*> go

flatListOf :: DecodeM [a] -> DecodeM [a]
flatListOf f = concat <$> listOf f

optional :: DecodeM a -> DecodeM (Maybe a)
optional f = do
    tok <- peek
    case tok of
        Nothing -> fail "read past end of list"
        Just CBOR.TNull -> skip >> return Nothing
        _ -> Just <$> f


-- Verilog AST decoding

nodeId :: DecodeM NodeId
nodeId = word

node :: (NodeId -> String -> DecodeM a) -> DecodeM a
node f = do
    (id, cls) <- traceScope "(node header)" $ do
        pushList
        id <- nodeId
        cls <- string
        return (id, cls)
    traceScope (cls, id) $ do
        x <- f id cls
        popList
        return x

nodeCls :: String -> (NodeId -> DecodeM a) -> DecodeM a
nodeCls expectCls f = node $ \id cls ->
    if cls == expectCls then
        f id
    else
        error $ "expected " ++ expectCls ++ ", but got " ++ cls

moduleDecl :: DecodeM ModuleDecl
moduleDecl = nodeCls "N7Verific10VeriModuleE" $ \id -> do
    name <- text
    traceShow ("in module", name) $ return ()
    skip    -- GetId()
    ports <- flatListOf portDecls
    params <- flatListOf paramDecls
    sourceItems <- flatListOf sourceModItems
    skip    -- GetPortConnects()
    skip    -- GetParameterConnects()
    skip    -- GetPackageImportDecls()
    let (items, params') = partitionSourceModItems sourceItems
    return $ ModuleDecl id name (params ++ params') ports items


sourceModItems :: DecodeM [SourceModItem]
sourceModItems = node $ \id cls -> case cls of
    "N7Verific12VeriDataDeclE" -> do
        skip    -- GetDeclType
        dir <- integer
        skip    -- GetDataType
        flatListOf $ node $ \id' cls' -> case cls' of
            "N7Verific12VeriVariableE" -> do
                name <- text
                skip    -- GetDataType()
                skip    -- GetDimensions()
                skip    -- GetInitialValue()
                dir' <- integer
                return [SMINormal $ VarDecl id' name dir]
            "N7Verific10VeriTypeIdE" -> skipRest >> return []
            "N7Verific11VeriParamIdE" -> do
                pd <- paramIdParts id'
                return [SMIParam pd]
            _ -> trace ("DataDecl.ids: unknown class " ++ cls') $ skipRest >> return []

    "N7Verific23VeriModuleInstantiationE" -> do
        modId <- nodeId
        skip    -- GetParamValues
        flatListOf $ node $ \id' cls' -> case cls' of
            "N7Verific10VeriInstIdE" -> do
                name <- text
                skip    -- GetPortConnects
                return [SMINormal $ Instance id' modId name]
            _ -> trace ("ModuleInstantiation.ids: unknown class " ++ cls') $ skipRest >> return []

    "N7Verific11VeriNetDeclE" -> do
        skip    -- DeclType
        dir <- integer
        skip    -- DataType
        skip    -- Strength
        flatListOf $ node $ \id' cls' -> case cls' of
            "N7Verific12VeriVariableE" -> do
                name <- text
                skip    -- GetDataType()
                skip    -- GetDimensions()
                skip    -- GetInitialValue()
                dir' <- integer
                return [SMINormal $ VarDecl id' name dir]
            _ -> trace ("NetDecl.ids: unknown class " ++ cls') $ skipRest >> return []

    "N7Verific20VeriContinuousAssignE" -> do
        skip    -- Strength
        listOf $ nodeCls "N7Verific16VeriNetRegAssignE" $ \id' -> do
            skip    -- LValExpr
            skip    -- RValExpr
            return $ SMINormal $ ContAssign id'

    "N7Verific19VeriAlwaysConstructE" -> do
        s <- stmt
        return [SMINormal $ Always s]

    "N7Verific20VeriInitialConstructE" -> do
        s <- stmt
        return [SMINormal $ Initial s]

    _ -> trace ("modItems: unknown class " ++ cls) $ skipRest >> return []

portDecls :: DecodeM [PortDecl]
portDecls = node $ \id cls -> case cls of
    "N7Verific12VeriVariableE" -> do
        name <- text
        skip    -- GetDataType()
        skip    -- GetDimensions()
        skip    -- GetInitialValue()
        dir <- integer
        return [PortDecl id name dir]
    _ -> trace ("portDecls: unknown class " ++ cls) $ skipRest >> return []

paramDecls :: DecodeM [ParamDecl]
paramDecls = node $ \id cls -> case cls of
    "N7Verific11VeriParamIdE" -> (:[]) <$> paramIdParts id
    _ -> trace ("paramDecls: unknown class " ++ cls) $ skipRest >> return []

paramIdParts id = do
    skip    -- DataType
    skip    -- InitialValue
    skip    -- ParamType
    skip    -- Dimensions
    skip    -- Actual
    return $ ParamDecl id

dataDeclId :: DecodeM ()
dataDeclId = node $ \id cls -> trace cls $ skipRest

stmt = node $ \id cls -> case cls of
    "N7Verific12VeriSeqBlockE" -> do
        skip    -- Label
        decls <- flatListOf $ node $ \id' cls' -> case cls' of
            _ -> trace ("SeqBlock.declItems: unknown class " ++ cls') $ skipRest >> return []
        ss <- listOf stmt
        return $ Block ss

    -- TODO: flatten EventControl.At into Always
    "N7Verific25VeriEventControlStatementE" -> do
        skip    -- At
        s <- stmt
        return s

    "N7Verific24VeriConditionalStatementE" -> do
        cond <- expr
        then_ <- stmt
        else_ <- optional stmt
        return $ If cond then_ else_

    "N7Verific17VeriCaseStatementE" -> do
        skip    -- CaseStyle
        skip    -- CaseType
        cond <- expr
        items <- listOf $ nodeCls "N7Verific12VeriCaseItemE" $ \id -> do
            es <- listOf expr
            s <- stmt
            return (es, s)
        return $ Case cond items

    "N7Verific7VeriForE" -> do
        skip    -- Initials
        skip    -- Condition
        skip    -- Repetitions
        body <- stmt
        return $ For body

    "N7Verific21VeriNonBlockingAssignE" -> do
        lval <- expr
        rval <- expr
        return $ NonBlockingAssign lval rval

    "N7Verific18VeriBlockingAssignE" -> do
        lval <- expr
        rval <- expr
        return $ BlockingAssign lval rval

    _ -> trace ("stmt: unknown class " ++ cls) $ skipRest >> return UnknownStmt

expr = node $ \id cls -> case cls of
    "N7Verific9VeriIdRefE" -> do
        defId <- nodeId
        return $ Var defId

    "N7Verific13VeriIndexedIdE" -> do
        base <- expr
        index <- expr
        skip    -- Id
        return $ Index base index

    "N7Verific12VeriConstValE" -> do
        t <- text
        skip    -- Size
        skip    -- Sign
        return $ Const t

    "N7Verific10VeriIntValE" -> do
        t <- text
        skip    -- Size
        skip    -- Sign
        skip    -- Num
        return $ Const t

    "N7Verific11VeriRealValE" -> do
        t <- text
        skip    -- Size
        skip    -- Sign
        skip    -- Num
        return $ Const t

    "N7Verific10VeriConcatE" -> do
        es <- listOf expr
        return $ Concat es

    "N7Verific17VeriQuestionColonE" -> do
        cond <- expr
        then_ <- expr
        else_ <- expr
        return $ IfExpr cond then_ else_


    _ -> trace ("expr: unknown class " ++ cls) $ skipRest >> return UnknownExpr



deserialize :: DecodeM a -> BS.ByteString -> Either String a
deserialize d bs = case CBOR.deserialiseFromBytes CBOR.decodeTerm bs of
    Left cborErr -> Left $ show cborErr
    Right (_, term) -> fst <$> runDecodeM d [[term]]



-- Source-level mod items include a few additional types that we don't include
-- in the ModItem data type.
data SourceModItem = SMINormal ModItem | SMIParam ParamDecl
  deriving (Show)

partitionSourceModItems xs = go xs
  where go [] = ([], [])
        go (SMINormal x : xs) = let (a, b) = go xs in (x : a, b)
        go (SMIParam x : xs) = let (a, b) = go xs in (a, x : b)
