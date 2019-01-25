{-# LANGUAGE RankNTypes #-}
module BESSPIN.ArchExtract.Verilog.Decode where

import Control.Monad
import qualified Data.ByteString.Lazy as BS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word

import Debug.Trace

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR

import BESSPIN.ArchExtract.Verilog.Raw


data S = S
    { sStack :: [[CBOR.Term]]
    , sNodes :: Map NodeId Node
    }

newtype DecodeM a = DecodeM { runDecodeM :: S -> Either String (a, S) }

instance Monad DecodeM where
    f >>= k = DecodeM $ \s -> do
        (x, s') <- runDecodeM f s
        runDecodeM (k x) s'
    return x = DecodeM $ \s -> Right (x, s)
    fail msg = DecodeM $ \s -> Left msg

instance Applicative DecodeM where
    pure = return
    (<*>) = ap

instance Functor DecodeM where
    fmap = liftM


-- Primitive `DecodeM` operations

-- Apply `f` to the next term, either parsing it as an `a` or raising an error.
match' :: (CBOR.Term -> DecodeM a) -> DecodeM a
match' f = DecodeM $ \s -> case sStack s of
    [] -> Left "empty stack"
    [] : _ -> Left "read past end of list"
    (x : terms) : stk -> runDecodeM (f x) (s { sStack = terms : stk })

-- Apply `f` to the next term, either parsing it as an `a` or raising an error.
match :: (CBOR.Term -> Either String a) -> DecodeM a
match f = match' $ \t -> case f t of
    Left err -> fail err
    Right x -> return x

-- Discard the next term.
skip :: DecodeM ()
skip = match' $ const $ return ()

-- Discard all remaining terms in the current list.
skipRest :: DecodeM ()
skipRest = DecodeM $ \s -> case sStack s of
    [] -> Left "empty stack"
    _ : stk -> Right ((), s { sStack = [] : stk })

record :: NodeId -> Node -> DecodeM ()
record nodeId n = DecodeM $ \s ->
    let s' = s { sNodes = M.insert nodeId n $ sNodes s } in
    Right ((), s')

-- Parse the next term as a list, and "enter" it by pushing its list of subterms
-- onto the stack.
pushList :: [CBOR.Term] -> DecodeM ()
pushList ts = DecodeM $ \s -> Right ((), s { sStack = ts : sStack s })

-- "Exit" the current list by popping it from the stack.  Raises an error if
-- the current list is not empty, as this usually indicates that the parsing
-- code is incomplete.
popList :: DecodeM ()
popList = DecodeM $ \s -> case sStack s of
    [] -> Left "empty stack"
    [] : stk -> Right ((), s { sStack = stk })
    terms : stk -> Left $ show (length terms) ++ " unparsed items at end of list"

-- Return the next term without consuming it.  Returns `Nothing` when at the
-- end of the current list.
peek :: DecodeM (Maybe CBOR.Term)
peek = DecodeM $ \s -> case sStack s of
    [] -> Left "empty stack"
    [] : _ -> Right (Nothing, s)
    (x : _) : _ -> Right (Just x, s)

-- Run `m`, mapping `f` over any error it raises.
mapErr :: (String -> String) -> DecodeM a -> DecodeM a
mapErr f m = DecodeM $ \s -> case runDecodeM m s of
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

null_ :: DecodeM ()
null_ = match $ \t -> case t of
    CBOR.TNull -> return ()
    t -> Left $ "expected null, but got " ++ describe t

-- Parse the contents of the next term, which must be a list, using `m`.
list :: DecodeM a -> DecodeM a
list m = do
    ts <- match' $ \t -> case t of
        CBOR.TList ts -> return ts
        CBOR.TListI ts -> return ts
        t -> fail $ "expected list, but got " ++ describe t
    pushList ts
    x <- m
    popList
    return x

-- Check if we're at the end of the current list.
eol :: DecodeM Bool
eol = (== Nothing) <$> peek

-- Run `m` in a scope described by `loc`.  If `m` raises an error, `traceScope`
-- will extend the error message with "backtrace" information including `loc`.
traceScope :: Show b => b -> DecodeM a -> DecodeM a
traceScope loc m = mapErr (\err -> "at " ++ show loc ++ ":\n" ++ err) m

nodeId :: DecodeM NodeId
nodeId = NodeId <$> fromIntegral <$> integer

-- Parse a node (using `m`), a CBOR NodeId, or null, returning the parsed
-- NodeId.
optNodeWith :: (Text -> DecodeM Node) -> DecodeM (Maybe NodeId)
optNodeWith f = match' $ \t -> case t of
    CBOR.TNull -> return Nothing
    CBOR.TInt i -> return $ Just $ NodeId $ fromIntegral i
    CBOR.TInteger i -> return $ Just $ NodeId $ fromIntegral i
    CBOR.TList ts -> Just <$> go ts
    CBOR.TListI ts -> Just <$> go ts
    t -> fail $ "expected list, integer, or null, but got " ++ describe t
  where
    go ts = do
        id_ <- traceScope "node header" $ do
            pushList ts
            nodeId
        cls <- traceScope ("node", id_, "header") $ do
            text
        traceScope (cls, id_) $ do
            n <- f cls
            popList
            record id_ n
        return id_

nodeWith :: (Text -> DecodeM Node) -> DecodeM NodeId
nodeWith f = optNodeWith f >>= \x -> case x of
    Nothing -> fail "expected node, but got null"
    Just x -> return x


-- Helpers

-- Parse each remaining term in the current list using `m`.
listOf :: DecodeM a -> DecodeM [a]
listOf m = list go
  where go = do
            atEol <- eol
            if atEol then return [] else (:) <$> m <*> go

flatListOf :: DecodeM [a] -> DecodeM [a]
flatListOf m = concat <$> listOf m

mapOf :: Ord k => DecodeM k -> DecodeM v -> DecodeM (Map k v)
mapOf mk mv = match' $ \t -> case t of
    CBOR.TMap m -> go m
    CBOR.TMapI m -> go m
  where
    go m = M.fromList <$> mapM (\(kt,vt) -> do
        pushList [kt, vt]
        k <- mk
        v <- mv
        popList
        return (k, v)) m

-- Parse the next term using `m`, unless the next term is null.
optional :: DecodeM a -> DecodeM (Maybe a)
optional m = do
    tok <- peek
    case tok of
        Nothing -> fail "read past end of list"
        Just CBOR.TNull -> skip >> return Nothing
        _ -> Just <$> m


-- Verilog AST decoding

node = nodeWith clsNode
optNode = optNodeWith clsNode
nodes = listOf node

nodeMap = mapOf text node

clsNode :: Text -> DecodeM Node
clsNode cls = case T.unpack cls of
    "N7Verific10VeriModuleE" -> do
        name <- text
        traceShow ("in module", name) $ return ()
        node    -- Id()
        ports <- nodes
        params <- nodes
        items <- nodes
        nodes   -- PortConnects()
        nodes   -- ParameterConnects()
        nodes   -- PackageImportDecls()
        return $ Module name ports params items

    "N7Verific12VeriDataDeclE" -> do
        -- All DataDecl info is also available directly on the `ids`.
        skip    -- DeclType
        skip    -- Dir
        optNode -- DataType
        ids <- nodes
        return $ DataDecl ids

    "N7Verific12VeriVariableE" -> do
        name <- text
        optNode -- DataType - null for implicitly declared vars
        dims <- optNode
        init <- optNode
        dir <- optPortDir
        return $ Variable name dims init dir

    "N7Verific10VeriTypeIdE" -> do
        node    -- ModuleItem
        return $ TypeId

    "N7Verific12VeriDataTypeE" -> do
        skip    -- Type
        skip    -- Signing
        dims <- optNode
        return $ DataType dims

    "N7Verific11VeriParamIdE" -> do
        name <- text
        optNode -- DataType
        init <- optNode
        skip    -- ParamType
        dims <- optNode
        optNode -- Actual
        return $ ParamId name init dims

    "N7Verific8VeriEnumE" -> do
        base <- optNode
        variants <- nodeMap
        return $ Enum base variants

    "N7Verific23VeriModuleInstantiationE" -> do
        mod <- node
        paramVals <- nodes
        ids <- nodes
        return $ ModuleInstantiation mod paramVals ids

    "N7Verific10VeriInstIdE" -> do
        parent <- node
        name <- text
        portConns <- nodes
        return $ InstId parent name portConns

    "N7Verific11VeriNetDeclE" -> do
        node    -- DeclType
        skip    -- Dir
        node    -- DataType
        optNode -- Strength
        ids <- nodes
        return $ NetDecl ids

    "N7Verific20VeriContinuousAssignE" -> do
        optNode -- Strength
        assigns <- nodes
        return $ ContinuousAssign assigns

    "N7Verific16VeriNetRegAssignE" -> do
        lval <- node
        rval <- node
        return $ NetRegAssign lval rval

    "N7Verific19VeriAlwaysConstructE" -> do
        s <- node
        return $ AlwaysConstruct s

    "N7Verific20VeriInitialConstructE" -> do
        s <- node
        return $ InitialConstruct s

    -- Statements

    "N7Verific12VeriSeqBlockE" -> do
        skip    -- Label
        decls <- nodes
        ss <- nodes
        return $ SeqBlock decls ss

    -- TODO: flatten EventControl.At into Always
    "N7Verific25VeriEventControlStatementE" -> do
        nodes   -- At
        s <- node
        return $ EventControlStatement s

    "N7Verific24VeriConditionalStatementE" -> do
        cond <- node
        then_ <- node
        else_ <- optNode
        return $ ConditionalStatement cond then_ else_

    "N7Verific17VeriCaseStatementE" -> do
        skip    -- CaseStyle
        skip    -- CaseType
        cond <- node
        items <- nodes
        return $ CaseStatement cond items

    "N7Verific12VeriCaseItemE" -> do
        es <- nodes
        s <- node
        return $ CaseItem es s

    "N7Verific7VeriForE" -> do
        inits <- nodes
        cond <- node
        steps <- nodes
        body <- node
        return $ For inits cond steps body

    "N7Verific21VeriNonBlockingAssignE" -> do
        lval <- node
        optNode -- Control
        rval <- node
        return $ NonBlockingAssign lval rval

    "N7Verific18VeriBlockingAssignE" -> do
        lval <- node
        optNode -- Control
        rval <- optNode
        oper <- integer
        case (oper, rval) of
            (413, Just rval) -> return $ BlockingAssign lval rval
            (498, Nothing) -> return $ BlockingAssignInPlace lval
            (499, Nothing) -> return $ BlockingAssignInPlace lval

    "N7Verific25VeriDelayControlStatementE" -> do
        node    -- delay
        s <- node
        return $ DelayControlStatement s

    "N7Verific17VeriNullStatementE" -> do
        return NullStatement

    -- Expressions

    "N7Verific9VeriIdRefE" -> do
        def <- node
        return $ IdRef def

    "N7Verific13VeriIndexedIdE" -> do
        base <- node
        ix <- node
        node    -- Id
        return $ IndexedId base ix

    "N7Verific19VeriIndexedMemoryIdE" -> do
        base <- node
        ixs <- nodes
        return $ IndexedMemoryId base ixs

    "N7Verific12VeriConstValE" -> do
        t <- text
        skip    -- Size
        skip    -- Sign
        return $ ConstVal t

    "N7Verific10VeriIntValE" -> do
        t <- text
        skip    -- Size
        skip    -- Sign
        skip    -- Num
        return $ IntVal t

    "N7Verific11VeriRealValE" -> do
        t <- text
        skip    -- Size
        skip    -- Sign
        skip    -- Num
        return $ RealVal t

    "N7Verific10VeriConcatE" -> do
        es <- nodes
        return $ Concat es

    "N7Verific15VeriMultiConcatE" -> do
        rep <- node
        es <- nodes
        return $ MultiConcat rep es

    "N7Verific17VeriQuestionColonE" -> do
        cond <- node
        then_ <- node
        else_ <- node
        return $ QuestionColon cond then_ else_

    "N7Verific17VeriUnaryOperatorE" -> do
        oper <- integer
        arg <- node
        return $ UnaryOperator arg

    "N7Verific18VeriBinaryOperatorE" -> do
        oper <- integer
        left <- node
        right <- node
        return $ BinaryOperator left right

    "N7Verific16VeriSelectedNameE" -> do
        base <- node
        name <- text
        optNode -- FullId
        return $ SelectedName base name

    "N7Verific26VeriMultiAssignmentPatternE" -> do
        optNode -- TargetType
        repeat <- node
        exprs <- nodes
        return $ MultiAssignmentPattern repeat exprs

    "N7Verific9VeriRangeE" -> do
        left <- node
        -- For `x[$]`, `right` can be null.
        right <- optNode
        skip    -- PartSelectToken
        skip    -- IsUnpacked
        skip    -- LeftRangeBound
        skip    -- RightRangeBound
        -- Ranges can theoretically be chained as linked lists, but in practice
        -- that never seems to happen.
        null_   -- Next
        return $ Range left right


    _ -> unknown cls

ignored cls = skipRest >> return (Ignored cls)
unknown cls = skipRest >> return (Unknown cls)


-- Token parsing.  Verific uses raw token numbers from their lexer for a
-- variety of purposes.

portDir :: DecodeM PortDir
portDir = optPortDir >>= \x -> case x of
    Nothing -> fail "expected port direction"
    Just d -> return d

optPortDir :: DecodeM (Maybe PortDir)
optPortDir = integer >>= \x -> case x of
    0 -> return Nothing
    329 -> return $ Just InOut
    330 -> return $ Just Input
    346 -> return $ Just Output
    _ -> fail $ "unknown PortDir enum: " ++ show x


-- Bytestring deserialization

deserialize :: BS.ByteString -> Either String (Map NodeId Node, [NodeId])
deserialize bs = case CBOR.deserialiseFromBytes CBOR.decodeTerm bs of
    Left cborErr -> Left $ show cborErr
    Right (_, term) ->
        runDecodeM (listOf $ nodeWith clsNode) (S [[term]] M.empty) >>= \(is, s) ->
            return (sNodes s, is)