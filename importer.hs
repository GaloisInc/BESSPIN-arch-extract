import Control.Monad
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word

import Debug.Trace

import Codec.CBOR.Decoding
import Codec.CBOR.Read
import Codec.CBOR.Term

import Language.Clafer
import Language.Clafer.Common
import Language.Clafer.Front.AbsClafer
import Language.Clafer.Front.PrintClafer
import qualified Language.Clafer.ClaferArgs as Args

type NodeId = Word

data VModule = VModule
    { moduleId :: NodeId
    , moduleName :: Text
    , moduleItems :: [VModItem]
    }
    deriving (Show)

data VModItem =
    VModInst
    { modItemId :: NodeId
    , modInstModId :: NodeId
    , modInstName :: Text
    }
    deriving (Show)

data VInstId = VInstId
    { instIdName :: Text
    }
    deriving (Show)

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

decodeNodeId = decodeWord

skipNode = decodeTerm >> return ()

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
    return $ VModule id name items

-- Returns a list because we flatten some kinds of items during decoding
decodeModItems :: Decoder s [VModItem]
decodeModItems = decodeNode $ \id clsName -> do
    case clsName of
        "N7Verific23VeriModuleInstantiationE" -> do
            modId <- decodeNodeId
            insts <- decodeListOf decodeInstId
            return $ map (\i -> VModInst id modId (instIdName i)) insts
        _ -> return []

decodeInstId :: Decoder s VInstId
decodeInstId = decodeNode $ \id clsName -> do
    name <- decodeString
    skipNode -- GetPortConnects
    return $ VInstId name







convName x = T.unpack x ++ "_"

cfrModule :: Text -> [Element] -> Element
cfrModule name elts = Subclafer noSpan $ Clafer noSpan
    (Abstract noSpan)
    []
    (GCardEmpty noSpan)
    (mkIdent $ convName name)
    (SuperEmpty noSpan)
    (ReferenceEmpty noSpan)
    (CardEmpty noSpan)
    (InitEmpty noSpan)
    (TransitionEmpty noSpan)
    (ElementsList noSpan elts)

cfrModInst :: Text -> Text -> Element
cfrModInst modName instName = Subclafer noSpan $ Clafer noSpan
    (AbstractEmpty noSpan)
    []
    (GCardEmpty noSpan)
    (mkIdent $ convName instName)
    (SuperSome noSpan $ ClaferId noSpan $
        Path noSpan [ModIdIdent noSpan $ mkIdent $ convName modName])
    (ReferenceEmpty noSpan)
    (CardEmpty noSpan)
    (InitEmpty noSpan)
    (TransitionEmpty noSpan)
    (ElementsEmpty noSpan)



genClafer v = Module noSpan $ map (ElementDecl noSpan) $ map go v ++ [root]
  where
    modMap = M.fromList [(moduleId m, m) | m <- v]

    go (m@VModule {}) = cfrModule (moduleName m) (concatMap goItem $ moduleItems m)
    goItem (mi@VModInst {}) = case M.lookup (modInstModId mi) modMap of
        Just m -> [cfrModInst (moduleName m) (modInstName mi)]
        Nothing -> trace ("warning: mod ref to unknown ID " ++ show (modInstModId mi)) []

    root = cfrModInst (T.pack "leg") (T.pack "root")


putAst a = do
    env <- getEnv
    putEnv env { cAst = Just a }

compiled ast = runClafer defaultClaferArgs { mode = [Args.Clafer] } $ do
    putAst ast
    iModule <- desugar Nothing
    compile iModule
    generate

{-
main__ = putStrLn $ printTree myAst
main_ = case compiled of
    Left errs -> mapM_ print errs
    Right outs -> mapM_ (putStrLn . outputCode) outs
-}


main = do
    bs <- BS.readFile "out.cbor"
    let (extra, v) = case deserialiseFromBytes (decodeListOf decodeModule) bs of
                Left errs -> error $ show errs
                Right x -> x
    case compiled $ genClafer v of
        Left errs -> mapM_ print errs
        Right outs -> mapM_ (putStrLn . outputCode) outs

