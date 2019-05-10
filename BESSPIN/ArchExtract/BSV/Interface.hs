{-# LANGUAGE OverloadedStrings, Rank2Types, DeriveGeneric, DeriveAnyClass #-}
module BESSPIN.ArchExtract.BSV.Interface
( IfcSpec(..), IfcEntry(..), IfcItem(..), IfcMethod(..), MethodKind(..)
, methodInPorts, methodOutPorts, itemInPorts, itemOutPorts
, dummyIfc
, translateIfcStructs
) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.Array.ST
import Data.Foldable
import Data.Generics hiding (Generic)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UnionFind.ST as UF
import GHC.Generics
import Lens.Micro.Platform
import Text.Read (readMaybe)

import Debug.FilterTrace
import Data.List

import BESSPIN.ArchExtract.BSV.Raw
import BESSPIN.ArchExtract.BSV.PrintRaw


TraceAPI trace traceId traceShow traceShowId traceM traceShowM = mkTraceAPI "BSV.Interface"


data IfcSpec = IfcSpec
    { ifcEntries :: [(Text, IfcEntry)]
    , ifcInPorts :: Int
    , ifcOutPorts :: Int
    }
    deriving (Show, Generic, NFData)

data IfcEntry = IfcEntry
    { ieItem :: IfcItem
    , ieFirstInPort :: Int
    , ieFirstOutPort :: Int
    }
    deriving (Show, Generic, NFData)

data IfcItem = IiMethod IfcMethod | IiSubIfc IfcSpec
    deriving (Show, Generic, NFData)

data IfcMethod = IfcMethod
    { imKind :: MethodKind
    , imName :: Text
    , imArgNames :: [Text]
    , imArgCounts :: (Int, Int)
    }
    deriving (Show, Generic, NFData)

data MethodKind =
      MkComb
    -- First flag indicates that the method's argument is a dummy token
    -- argument, added  because the method has no real arguments.  Second flag
    -- indicates that the method has a return value (non-`TUnit`).
    | MkAction Bool Bool
    deriving (Show, Generic, NFData)


dummyIfc = IfcSpec [] 0 0


-- Generic helper for building a map of recursive data structures, where some
-- values in the map contain references to others.
fixMap :: (Ord k) => (forall m. Monad m => (k -> m v) -> k -> m v) -> [k] -> Either k (Map k v)
fixMap f ks = do
    m <- execStateT (mapM_ get ks) M.empty
    return $ fmap (\x -> case x of
        Left _ -> error "fixMap: impossible: Left remained after building all keys?"
        Right v -> v) m
  where
    -- The actual monad we use for `m` is:
    --      StateT (Map k (Either k v)) (Either k) v
    -- The map contains `Right v` for keys where evaluation succeeded, and
    -- `Left k` for keys where evaluation is ongoing.  Accessing an ongoing key
    -- causes an exception in the `Either` monad containing the value of the
    -- first key encountered in the infinite loop.
    --build :: k -> StateT (Map k (Either k v)) (Either k) v
    build k = f get k

    --get :: k -> StateT (Map k (Either k v)) (Either k) v
    get k = do
        cached <- use $ at k
        case cached of
            -- `v` has type `Either k v`, so we can simply `lift` it to either
            -- return the value or throw an exception, depending on the status
            -- of this entry.
            Just v -> lift v
            Nothing -> do
                at k .= Just (Left k)
                v <- build k
                at k .= Just (Right v)
                return v


translateIfcStructs :: Map Text Struct -> Map Text IfcSpec
translateIfcStructs ss = case fixMap go (M.keys ss') of
    Left k -> error $ "infinite loop when processing interface " ++ show k
    Right x -> x
  where
    ss' = M.filter structIsIfc ss

    go get k = do
        ifc <- case M.lookup k ss' of
            Nothing -> traceShow ("unknown ifc", k) $ return dummyIfc
            Just s -> layoutIfc <$> mapM (goField get) (structFields s)
        traceM $ T.unpack $ "interface " <> k <> ":\n" <> T.unlines (renderIfc ifc)
        return ifc

    goField get (Field (Id name _ _) ty _)
      | (TIfc (Id subIfcName _ _), _) <- splitAppTy $ resolveTy ty = do
        subIfc <- get subIfcName
        return (name, IiSubIfc subIfc)
    goField get (Field (Id name _ _) ty optArgNames) =
        return (name, IiMethod $ IfcMethod kind name argNames argCounts)
      where
        (tys, args0, ret) = splitFnTy ty

        (kind, addTokenArg) = case resolveTy ret of
            TAction t ->
                let addToken = null args0 in
                let hasRet = case t of TUnit -> False; _ -> True in
                (MkAction addToken hasRet, addToken)
            _ -> (MkComb, False)

        providedArgNames = maybe [] (map idName) optArgNames
        autoArgNames = map (\i -> "_arg" <> T.pack (show i))
            [length providedArgNames .. length args0 - 1]
        argNames0 = providedArgNames ++ autoArgNames

        (args, argNames) =
            if not addTokenArg then (args0, argNames0)
            else ([TUnit], ["_go"])

        argCounts = (length tys, length args0)

methodInPorts m = length $ imArgNames m

methodOutPorts (IfcMethod { imKind = MkAction _ False }) = 0
methodOutPorts _ = 1

itemInPorts (IiMethod m) = methodInPorts m
itemInPorts (IiSubIfc ifc) = ifcInPorts ifc

itemOutPorts (IiMethod m) = methodOutPorts m
itemOutPorts (IiSubIfc ifc) = ifcOutPorts ifc


layoutIfc :: [(Text, IfcItem)] -> IfcSpec
layoutIfc items = IfcSpec entries numIn numOut
  where
    entries :: [(Text, IfcEntry)]
    (entries, (numIn, numOut)) = runState (mapM go items) (0, 0)

    go :: (Text, IfcItem) -> State (Int, Int) (Text, IfcEntry)
    go (name, item) = do
        (curIn, curOut) <- get
        _1 %= (+ itemInPorts item)
        _2 %= (+ itemOutPorts item)
        return $ (name, IfcEntry item curIn curOut)


renderIfc :: IfcSpec -> [Text]
renderIfc ifc = concatMap (go "  ") $ ifcEntries ifc
  where
    go indent (name, entry) = case ieItem entry of
        IiMethod m -> [indent <> "method " <> name <> ": " <> T.pack (show m)]
        IiSubIfc i -> [indent <> "interface " <> name <> ":"] ++
            concatMap (go (indent <> "  ")) (ifcEntries i)
