{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module BESSPIN.ArchExtract.BSV.Interface where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.Array.ST
import Data.Foldable
import Data.Generics
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
import Lens.Micro.Platform
import Text.Read (readMaybe)

import Debug.Trace
import Data.List

import BESSPIN.ArchExtract.BSV.Raw
import BESSPIN.ArchExtract.BSV.PrintRaw


data IfcSpec = IfcSpec
    { ifcEntries :: [(Text, IfcEntry)]
    , ifcInPorts :: Int
    , ifcOutPorts :: Int
    }
    deriving (Show)

data IfcEntry = IfcEntry
    { ieItem :: IfcItem
    , ieFirstInPort :: Int
    , ieFirstOutPort :: Int
    }
    deriving (Show)

data IfcItem = IiMethod IfcMethod | IiSubIfc IfcSpec
    deriving (Show)

data IfcMethod = IfcMethod
    { imKind :: MethodKind
    , imName :: Text
    , imArgNames :: [Text]
    , imArgCounts :: (Int, Int)
    }
    deriving (Show)

data MethodKind =
      MkComb
    -- `True` means it has a return value / output port
    | MkAction Bool 
    deriving (Show)


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

    go get k = case M.lookup k ss' of
        Nothing -> traceShow ("unknown ifc", k) $ return dummyIfc
        Just s -> layoutIfc <$> mapM (goField get) (structFields s)

    goField get (Field (Id name _ _) (TIfc (Id subIfcName _ _)) _) = do
        subIfc <- get subIfcName
        return (name, IiSubIfc subIfc)
    goField get (Field (Id name _ _) ty optArgNames) =
        return (name, IiMethod $ IfcMethod kind name argNames argCounts)
      where
        (tys, args, ret) = splitFnTy ty
        kind = case ret of
            TAction TUnit -> MkAction False
            TAction _ -> MkAction True
            _ -> MkComb

        providedArgNames = maybe [] (map idName) optArgNames
        autoArgNames = map (\i -> "_arg" <> T.pack (show i))
            [length providedArgNames .. length args - 1]
        argNames = providedArgNames ++ autoArgNames

        argCounts = (length tys, length args)

methodInPorts m = length $ imArgNames m

methodOutPorts (IfcMethod { imKind = MkAction False }) = 0
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
