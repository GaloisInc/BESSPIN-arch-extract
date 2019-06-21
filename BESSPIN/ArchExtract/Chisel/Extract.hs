{-# LANGUAGE OverloadedStrings, TemplateHaskell, PatternSynonyms, DeriveFunctor,
    DeriveDataTypeable #-}
module BESSPIN.ArchExtract.Chisel.Extract where

import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.Generics
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as M
import Data.Maybe
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro.Platform

import Debug.FilterTrace

import qualified BESSPIN.ArchExtract.Config as Config
import BESSPIN.ArchExtract.Lens
import qualified BESSPIN.ArchExtract.Architecture as A
import BESSPIN.ArchExtract.Chisel.FIRRTL.AST
import BESSPIN.ArchExtract.Simplify (reconnectNets, mergeAliasedNets)


TraceAPI trace traceId traceShow traceShowId traceM traceShowM = mkTraceAPI "Chisel.Extract"


type NodeId = Text

data Projection = PrIndex Int | PrField Text
    deriving (Show, Eq, Ord, Data)

data ConnExpr = ConnExpr NodeId (Seq Projection)
    deriving (Show, Eq, Ord, Data)

data Rvalue =
      RvExpr ConnExpr
    | RvMux Rvalue Rvalue Rvalue
    -- Specialized `RvMux` used for assignment to a non-constant index:
    -- `array[idx_expr] <= value`.  Such statements unroll into many copies of
    -- `when idx_expr == <i>: array[<i>] <= value`.  The unrolled form turns
    -- into an assignment `array[<i>] <= RvIndexMux idx_expr <i> value <prev>`.
    | RvIndexMux Rvalue Int Rvalue Rvalue
    -- Combinational logic node.  Takes zero or more inputs, and produces one
    -- output.
    | RvComb Text [Rvalue]
    -- Initial value of every node.  Also, the assignment RHS for `x is
    -- invalid` statements.
    | RvInvalid
    -- Marker indicating that the value is unset / unconnected.  This often
    -- appears on the `else` side of `RvMux`, to handle `when` with no `else`.
    -- When unioning `ConnTree`s, `RvUnset` will be replaced with the value
    -- from the older tree.
    | RvUnset
    deriving (Show, Data)

data GConnection a =
      CGround' Ty Rvalue a
    | CBundle [Field] (Map Text (GConnection a))
    | CVector Ty Int (Map Int (GConnection a))
    deriving (Show, Functor)

type GConnTree a = Map NodeId (GConnection a)


-- "Normal" connection trees, with no annotations on the ground nodes.
type Connection = GConnection ()
pattern CGround ty rv = CGround' ty rv ()
type ConnTree = GConnTree ()
{-# COMPLETE CGround, CBundle, CVector #-}

-- Bidirectional connection trees, where ground connections can be either
-- forward or reversed.  These are used when rerolling bundle connections.
data ConnDir = Fwd | Rev
    deriving (Show, Eq)
type BiConnection = GConnection ConnDir
type BiConnTree = GConnTree ConnDir

flipConnDir Fwd = Rev
flipConnDir Rev = Fwd


connForType :: Ty -> Connection
connForType (TBundle fs) = CBundle fs M.empty
connForType (TVector ty len) = CVector ty len M.empty
connForType ty = CGround ty RvUnset

connTy :: GConnection a -> Ty
connTy (CGround' ty _ _) = ty
connTy (CBundle fs _) = TBundle fs
connTy (CVector ty len _) = TVector ty len

connExprNode :: ConnExpr -> NodeId
connExprNode (ConnExpr n _) = n

connTreeInsertNode :: NodeId -> Ty -> ConnTree -> ConnTree
connTreeInsertNode n ty m = M.insert n (connForType ty) m

connTreeEnsureNode :: NodeId -> Ty -> ConnTree -> ConnTree
connTreeEnsureNode n ty m = M.alter (\x -> case x of
    Nothing -> Just $ connForType ty
    Just x -> Just x) n m

connTreeInsertRvalue :: ConnExpr -> Rvalue -> ConnTree -> ConnTree
connTreeInsertRvalue l r m = connTreeAdjustRvalue l (\_ _ -> r) m

connTreeAdjustRvalue :: ConnExpr -> (Ty -> Rvalue -> Rvalue) -> ConnTree -> ConnTree
connTreeAdjustRvalue (ConnExpr n ps) f m = M.alter (\x -> case x of
    Nothing -> traceShow ("connTreeAdjustRvalue: bad node ID", n) Nothing
    Just x -> Just $ go (toList ps) x) n m
  where
    go :: [Projection] -> Connection -> Connection
    go [] (CGround ty rv) = CGround ty $ f ty rv
    go (PrIndex idx : ps) (CVector ty len m') | 0 <= idx, idx < len =
        CVector ty len $ M.alter (\x ->
            Just $ go ps $ fromMaybe (connForType ty) x) idx m'
    go (PrField name : ps) (CBundle fs m') | Just f <- find (\f -> fieldName f == name) fs =
        CBundle fs $ M.alter (\x ->
            Just $ go ps $ fromMaybe (connForType $ fieldTy f) x) (fieldName f) m'
    go ps c = traceShow ("connTreeAdjustRvalue: bad projection", ps) c

mapConn' :: (Ty -> Rvalue -> a -> (Rvalue, b)) -> GConnection a -> GConnection b
mapConn' f (CGround' ty rv ann) =
    let (rv', ann') = f ty rv ann in
    CGround' ty rv' ann'
mapConn' f (CBundle fs m) = CBundle fs $ fmap (mapConn' f) m
mapConn' f (CVector ty len m) = CVector ty len $ fmap (mapConn' f) m

mapConnTree' :: Ord k => (Ty -> Rvalue -> a -> (Rvalue, b)) ->
    Map k (GConnection a) -> Map k (GConnection b)
mapConnTree' f m = fmap (mapConn' f) m

mapConn :: (Ty -> Rvalue -> Rvalue) -> GConnection a -> GConnection a
mapConn f c = mapConn' (\ty rv ann -> (f ty rv, ann)) c

mapConnTree :: Ord k => (Ty -> Rvalue -> Rvalue) -> Map k (GConnection a) -> Map k (GConnection a)
mapConnTree f m = mapConnTree' (\ty rv ann -> (f ty rv, ann)) m


mapMaybeConn' :: (Ty -> Rvalue -> a -> Maybe (Rvalue, b)) -> GConnection a -> Maybe (GConnection b)
mapMaybeConn' f c@(CGround' ty rv ann) = case f ty rv ann of
    Just (rv', ann') -> Just $ CGround' ty rv' ann'
    Nothing -> Nothing
mapMaybeConn' f (CBundle fs m) = Just $ CBundle fs $ mapMaybeConnTree' f m
mapMaybeConn' f (CVector ty len m) = Just $ CVector ty len $ mapMaybeConnTree' f m

mapMaybeConnTree' :: Ord k => (Ty -> Rvalue -> a -> Maybe (Rvalue, b)) ->
    Map k (GConnection a) -> Map k (GConnection b)
mapMaybeConnTree' f m = M.mapMaybe (mapMaybeConn' f) m

mapMaybeConn :: (Ty -> Rvalue -> Maybe Rvalue) -> GConnection a -> Maybe (GConnection a)
mapMaybeConn f c =
    mapMaybeConn' (\ty rv ann -> f ty rv >>= \rv' -> return (rv', ann)) c

mapMaybeConnTree :: Ord k => (Ty -> Rvalue -> Maybe Rvalue) ->
    Map k (GConnection a) -> Map k (GConnection a)
mapMaybeConnTree f m =
    mapMaybeConnTree' (\ty rv ann -> f ty rv >>= \rv' -> return (rv', ann)) m


filterConn' :: (Ty -> Rvalue -> a -> Bool) -> GConnection a -> Maybe (GConnection a)
filterConn' f c@(CGround' ty rv ann)
  | f ty rv ann = Just c
  | otherwise = Nothing
filterConn' f (CBundle fs m) = Just $ CBundle fs $ filterConnTree' f m
filterConn' f (CVector ty len m) = Just $ CVector ty len $ filterConnTree' f m

filterConnTree' :: (Ty -> Rvalue -> a -> Bool) -> Map k (GConnection a) -> Map k (GConnection a)
filterConnTree' f m = M.mapMaybe (filterConn' f) m

filterConn :: (Ty -> Rvalue -> Bool) -> GConnection a -> Maybe (GConnection a)
filterConn f c = filterConn' (\ty rv ann -> f ty rv) c

filterConnTree :: (Ty -> Rvalue -> Bool) -> Map k (GConnection a) -> Map k (GConnection a)
filterConnTree f m = filterConnTree' (\ty rv ann -> f ty rv) m

mergeConn' ::
    (Ty -> Rvalue -> a -> (Rvalue, c)) ->
    (Ty -> Rvalue -> b -> (Rvalue, c)) ->
    (Ty -> Rvalue -> a -> Rvalue -> b -> (Rvalue, c)) ->
    GConnection a -> GConnection b -> GConnection c
mergeConn' _ _ g (CGround' ty rv1 ann1) (CGround' _ rv2 ann2) =
    let (rv', ann') = g ty rv1 ann1 rv2 ann2 in
    CGround' ty rv' ann'
mergeConn' fl fr g (CBundle fs m1) (CBundle _ m2) =
    CBundle fs $ mergeConnMap' fl fr g m1 m2
mergeConn' fl fr g (CVector ty idx m1) (CVector _ _ m2) =
    CVector ty idx $ mergeConnMap' fl fr g m1 m2
mergeConn' _ _ _ c1 c2 = error "mergeConn: tried to merge mismatched connections"

mergeConnMap' :: Ord k =>
    (Ty -> Rvalue -> a -> (Rvalue, c)) ->
    (Ty -> Rvalue -> b -> (Rvalue, c)) ->
    (Ty -> Rvalue -> a -> Rvalue -> b -> (Rvalue, c)) ->
    Map k (GConnection a) -> Map k (GConnection b) -> Map k (GConnection c)
mergeConnMap' fl fr g m1 m2 = M.merge
    (M.mapMissing $ \k c -> mapConn' fl c)
    (M.mapMissing $ \k c -> mapConn' fr c)
    (M.zipWithMatched $ \k c1 c2-> mergeConn' fl fr g c1 c2)
    m1 m2

mergeConn :: Semigroup a =>
    (Ty -> Rvalue -> Rvalue) ->
    (Ty -> Rvalue -> Rvalue) ->
    (Ty -> Rvalue -> Rvalue -> Rvalue) ->
    GConnection a -> GConnection a -> GConnection a
mergeConn fl fr g c1 c2 = mergeConn'
    (\ty rv ann -> (fl ty rv, ann))
    (\ty rv ann -> (fr ty rv, ann))
    (\ty rv1 ann1 rv2 ann2 -> (g ty rv1 rv2, ann1 <> ann2))
    c1 c2

mergeConnMap :: (Ord k, Semigroup a) =>
    (Ty -> Rvalue -> Rvalue) ->
    (Ty -> Rvalue -> Rvalue) ->
    (Ty -> Rvalue -> Rvalue -> Rvalue) ->
    Map k (GConnection a) -> Map k (GConnection a) -> Map k (GConnection a)
mergeConnMap fl fr g m1 m2 = mergeConnMap'
    (\ty rv ann -> (fl ty rv, ann))
    (\ty rv ann -> (fr ty rv, ann))
    (\ty rv1 ann1 rv2 ann2 -> (g ty rv1 rv2, ann1 <> ann2))
    m1 m2


-- Join a pair of conditional branches.  Each ground/leaf node is assigned its
-- `tm` ("then") value when `c` is 1, and its `em` ("else") value when `c` is
-- 0.  If either value is missing, `RvUnset` is used.
connTreeJoin :: Rvalue -> ConnTree -> ConnTree -> ConnTree
connTreeJoin c tm em = mergeConnMap
    (\_ rv1 -> RvMux c rv1 RvUnset)
    (\_ rv2 -> RvMux c RvUnset rv2)
    (\_ rv1 rv2 -> RvMux c rv1 rv2)
    tm em

rvalueSubstUnset :: Rvalue -> Rvalue -> Rvalue
rvalueSubstUnset old rv = go rv
  where
    go (RvExpr ce) = RvExpr ce
    go (RvMux c t e) = RvMux (go c) (go t) (go e)
    go (RvIndexMux ie ic t e) = RvIndexMux (go ie) ic (go t) (go e)
    go (RvComb op args) = RvComb op (map go args)
    go RvInvalid = RvInvalid
    go RvUnset = old

-- Apply the updates in `new` atop those in `old`.  If `new` contains `RvUnset`
-- anywhere in some leaf, the `RvUnset` is replaced by the `old` value for the
-- same leaf; in all other cases, the `new` value replaces the `old`.
connTreeAppend :: ConnTree -> ConnTree -> ConnTree
connTreeAppend old new = mergeConnMap
    (\_ rv1 -> rv1)
    (\_ rv2 -> rv2)
    (\_ rv1 rv2 -> rvalueSubstUnset rv1 rv2)
    old new

connTreeConcat :: [ConnTree] -> ConnTree
connTreeConcat cts = foldl' (\acc ct -> connTreeAppend acc ct) M.empty cts

traceTree :: (Ord k, Show k) => Text -> Map k Connection -> a -> a
traceTree name ct x = trace (T.unpack $
    "conn tree " <> name <> ":\n" <> T.unlines (connTreePrint ct)) x


connTreePrint :: (Ord k, Show k) => Map k Connection -> [Text]
connTreePrint cm = go cm
  where
    go :: (Ord k, Show k) => Map k Connection -> [Text]
    go m = concatMap (\(k, c) -> header k c : indent (body c)) (M.toList m)

    header :: Show k => k -> Connection -> Text
    header k c = T.pack (show k) <> case c of
        CGround _ rv -> " <- " <> T.pack (show rv)
        CBundle fs m -> ": bundle " <> T.intercalate ", "
            [(if fieldFlip f then "FLIP " else "") <> fieldName f | f <- fs]
        CVector ty len m -> ": vector " <> T.pack (show len)

    body :: Connection -> [Text]
    body c = case c of
        CGround _ _ -> []
        CBundle _ m -> go m
        CVector _ _ m -> go m

    indent :: [Text] -> [Text]
    indent ts = map ("  " <>) ts

biConnTreePrint :: (Ord k, Show k) => Map k BiConnection -> [Text]
biConnTreePrint cm = go cm
  where
    go :: (Ord k, Show k) => Map k BiConnection -> [Text]
    go m = concatMap (\(k, c) -> header k c : indent (body c)) (M.toList m)

    header :: Show k => k -> BiConnection -> Text
    header k c = T.pack (show k) <> case c of
        CGround' _ rv Fwd -> " <- " <> T.pack (show rv)
        CGround' _ rv Rev -> " -> " <> T.pack (show rv)
        CBundle fs m -> ": bundle " <> T.intercalate ", "
            [(if fieldFlip f then "FLIP " else "") <> fieldName f | f <- fs]
        CVector ty len m -> ": vector " <> T.pack (show len)

    body :: BiConnection -> [Text]
    body c = case c of
        CGround' _ _ _ -> []
        CBundle _ m -> go m
        CVector _ _ m -> go m

    indent :: [Text] -> [Text]
    indent ts = map ("  " <>) ts



allSame :: Eq a => [a] -> Maybe a
allSame [] = Nothing
allSame [x] = Just x
allSame (x : x' : xs)
  | x == x' = allSame $ x' : xs
  | otherwise = Nothing

-- Description of a child node: connected (forward), connected (reversed),
-- invalid.  Only applies to `CGround'` children.  The `ConnDir` here gives the
-- direction of the enclosing aggregate: a `CGround' _ _ Rev` in a flipped
-- field produces a `CdConn _ Fwd`.
data ChildDesc = CdConn ConnExpr ConnDir | CdInvalid
    deriving (Show, Eq)

describeChild :: Projection -> Bool -> BiConnection -> Maybe ChildDesc
describeChild p flipped (CGround' _ (RvExpr (ConnExpr n ps)) dir)
  | ps' S.:> p' <- S.viewr ps, p == p'
  = Just $ CdConn (ConnExpr n ps') (if flipped then flipConnDir dir else dir)
describeChild p False (CGround' _ RvInvalid Fwd) = Just $ CdInvalid
describeChild _ _ _ = Nothing

describeChildren :: BiConnection -> Maybe ChildDesc
describeChildren (CBundle fs m) = join $ allSame $ map (\f -> do
    c <- M.lookup (fieldName f) m
    describeChild (PrField $ fieldName f) (fieldFlip f) c) fs
describeChildren (CVector _ len m) = join $ allSame $ map (\i -> do
    c <- M.lookup i m
    describeChild (PrIndex i) False c) [0 .. len - 1]
describeChildren _ = Nothing

rerollBiConnChildren :: BiConnection -> BiConnection
rerollBiConnChildren c@(CGround' _ _ _) = c
rerollBiConnChildren (CBundle fs m) = CBundle fs $ rerollBiConn <$> m
rerollBiConnChildren (CVector ty len m) = CVector ty len $ rerollBiConn <$> m

rerollBiConn :: BiConnection -> BiConnection
rerollBiConn c =
    let c' = rerollBiConnChildren c in
    case describeChildren c' of
        Just (CdConn ce dir) -> CGround' (connTy c) (RvExpr ce) dir
        Just CdInvalid -> CGround' (connTy c) RvInvalid Fwd
        Nothing -> c'

rerollBiConnTree :: BiConnTree -> BiConnTree
rerollBiConnTree ct = rerollBiConn <$> ct



collectFlippableConns :: GConnTree a -> [(ConnExpr, ConnExpr)]
collectFlippableConns ct = concatMap (\(n,c) -> go n S.empty c) $ M.toList ct
  where
    go n ps (CGround' _ (RvExpr ce) _) = [(ConnExpr n ps, ce)]
    go n ps (CGround' _ _ _) = []
    go n ps (CBundle _ m) =
        concatMap (\(name, c) -> go n (ps |> PrField name) c) $ M.toList m
    go n ps (CVector _ _ m) =
        concatMap (\(idx, c) -> go n (ps |> PrIndex idx) c) $ M.toList m

clearConn :: GConnection a -> Connection
clearConn (CGround' ty _ _) = CGround ty RvUnset
clearConn (CBundle fs _) = CBundle fs M.empty
clearConn (CVector ty len _) = CVector ty len M.empty

buildFlippedConnMap :: GConnTree a -> ConnTree
buildFlippedConnMap ct = ct'
  where
    conns = collectFlippableConns ct
    counts = M.fromListWith (+) $ map (\(_,ce) -> (ce, 1)) conns
    conns' = filter (\(_,ce) -> counts M.! ce == 1) conns

    base = fmap clearConn ct
    ct' = foldl' (\ct (l,r) -> connTreeInsertRvalue r (RvExpr l) ct) base conns'

addRevConns :: BiConnTree -> BiConnTree
addRevConns ct = mergeConnMap'
    (\_ rv ann -> (rv, ann))
    (\_ rv _ -> (rv, Rev))
    (\_ rv ann rv' _ -> case rv of
        -- TODO/HACK: fix IsInvalid handling to not assign RvInvalid to
        -- non-assignable nodes, then remove this check
        RvInvalid -> (rv', Rev)
        _ -> traceShow ("addRevConns: duplicate entry", rv, rv') (rv, ann))
    ct (buildFlippedConnMap ct)



rerollConnTree :: ConnTree -> ConnTree
rerollConnTree ct =
    mapMaybeConnTree' (\ty rv dir -> case dir of
        Fwd -> Just (rv, ())
        Rev -> Nothing) $
    rerollConnTree' ct

rerollConnTree' :: ConnTree -> BiConnTree
rerollConnTree' ct =
    rerollBiConnTree $
    addRevConns $
    mapConnTree' (\ty rv () -> (rv, Fwd)) $
    ct



data ModInfo = ModInfo
    { miName :: Text
    , miPorts :: [Port]
    , miModId :: A.ModId
    }

data MemoryDef = MemoryDef
    { mdTy :: Ty
    , mdDepth :: Int
    , mdPorts :: Seq MemPort
    }
    deriving (Show)

data MemPort = MemPort
    { mpAddrNet :: A.NetId
    -- Net IDs to connect to the LkRam's ports.  These are on the "inside" of
    -- each wire.
    , mpReadNet :: A.NetId
    , mpWriteNet :: A.NetId
    -- Node IDs on the "outside" of the two wires.  We look at these nodes to
    -- check if the read or write side is used.
    , mpReadNode :: NodeId
    , mpWriteNode :: NodeId
    }
    deriving (Show)

data ResolvedNode =
    -- Use `.0` as the node's ID in all contexts.
      RnSingle NodeId
    -- Use `.0` when the node appears in lvalue position and `.1` when it
    -- appears in rvalue position.
    | RnSplit NodeId NodeId
    deriving (Show, Eq)

data ExtractState = ExtractState
    { esCurModule :: A.Module ()
    , esModMap :: Map Text ModInfo
    , esLocalScope :: Map Text ResolvedNode
    , esNodeTy :: Map NodeId Ty
    -- List of all defs in the current module.  Hack: for defs that produce
    -- multiple nodes, such as `DWire`, the `NodeId` is the ID of the first
    -- node.  The other nodes' IDs are derived by applying a transformation to
    -- the ID, depending on the kind of def.
    , esDefs :: Seq (Def, ResolvedNode)
    , esMems :: Map Text MemoryDef
    -- Set of "split nodes".  A reference to a node `n` in this set gets turned
    -- into a reference to either the "left" or "right" variant of `n`,
    -- depending on whether the reference is in an lvalue or rvalue position.
    , esSplitNodes :: Set NodeId
    -- Gives the `NetId` assigned to each expression that appears in the
    -- `ConnTree`.  Populated by `buildNodes`.
    , esNetMap :: Map ConnExpr A.NetId
    }

makeLenses' ''ModInfo
makeLenses' ''MemoryDef
makeLenses' ''MemPort
makeLenses' ''ExtractState

type ExtractM a = State ExtractState a

-- Enter a new module scope.  `esCurModule` is initalized to `initMod` at the
-- start, and the final value of `esCurModule` is returned at the end.  All
-- `ExtractState` fields are restored to their previous values after running
-- `act`.
withModule :: A.Module () -> ExtractM a -> ExtractM (a, A.Module ())
withModule initMod act = do
    old <- get
    _esCurModule .= initMod
    r <- act
    newMod <- use _esCurModule
    put old
    return (r, newMod)

withScope :: ExtractM a -> ExtractM a
withScope act = do
    oldScope <- use _esLocalScope
    r <- act
    _esLocalScope .= oldScope
    return r

-- Create a new `Node` of the given type, with no `Def` and no entry in
-- `esLocalScope`.
addNode :: Text -> Ty -> ExtractM NodeId
addNode name ty = do
    i <- return name
    _esNodeTy %= M.insert i ty
    return i

bindDef :: Text -> Def -> Ty -> ExtractM NodeId
bindDef name def ty = do
    i <- return name    -- TODO: switch to numeric ids
    _esDefs %= (|> (def, RnSingle i))
    _esNodeTy %= M.insert i ty
    _esLocalScope %= M.insert name (RnSingle i)
    return i

bindSplitDef :: Text -> Def -> Ty -> ExtractM (NodeId, NodeId)
bindSplitDef name def ty = do
    il <- return $ name <> ".L"    -- TODO: switch to numeric ids
    ir <- return $ name <> ".R"    -- TODO: switch to numeric ids
    _esDefs %= (|> (def, RnSplit il ir))
    _esNodeTy %= M.insert il ty
    _esNodeTy %= M.insert ir ty
    _esLocalScope %= M.insert name (RnSplit il ir)
    return (il, ir)


extractDesign :: Config.Chisel -> Circuit -> A.Design ()
extractDesign cfg circ = evalState (extractDesign' cfg circ) initState
  where
    firMods = circuitModules circ
    modMap = M.fromList $ zipWith mkModInfo firMods [0..]

    mkModInfo m i = (name, ModInfo name ports i)
      where
        name = moduleName m
        ports = modulePorts m

    initState = ExtractState
        { esCurModule = error $ "current module is unset"
        , esModMap = modMap
        , esLocalScope = M.empty
        , esNodeTy = M.empty
        , esDefs = S.empty
        , esMems = mempty
        , esSplitNodes = Set.empty
        , esNetMap = M.empty
        }

extractDesign' :: Config.Chisel -> Circuit -> ExtractM (A.Design ())
extractDesign' cfg circ = do
    mods <- S.fromList <$> mapM (extractModule cfg) (circuitModules circ)
    return $ A.Design mods

extractModule :: Config.Chisel -> Module -> ExtractM (A.Module ())
extractModule cfg m = do
    ((), m') <- withModule initMod $ do
        traceM (T.unpack $ " --- begin extracting " <> moduleName m <> " ---")

        portConns <- foldl' connTreeAppend M.empty <$> mapM evalExtPort (modulePorts m)

        withScope $ do
            case moduleKind m of
                MkNormal body -> do
                    let bbox = Set.member (moduleName m) (Config.chiselBlackboxModules cfg)
                    when (not bbox) $ do
                        conns' <- evalStmt body
                        let conns'' = rerollConnTree $ connTreeAppend portConns conns'
                        traceM (T.unpack $ "new-style connections (orig): \n" <>
                            T.unlines (connTreePrint $ connTreeAppend portConns conns'))
                        traceM (T.unpack $ "new-style connections (rerolled): \n" <>
                            T.unlines (connTreePrint conns''))

                        void $ buildNodes conns''
                        void $ makeConnections conns''

                MkExtern _ -> return ()

    return $
        --mergeAliasedNets $
        reconnectNets $
        m'
  where
    initMod = A.Module (moduleName m) (convertModuleKind $ moduleKind m)
        S.empty S.empty S.empty S.empty S.empty S.empty

convertModuleKind (MkNormal _) = A.MkNormal
convertModuleKind (MkExtern _) = A.MkExtern

buildNet :: Text -> Int -> Ty -> ExtractM A.NetId
buildNet name prio ty = buildNet' name prio (convertTy ty)

buildNet' :: Text -> Int -> A.Ty -> ExtractM A.NetId
buildNet' name prio ty = zoom (_esCurModule . A._moduleNets) $ do
    idx <- gets S.length
    traceShowM ("buildNet", idx, name, prio, ty)
    modify (|> A.Net name prio S.empty S.empty ty ())
    return $ A.NetId idx

buildLogic :: A.LogicKind -> [A.NetId] -> [A.NetId] -> ExtractM Int
buildLogic kind ins outs = do
    ins' <- S.fromList <$> mapM buildPin ins
    outs' <- S.fromList <$> mapM buildPin outs
    zoom (_esCurModule . A._moduleLogics) $ do
        idx <- gets S.length
        modify (|> A.Logic kind ins' outs' ())
        return idx

buildPin :: A.NetId -> ExtractM A.Pin
buildPin n = do
    ty <- use $ _esCurModule . A._moduleNet n . A._netTy
    return $ A.Pin n ty

buildNetAlias :: A.NetId -> A.NetId -> ExtractM Int
buildNetAlias src dest = buildLogic A.LkNetAlias [src] [dest]

buildPort :: A.NetId -> Bool -> ExtractM ()
buildPort n output = do
    (name, ty) <- zoom (_esCurModule . A._moduleNet n) $ do
        name <- use A._netName
        ty <- use A._netTy
        return (head $ T.lines name, ty)
    if output then
        _esCurModule . A._moduleOutputs %= (|> A.Port name n ty)
    else
        _esCurModule . A._moduleInputs %= (|> A.Port name n ty)

buildDummyNet :: Text -> ExtractM A.NetId
buildDummyNet name = buildNet name (-10) TUnknown


convertTy :: Ty -> A.Ty
convertTy _ = A.TUnknown

nodeIdForSide :: Bool -> ResolvedNode -> NodeId
nodeIdForSide _ (RnSingle i) = i
nodeIdForSide right (RnSplit l r) = if right then r else l


-- Process a `Def`.  This updates `esDefs` and so on, but can also produce
-- connections in some cases, like the implicit connection on the RHS of a
-- `DNode`.
evalDef :: Def -> ExtractM ConnTree
evalDef d@(DWire name ty) = bindSplitDef name d ty >> return M.empty
evalDef d@(DReg name ty _clk _res _init) = bindSplitDef name d ty >> return M.empty
evalDef d@(DInst name modName) = use (_esModMap . at modName) >>= \x -> case x of
    Nothing ->
        traceShow ("instantiation of unknown module", name, modName) $ return M.empty
    Just mi -> evalInst name (miModId mi) (moduleSig $ miPorts mi)
evalDef d@(DMemory name ty depth rds wrs rdwrs) = do
    traceShowM ("monolithic DMemory NYI", name)
    return M.empty
evalDef d@(DNode name expr) = do
    bindSplitDef name d =<< exprTy expr
    evalStmt $ SConnect mempty (ERef name TUnknown) expr
evalDef d@(DCMem name ty depth _isSync) = do
    traceShowM ("handle mem def", name, ty, depth)
    _esMems %= M.insert name (MemoryDef ty depth S.empty)
    return M.empty
evalDef d@(DCMemPort name ty memName args dir)
  | [addr, _clk] <- args = evalMemPort name memName addr
  | otherwise = do
    traceShowM ("bad arg count for CMemPort", name, memName, args)
    return M.empty

-- Compute the set of net connections made by a `Stmt`.  (A statement can also
-- disconnect nets, represented as connecting the net to `Nothing`.)  Statement
-- evaluation can also have side effects on the extraction state, such as
-- producing new logic nodes or adding variables to the current scope.
evalStmt :: Stmt -> ExtractM ConnTree
evalStmt (SDef _ d) = evalDef d
evalStmt (SCond _ cond then_ else_) = do
    c <- evalRvalue cond
    t <- evalStmt then_
    e <- evalStmt else_
    return $ connTreeJoin c t e
evalStmt (SBlock stmts) =
    -- `M.union` is left-biased, so we `flip` it to make new connections
    -- override the old ones.
    foldM (\acc s -> connTreeAppend acc <$> evalStmt s) M.empty stmts
evalStmt (SPartialConnect _ lhs rhs) = unrollAssign lhs rhs >>= assignTree
evalStmt (SConnect _ lhs rhs) = unrollAssign lhs rhs >>= assignTree
evalStmt (SIsInvalid _ e) = unrollInvalidate e >>= assignTree
evalStmt (SAttach src es) = traceShow ("evalStmt SAttach NYI", src, es) $ return M.empty
evalStmt (SStop _ _ _ _) = return M.empty
evalStmt (SPrint _ _ _ _ _) = return M.empty
evalStmt SEmpty = return M.empty

ensureNode :: NodeId -> ConnTree -> ExtractM ConnTree
ensureNode n ct = connTreeEnsureNode n <$> nodeTy n <*> pure ct

assignTree :: [(Ty, ConnExpr, Rvalue)] -> ExtractM ConnTree
assignTree assigns = foldM (\acc (_ty, l, r) -> do
    acc' <- ensureNode (connExprNode l) acc
    return $ connTreeInsertRvalue l r acc') M.empty assigns

nodeTy :: NodeId -> ExtractM Ty
nodeTy i = use (_esNodeTy . at i) >>= \x -> case x of
    Nothing -> traceShow ("no type for node", i) $ return TUnknown
    Just x -> return x

varNode :: Text -> ExtractM ResolvedNode
varNode name = use (_esLocalScope . at name) >>= \x -> case x of
    Nothing -> traceShow ("failed to resolve var", name) $ return $ RnSingle $ "<" <> name <> ">"
    Just i -> return i

varTy :: Text -> ExtractM Ty
varTy name = varNode name >>= \rn -> case rn of
    RnSingle i -> nodeTy i
    -- Split nodes should have the same type for both left and right halves.
    RnSplit l r -> nodeTy l

exprTy :: Expr -> ExtractM Ty
exprTy (ELit (LUInt _ w)) = return $ TUInt w
exprTy (ELit (LSInt _ w)) = return $ TSInt w
exprTy (ELit (LFixed _ w p)) = return $ TFixed w p
exprTy (ERef name _) = varTy name
exprTy (EField e name _) = bundleFieldTy name <$> exprTy e
exprTy (EIndex e _ _) = vectorItemTy <$> exprTy e
exprTy (EIndexC e _ _) = vectorItemTy <$> exprTy e
exprTy (EMux _ t _ _) = exprTy t
exprTy (EValidIf _ t _) = exprTy t
-- TODO: EPrim ty is probably TUnknown in most case, just like the others
exprTy (EPrim _ _ _ ty) = return ty

exprTyFlip :: Expr -> ExtractM (Ty, Bool)
exprTyFlip (ELit (LUInt _ w)) = return $ (TUInt w, False)
exprTyFlip (ELit (LSInt _ w)) = return $ (TSInt w, False)
exprTyFlip (ELit (LFixed _ w p)) = return $ (TFixed w p, False)
exprTyFlip (ERef name _) = varTy name >>= \ty -> return (ty, False)
exprTyFlip (EField e name _) = bundleFieldTyFlip name <$> exprTyFlip e
exprTyFlip (EIndex e _ _) = vectorItemTyFlip <$> exprTyFlip e
exprTyFlip (EIndexC e _ _) = vectorItemTyFlip <$> exprTyFlip e
exprTyFlip (EMux _ t _ _) = exprTyFlip t
exprTyFlip (EValidIf _ t _) = exprTyFlip t
-- TODO: EPrim ty is probably TUnknown in most case, just like the others
exprTyFlip (EPrim _ _ _ ty) = return (ty, False)

projectConnExpr p (ConnExpr n ps) = ConnExpr n (ps |> p)

projectConnExpr' ps' (ConnExpr n ps) = ConnExpr n (ps <> ps')

projectRvalue :: Projection -> Rvalue -> Rvalue
projectRvalue p rv = go rv
  where
    go (RvExpr (ConnExpr n ps)) = RvExpr $ ConnExpr n (ps |> p)
    go (RvMux c t e) = RvMux c (go t) (go e)
    go (RvIndexMux ie ic t e) = RvIndexMux ie ic (go t) (go e)
    -- TODO: this can happen when taking a field of a vector element at a
    -- non-constant index: `v[idx].f`.  (Though it's probably better to make
    -- EIndex produce something other than RvComb, instead of changing this
    -- code - all other uses of RvComb produce only ground values.)
    go (RvComb op args) =
        traceShow ("projectRvalue: RvComb NYI", op, args, p) RvInvalid
    go RvInvalid = RvInvalid
    go RvUnset = RvUnset

projectTy :: Projection -> Ty -> Ty
projectTy (PrIndex idx) (TVector ty len)
  | 0 <= idx && idx < len = ty
projectTy (PrField name) (TBundle fs)
  | Just f <- find (\f -> fieldName f == name) fs = fieldTy f
projectTy p t = traceShow ("bad projection (Ty)", p, t) TUnknown

projectTyFlip :: Projection -> Ty -> (Ty, Bool)
projectTyFlip (PrIndex idx) (TVector ty len)
  | 0 <= idx && idx < len = (ty, False)
projectTyFlip (PrField name) (TBundle fs)
  | Just f <- find (\f -> fieldName f == name) fs = (fieldTy f, fieldFlip f)
projectTyFlip p t = traceShow ("bad projection (Ty)", p, t) (TUnknown, False)

projectTyFlip' :: Seq Projection -> Ty -> (Ty, Bool)
projectTyFlip' ps t = case S.viewl ps of
    S.EmptyL -> (t, False)
    p S.:< ps ->
        let (t1, f1) = projectTyFlip p t in
        let (t2, f2) = projectTyFlip' ps t1 in
        (t2, f1 /= f2)

bundleFieldTy :: Text -> Ty -> Ty
bundleFieldTy name (TBundle fs)
  | Just f <- find (\f -> fieldName f == name) fs = fieldTy f
bundleFieldTy name ty = traceShow ("bad projection: field not found", name, ty) TUnknown

bundleFieldTyFlip :: Text -> (Ty, Bool) -> (Ty, Bool)
bundleFieldTyFlip name (TBundle fs, flipped)
  | Just f <- find (\f -> fieldName f == name) fs
  = (fieldTy f, if fieldFlip f then not flipped else flipped)
bundleFieldTyFlip name (ty, flipped) =
    traceShow ("bad projection: field not found", name, ty) (TUnknown, flipped)

vectorItemTy :: Ty -> Ty
vectorItemTy (TVector ty _) = ty
vectorItemTy ty = traceShow ("bad projection: not a vector", ty) TUnknown

vectorItemTyFlip :: (Ty, Bool) -> (Ty, Bool)
vectorItemTyFlip (TVector ty _, flipped) = (ty, flipped)
vectorItemTyFlip (ty, flipped) =
    traceShow ("bad projection: not a vector", ty) (TUnknown, flipped)

vectorLen :: Ty -> Int
vectorLen (TVector _ len) = len
vectorLen ty = traceShow ("can't get len of non-vector", ty) 0

evalRvalue :: Expr -> ExtractM Rvalue
evalRvalue e = evalRvalue' False e

evalRvalue' :: Bool -> Expr -> ExtractM Rvalue
evalRvalue' flipSplit e = go e
  where
    go :: Expr -> ExtractM Rvalue
    go (ELit _) = return $ RvComb "lit" []
    go (ERef name _) = do
        i <- nodeIdForSide (not flipSplit) <$> varNode name
        return $ RvExpr $ ConnExpr i S.empty
    go (EField e name _) = projectRvalue (PrField name) <$> go e
    -- TODO: gen combinational logic nodes for EIndex
    go (EIndex e idx _) = do
        e' <- go e
        idx' <- go idx
        return $ RvComb "idx" [e', idx']
    go (EIndexC e idx _) = projectRvalue (PrIndex idx) <$> go e
    go (EMux cond then_ else_ _) =
        RvMux <$> go cond <*> go then_ <*> go else_
    go (EValidIf cond then_ _) =
        RvMux <$> go cond <*> go then_ <*> pure RvInvalid
    -- TODO: gen combinational logic nodes for EPrim
    go (EPrim op args _ ty) = RvComb op <$> mapM go args


-- Expand a (possibly) aggregate-typed assignment into a list of ground-typed
-- assignments.  This is the part where we handle flipped connections inside of
-- bundles.
--
-- The `Bool` in each result indicates whether this part of the assignment was
-- flipped.  For two-sided nodes (wires and regs), this is used to determine
-- whether each expr was on the right or the left of the original assignment.
unrollAggregates :: Ty -> Expr -> Ty -> Expr -> [(Expr, Expr, Bool)]
unrollAggregates lty l rty r = go lty l rty r False
  where
    go (TBundle fs) l (TBundle fs') r flipped = do
        let fsMap' = M.fromList [(fieldName f, f) | f <- fs']
        f <- fs
        Just f' <- return $ M.lookup (fieldName f) fsMap'
        let name = fieldName f
        let ty = fieldTy f
        let ty' = fieldTy f'
        if not $ fieldFlip f then
            go ty (EField l name ty) ty' (EField r name ty) flipped
        else
            go ty (EField r name ty) ty' (EField l name ty) (not flipped)
    go (TVector ty len) l (TVector ty' len') r flipped = do
        idx <- [0 .. min len len' - 1]
        go ty (EIndexC l idx ty) ty' (EIndexC r idx ty) flipped
    go lty l rty r flipped | isGroundTy lty, isGroundTy rty = return (l, r, flipped)
    go lty _ rty _ _ =
        traceShow ("go: type mismatch", lty, rty) []

-- Expand a single aggregate-typed expression.
unrollAggregate :: Ty -> Expr -> Bool -> [Expr]
unrollAggregate ty e flipped = go ty e flipped
  where
    go (TBundle fs) e flipped = do
        f <- fs
        let name = fieldName f
        let ty = fieldTy f
        let flipped' = if fieldFlip f then not flipped else flipped
        go ty (EField e name ty) flipped'
    go (TVector ty len) e flipped = do
        idx <- [0 .. len - 1]
        go ty (EIndexC e idx ty) flipped
    go _ e False = return e
    go _ e True = []

-- Convert an lvalue `Expr` to a `ConnExpr`.  This can produce multiple
-- `ConnExpr`s in some cases, and also sometimes generates a transformation to
-- apply to the `Rvalue` of the assignment.
evalLvalue :: Bool -> Expr -> ExtractM (Ty, [(ConnExpr, Rvalue -> Rvalue)])
evalLvalue flipSplit e = go e
  where
    go (ERef name _) = do
        i <- nodeIdForSide flipSplit <$> varNode name
        ty <- nodeTy i
        return (ty, [(ConnExpr i S.empty, id)])
    go (EField e name _) = do
        (ty, lvs) <- go e
        let ty' = bundleFieldTy name ty
        let lvs' = [(projectConnExpr (PrField name) ce, f) | (ce, f) <- lvs]
        return (ty', lvs')
    go (EIndex e idx _) = do
        (ty, lvs) <- go e
        idxRv <- evalRvalue idx
        let ty' = vectorItemTy ty
        let lvs' = do
                (ce, f) <- lvs
                i <- [0 .. vectorLen ty - 1]
                return (projectConnExpr (PrIndex i) ce,
                    \rv -> RvIndexMux idxRv i (f rv) RvUnset)
        return (ty', lvs')
    go (EIndexC e idx _) = do
        (ty, lvs) <- go e
        let ty' = vectorItemTy ty
        let lvs' = [(projectConnExpr (PrIndex idx) ce, f) | (ce, f) <- lvs]
        return (ty', lvs')
    -- c ? t : e <= r  -->  when c: t <= r; else: e <= r
    go (EMux c t e _) = do
        (ty1, lvs1) <- go t
        (ty2, lvs2) <- go e
        condRv <- evalRvalue c
        let lvs' = [(ce, \rv -> RvMux condRv (f rv) RvUnset) | (ce, f) <- lvs1]
                ++ [(ce, \rv -> RvMux condRv RvUnset (f rv)) | (ce, f) <- lvs2]
        return (ty1, lvs')
    -- c ? t : undef <= r  -->  when c: t <= r
    go (EValidIf c t _) = do
        (ty, lvs) <- go t
        condRv <- evalRvalue c
        let lvs' = [(ce, \rv -> RvMux condRv (f rv) RvUnset) | (ce, f) <- lvs]
        return (ty, lvs')
    go e@(ELit _) = traceShow ("invalid lvalue", e) $ return (TUnknown, [])
    go e@(EPrim _ _ _ _) = traceShow ("invalid lvalue", e) $ return (TUnknown, [])

unrollAssign :: Expr -> Expr -> ExtractM [(Ty, ConnExpr, Rvalue)]
unrollAssign l r = do
    (lty, lFlipped) <- exprTyFlip l
    (rty, rFlipped) <- exprTyFlip r
    let assigns = unrollAggregates lty l rty r
    liftM concat $ forM assigns $ \(l', r', flipped) -> do
        traceShowM ("unrolled assign", l', r')
        let (lFlipped', rFlipped') =
                if flipped then (rFlipped, lFlipped) else (lFlipped, rFlipped)
        r'' <- evalRvalue' (flipped /= rFlipped') r'
        (ty, lfs'') <- evalLvalue (flipped /= lFlipped') l'
        traceShowM ("directions", l, r, lFlipped, rFlipped, flipped)
        return [(ty, l'', f r'') | (l'', f) <- lfs'']

unrollInvalidate :: Expr -> ExtractM [(Ty, ConnExpr, Rvalue)]
unrollInvalidate l = do
    assignsL <- unrollAggregate <$> exprTy l <*> pure l <*> pure False
    ls <- liftM concat $ forM assignsL $ \(l') -> do
        (ty, lfs'') <- evalLvalue False l'
        return [(ty, l'', f RvInvalid) | (l'', f) <- lfs'']

    assignsR <- unrollAggregate <$> exprTy l <*> pure l <*> pure True
    rs <- liftM concat $ forM assignsR $ \(l') -> do
        (ty, lfs'') <- evalLvalue True l'
        return [(ty, l'', f RvInvalid) | (l'', f) <- lfs'']

    return $ ls ++ rs



-- List all leaf fields of `ty`.  For each field, returns the projections to
-- reach that field, the type of the field, and a flag indicating whether the
-- field is flipped or not.  For each entry `(ps, ty', flipped)`, it should hold
-- that `projectTyFlip ps ty = (ty', flipped)`.
listLeafFields :: Ty -> [(Seq Projection, Ty, Bool)]
listLeafFields ty = go S.empty False ty
  where
    go ps flipped (TBundle fs) = do
        f <- fs
        go (ps |> PrField (fieldName f)) (flipped /= fieldFlip f) (fieldTy f)
    go ps flipped (TVector ty len) = do
        idx <- [0 .. len - 1]
        go (ps |> PrIndex idx) flipped ty
    go ps flipped ty = [(ps, ty, flipped)]

-- Handle an external module port.
--
--  1. Generate a `DWire` for the entire port.
--  2. Unroll the port type, and for each leaf field:
--      a. Generate a new `NodeId` (with no associated def, but with a name and
--         `Ty`).
--      b. Connect the node to the appropriate subelement of the `DWire`.
--      c. Generate a new `A.NetId`, and add an `esNetMap` entry to map the
--         leaf node to the new net.
--      d. Run `act`, which usually will connect the new net to something in
--          `esCurModule`.
--
-- The leaf ports have `NodeId`s, so they can appear in the `ConnTree`, but
-- they have no `Def` (so they don't get handled by `buildNodes`) and no entry
-- in `esLocalScope` (so they don't affect name resolution).  The `DWire`
-- representing the port as a whole works exactly like a normal `Def` (except
-- for the implicit connections to the leaf ports), so it introduces no special
-- cases for evaluation, and `buildNodes` will apply the normal splitting and
-- repacking as it does for all `DWire`s.
--
-- This function always traverses the fields of `portTy p` in order, so results
-- should be consistent between different runs.
evalExtPort :: Port -> ExtractM ConnTree
evalExtPort p = do
    -- DWire node
    ct <- evalDef $ DWire (portName p) (portTy p)
    wrn <- varNode $ portName p

    let output = portDir p == Output
    let wi = nodeIdForSide output wrn
    let we = ConnExpr wi S.empty

    connectLeaves ("." <> portName p) (portTy p) output we $ \net f ->
        buildPort net f

-- Handle a module instance.
--
-- Works by generating a `DWire` whose rvalue side is accessible to other
-- expressions and whose lvalue side is connected to the Architecture module
-- instance.
evalInst :: Text -> Int -> Ty -> ExtractM ConnTree
evalInst name modId sig = do
    (leftId, rightId) <- bindSplitDef name (DWire name sig) sig
    _esLocalScope %= M.insert name (RnSingle rightId)

    let inst = A.Inst modId name S.empty
    instId <- buildLogic (A.LkInst inst) [] []

    connectLeaves name sig False (ConnExpr leftId S.empty) $ \net f -> do
        let output = not f
        ty <- use $ _esCurModule . A._moduleNet net . A._netTy
        zoom (_esCurModule . A._moduleLogic instId) $ do
            if not output then
                A._logicInputs %= (|> A.Pin net ty)
            else
                A._logicOutputs %= (|> A.Pin net ty)

-- The "for each leaf field" part of `evalExtPort`.  Also used for instance
-- ports, with slightly different arguments.
--
-- Note that `portDir p` is ignored - only `flipped` is considered.  The
-- default is to connect the leaf node to the wire, which is appropriate for an
-- external input port.
connectLeaves :: Text -> Ty -> Bool -> ConnExpr ->
    (A.NetId -> Bool -> ExtractM ()) -> ExtractM ConnTree
connectLeaves prefix ty flipped wireExpr act =
    liftM connTreeConcat $ forM (listLeafFields ty) $ \(ps, ty, flipped') -> do
        let name = prefix <>
                (if not $ S.null ps then "." <> pathName ps else "")
        let f = flipped /= flipped'

        -- Architecture net + port; leaf `NodeId` + `esNetMap` entry
        (pi, net) <- buildArchNode name 10 ty
        act net f
        let pe = ConnExpr pi S.empty

        -- Connection from leaf node to wire
        let we = projectConnExpr' ps wireExpr
        let (l, r) = if not f then (we, pe) else (pe, we)

        ct <- ensureNode (connExprNode wireExpr) M.empty >>= ensureNode pi
        return $ connTreeInsertRvalue l (RvExpr r) ct


-- Build an "arch node", used to bridge the `ConnTree` with `Architecture`
-- nets.  The dummy node has a `NodeId`, so it can appear in the `ConnTree`,
-- but it also has a `NetId` directly assigned to it, which can be used for
-- special connection types, such as connecting to module ports.  The new node
-- does not appear in `esLocalScope`, and does not have an associated `Def`.
buildArchNode :: Text -> Int -> Ty -> ExtractM (NodeId, A.NetId)
buildArchNode name prio ty = do
    net <- buildNet name 10 ty
    pi <- addNode name ty
    _esNetMap %= M.insert (ConnExpr pi S.empty) net
    return (pi, net)

-- Handle a memory port.  This adds a new entry to `esMems . mdPorts`
--
--  1. Build a new arch node.  Connect the address argument to it.  Store its
--     `NetId` as `mpAddrNet`.
--  2. Build new wires for the read and write ports.  Connect the "inner" sides
--     of the wires to new arch nodes (`mpReadNet` + `mpWriteNet`).
--  3. Combine the "outer" sides of the wires into a single split node, and add
--     it to `esLocalScope` under the port's name.
--
-- We always generate both read and write sides, regardless of the port's
-- declared direction.  The final step of building the `LkRam` element checks
-- which sides are connected for each port.
evalMemPort :: Text -> Text -> Expr -> ExtractM ConnTree
evalMemPort name memName addrExpr = do
    addrTy <- exprTy addrExpr
    addrRvalue <- evalRvalue addrExpr
    (addrNode, addrNet) <- buildArchNode (name <> ".addr") 10 addrTy

    ty <- preuse (_esMems . ix memName . _mdTy) >>= \x -> case x of
        Just x -> return x
        Nothing -> traceShow ("failed to resolve memory for port", memName, name) $
            return TUnknown
    let readName = name <> ".read"
    let writeName = name <> ".write"
    (readLeft, readRight) <- bindSplitDef readName (DWire readName ty) ty
    (writeLeft, writeRight) <- bindSplitDef writeName (DWire writeName ty) ty
    _esLocalScope %= M.insert name (RnSplit writeLeft readRight)

    (readNode, readNet) <- buildArchNode readName 10 ty
    (writeNode, writeNet) <- buildArchNode writeName 10 ty

    _esMems . ix memName . _mdPorts %=
        (|> MemPort addrNet readNet writeNet readRight writeLeft)

    let mkLv i = ConnExpr i S.empty
    let mkRv i = RvExpr $ mkLv i

    return $
        connTreeInsertRvalue (mkLv readLeft) (mkRv readNode) $
        connTreeInsertRvalue (mkLv writeNode) (mkRv writeRight) $
        connTreeInsertNode readLeft ty $
        --connTreeInsertNode readRight ty $
        --connTreeInsertNode readNode ty $
        --connTreeInsertNode writeLeft ty $
        --connTreeInsertNode writeRight ty $
        connTreeInsertNode writeNode ty $
        connTreeInsertRvalue (mkLv addrNode) addrRvalue $
        connTreeInsertNode addrNode addrTy $
        M.empty



moduleSig :: [Port] -> Ty
moduleSig ps = TBundle $ map (\p ->
    Field (portName p) (portTy p) (portDir p == Input)) ps


muxNets :: A.NetId -> A.NetId -> A.NetId -> ExtractM A.NetId
muxNets c t e = do
    outNet <- buildNet "mux_out" 0 TUnknown
    buildLogic (A.LkMux ("val" <| S.empty) 2) [c, t, e] [outNet]
    return outNet

muxNetsOpt :: A.NetId -> Maybe A.NetId -> Maybe A.NetId -> ExtractM A.NetId
muxNetsOpt c t e = do
    t' <- maybe (buildDummyNet "<undef>") return t
    e' <- maybe (buildDummyNet "<undef>") return e
    muxNets c t' e'




newtype MultiMap a b = MultiMap { getMultiMap :: Map a (Set b) }

instance (Ord a, Ord b) => Semigroup (MultiMap a b) where
    MultiMap a <> MultiMap b = MultiMap $ M.unionWith Set.union a b

instance (Ord a, Ord b) => Monoid (MultiMap a b) where
    mempty = MultiMap M.empty

multiMapInsert :: (Ord k, Ord v) => k -> v -> MultiMap k v -> MultiMap k v
multiMapInsert k v (MultiMap m) = MultiMap $
    M.alter (\x -> Just $ Set.insert v $ fromMaybe Set.empty x) k m

multiMapSingleton :: (Ord k, Ord v) => k -> v -> MultiMap k v
multiMapSingleton k v = MultiMap $ M.singleton k (Set.singleton v)

multiMapGet :: (Ord k, Ord v) => k -> MultiMap k v -> Set v
multiMapGet k (MultiMap m) = fromMaybe Set.empty $ M.lookup k m


rvalueConnExprs :: Rvalue -> MultiMap NodeId (Seq Projection)
rvalueConnExprs rv = everything (<>) (mempty `mkQ` go) rv
  where
    go (RvExpr (ConnExpr n ps)) = multiMapSingleton n ps
    go _ = mempty

treeConnExprs :: GConnTree a -> MultiMap NodeId (Seq Projection)
treeConnExprs ct = mconcat $ map (\(n, c) -> go n S.empty c) $ M.toList ct
  where
    go n ps (CGround' _ rv _) = multiMapInsert n ps $ rvalueConnExprs rv
    go n ps (CBundle _ m) = mconcat $ map (\(name, c) ->
        go n (ps |> PrField name) c) $ M.toList m
    go n ps (CVector _ _ m) = mconcat $ map (\(idx, c) ->
        go n (ps |> PrIndex idx) c) $ M.toList m


data ProjTrie = PtNode Bool (Map Projection ProjTrie)
    deriving (Show)

instance Semigroup ProjTrie where
    PtNode p1 m1 <> PtNode p2 m2 = PtNode (p1 || p2) (M.unionWith (<>) m1 m2)

instance Monoid ProjTrie where
    mempty = PtNode False M.empty

projTrieInsert :: [Projection] -> ProjTrie -> ProjTrie
projTrieInsert ps t = go ps t
  where
    go [] (PtNode _ m) = PtNode True m
    go (p : ps) (PtNode pres m) = PtNode pres $
        M.alter (\x -> Just $ go ps $ fromMaybe mempty x) p m

projTrieFromList :: [[Projection]] -> ProjTrie
projTrieFromList pss = foldl' (\acc ps -> projTrieInsert ps acc) mempty pss


-- Collect the topmost `present` node from each branch of the `ProjTrie`.  That
-- is, it returns a pair for each node that is present and has no present
-- parents.  Each pair has the path to the node in question and its child map
-- (which may be empty).
gatherPresent :: ProjTrie -> [(Seq Projection, ProjTrie)]
gatherPresent t = go' S.empty t
  where
    go' ps t@(PtNode pres m)
      | pres = [(ps, t)]
      | otherwise = concatMap (\(p', t') -> go' (ps |> p') t') $ M.toList m

-- Like `gatherPresent`, but ignores the top node itself.
gatherPresent' :: ProjTrie -> [(Seq Projection, ProjTrie)]
gatherPresent' (PtNode _ m) = gatherPresent $ PtNode False m

-- Find nodes that are topmost-present in one or both input tries.
zipGatherPresent :: ProjTrie -> ProjTrie -> [(Seq Projection, ProjTrie, ProjTrie)]
zipGatherPresent t = go S.empty t
  where
    go ps t1@(PtNode pres1 m1) t2@(PtNode pres2 m2)
      | pres1 || pres2 = [(ps, t1, t2)]
      | otherwise = concat $ M.elems $ M.merge
        (M.mapMissing $ \p c1 -> map (\(ps', t') -> (ps', t', absent)) $ go' (ps |> p) c1)
        (M.mapMissing $ \p c2 -> map (\(ps', t') -> (ps', absent, t')) $ go' (ps |> p) c2)
        (M.zipWithMatched $ \p c1 c2 -> go (ps |> p) c1 c2)
        m1 m2

    go' ps t@(PtNode pres m)
      | pres = [(ps, t)]
      | otherwise = concatMap (\(p', t') -> go' (ps |> p') t') $ M.toList m

    absent = PtNode False M.empty


projName :: Projection -> Text
projName (PrField name) = name
projName (PrIndex idx) = T.pack $ show idx

pathName :: Seq Projection -> Text
pathName ps = T.intercalate "." $ map projName $ toList ps

nodeName :: NodeId -> ExtractM Text
nodeName n = return n

exprName :: ConnExpr -> ExtractM Text
exprName (ConnExpr n ps)
  | S.null ps = nodeName n
  | otherwise = do
    a <- nodeName n
    let b = pathName ps
    return $ a <> "." <> b

repackPortName :: Text -> Text
repackPortName "" = "(all)"
repackPortName n = n

recordNet :: ConnExpr -> A.NetId -> ExtractM ()
recordNet ce net = _esNetMap %= M.insert ce net

-- Construct a net for the root of the given `ProjTrie`.  Generates additional
-- nets and a `repack` node if the `ProjTrie` has children.
--
-- By default, the generated `repack` takes the bundle's (unflipped) fields as
-- inputs and produces the full bundle as an output (meaning the returned net
-- can be used as an input to another logic element).  If `flipped`, then the
-- sides are reversed.
buildProjTrieNet :: ConnExpr -> Ty -> Bool -> ProjTrie -> ExtractM A.NetId
buildProjTrieNet ce ty flipped t = do
    name <- exprName ce
    i <- buildNet name 10 ty
    recordNet ce i
    -- _2 is True for outputs and False for inputs.
    flds <- forM (gatherPresent' t) $ \(ps, t') -> do
        let (ty', flipped') = projectTyFlip' ps ty
        let ce' = projectConnExpr' ps ce
        -- If the field and the top level are both unflipped, this field
        -- appears as an input (False)
        let f = flipped /= flipped'
        i' <- buildProjTrieNet ce' ty' f t'
        return (i', f, pathName ps)
    when (not $ null flds) $ do
        -- If the top level is unflipped, the `bundle` field appears as an
        -- output (True)
        buildWireRepack name $ (i, not flipped, "(all)") : flds
    return i

buildWireRepack :: Text -> [(A.NetId, Bool, Text)] -> ExtractM ()
buildWireRepack name flds = void $ do
    let (outs, ins) = partition (^. _2) flds
    let lk = A.LkRepack (Just name)
            (S.fromList $ map (repackPortName . (^. _3)) ins)
            (S.fromList $ map (repackPortName . (^. _3)) outs)
    buildLogic lk (map (^. _1) ins) (map (^. _1) outs)

{-
    if shortcutWire ins outs then do
        let inNet = head ins ^. _1
        let outNet = head outs ^. _1
        buildNetAlias inNet outNet
-}

buildNodes :: ConnTree -> ExtractM ()
buildNodes ct = do
    defs <- use _esDefs
    forM_ (toList defs) $ \(def, rn) -> case def of
        DWire _ _ -> goWire rn
        DReg _ _ _ _ _ -> goReg rn
        DNode _ _ -> goWire rn
        _ -> return ()

    mems <- use _esMems
    forM_ (M.toList mems) $ \(name, mem) -> goMem name mem

  where
    allExprs = treeConnExprs ct

    goWire (RnSingle i) = traceShowM ("don't know how to handle RnSingle DWire", i)
    goWire (RnSplit l r) = do
        let lpt = projTrieFromList $ map toList $ Set.toList $ multiMapGet l allExprs
        let rpt = projTrieFromList $ map toList $ Set.toList $ multiMapGet r allExprs
        ty <- nodeTy l

        -- This `zipGatherPresent` splits the wire apart into as many
        -- disjoint pieces as possible.  For example, if neither the L nor
        -- the R side uses the whole bundle, this will split the bundle
        -- into (at least) one piece per bundle field.  The goal is to
        -- avoid forcing unrelated connections through a shared `repack`
        -- node in the output graph.
        forM_ (zipGatherPresent lpt rpt) $ \(ps, lt, rt) -> do
            let (ty', flipped) = projectTyFlip' ps ty

            if shortcutWire lt rt then do
                lnet <- buildProjTrieNet (ConnExpr l ps) ty' flipped lt
                rnet <- buildProjTrieNet (ConnExpr r ps) ty' (not flipped) rt
                void $ if not flipped then
                    buildNetAlias lnet rnet
                else
                    buildNetAlias rnet lnet
            else do
                -- Topmost present nodes of `lt`/`rt` (which may be `lt`/`rt`
                -- itself) become fields of the main repack node for this wire.
                let present =
                        map (\(ps, t) -> (l, ps, t, False)) (gatherPresent lt)
                        <> map (\(ps, t) -> (r, ps, t, True)) (gatherPresent rt)

                flds <- forM present $ \(n', ps', t', flipped') -> do
                    let (ty'', flipped'') = projectTyFlip' ps' ty'
                    let f = flipped /= (flipped' /= flipped'')
                    i <- buildProjTrieNet (ConnExpr n' (ps <> ps')) ty'' f t'
                    return (i, f, pathName ps')

                name <- exprName $ ConnExpr l ps
                buildWireRepack name flds

    shortcutWire (PtNode True _) (PtNode True _) = True
    shortcutWire _ _ = False

    goReg (RnSingle i) = traceShowM ("don't know how to handle RnSingle DReg", i)
    goReg (RnSplit l r) = do
        let lpt = projTrieFromList $ map toList $ Set.toList $ multiMapGet l allExprs
        let rpt = projTrieFromList $ map toList $ Set.toList $ multiMapGet r allExprs
        ty <- nodeTy l

        forM_ (zipGatherPresent lpt rpt) $ \(ps, lt, rt) -> do
            let (ty', flipped) = projectTyFlip' ps ty

            lnet <- buildProjTrieNet (ConnExpr l ps) ty' flipped lt
            rnet <- buildProjTrieNet (ConnExpr r ps) ty' (not flipped) rt
            let (inNet, outNet) = if not flipped then (lnet, rnet) else (rnet, lnet)
            name <- exprName $ ConnExpr l ps
            buildLogic (A.LkDFlipFlop name 0) [inNet] [outNet]

    goMem name md = do
        (rdAddrs, rdDatas) <- liftM mconcat $ forM (toList $ mdPorts md) $ \port -> do
            let rds = multiMapGet (mpReadNode port) allExprs
            if Set.null rds then
                return ([], [])
            else
                return ([mpAddrNet port], [mpReadNet port])

        wrNets <- liftM mconcat $ forM (toList $ mdPorts md) $ \port -> do
            let wrs = multiMapGet (mpWriteNode port) allExprs
            if Set.null wrs then
                return []
            else do
                constNet <- buildDummyNet "<const>"
                return [mpAddrNet port, mpWriteNet port, constNet]

        let lk = A.LkRam name (A.EIntLit A.dummySpan $ mdDepth md) 0
                (length rdAddrs) (length wrNets `div` 3)
        ramNet <- buildDummyNet "<ram>"
        ramNet' <- buildDummyNet "<ram>"
        clkNet <- buildDummyNet "<clk>"
        buildLogic lk (ramNet : clkNet : rdAddrs ++ wrNets) (ramNet' : rdDatas)



connTreeToList' :: GConnTree a -> [(ConnExpr, Ty, Rvalue, a)]
connTreeToList' ct = mconcat $ map (\(n, c) -> go n S.empty c) $ M.toList ct
  where
    go n ps (CGround' ty rv ann) = [(ConnExpr n ps, ty, rv, ann)]
    go n ps (CBundle _ m) = mconcat $ map (\(name, c) ->
        go n (ps |> PrField name) c) $ M.toList m
    go n ps (CVector _ _ m) = mconcat $ map (\(idx, c) ->
        go n (ps |> PrIndex idx) c) $ M.toList m

connTreeToList :: ConnTree -> [(ConnExpr, Ty, Rvalue)]
connTreeToList ct = map (\(ce, ty, rv, ann) -> (ce, ty, rv)) $ connTreeToList' ct

makeConnection :: A.NetId -> Ty -> Rvalue -> ExtractM ()
makeConnection dest ty (RvExpr ce) = do
    src <- use $ _esNetMap . at ce
    case src of
        Just src -> do
            traceShowM ("connect", src, dest)
            void $ buildNetAlias src dest
        Nothing -> traceShowM ("failed to resolve net for rvalue", ce)
makeConnection dest ty (RvMux c t e) = do
    cNet <- buildNet "<mux-cond>" 0 (TUInt $ WInt 1)
    tNet <- buildNet "<mux-then>" 0 ty
    eNet <- buildNet "<mux-else>" 0 ty
    makeConnection cNet ty c
    makeConnection tNet ty t
    makeConnection eNet ty e
    void $ buildLogic (A.LkMux ("val" <| S.empty) 2) [cNet, tNet, eNet] [dest]
makeConnection dest ty (RvIndexMux ie _ic t e) = do
    let idxTy = TUInt $ WInt 1  -- TODO: compute actual type of ie
    ieNet <- buildNet "<imux-idx>" 0 idxTy
    tNet <- buildNet "<imux-then>" 0 ty
    eNet <- buildNet "<imux-else>" 0 ty
    makeConnection ieNet ty ie
    makeConnection tNet ty t
    makeConnection eNet ty e
    void $ buildLogic (A.LkMux ("val" <| S.empty) 2) [ieNet, tNet, eNet] [dest]
makeConnection dest ty (RvComb _op args) = do
    -- TODO: compute actual types of args
    argNets <- forM (zip [0 ..] args) $ \(i, arg) -> do
        n <- buildNet ("<comb-" <> T.pack (show i) <> ">") 0 TUnknown
        makeConnection n TUnknown arg
        return n
    void $ buildLogic (A.LkExpr) argNets [dest]
-- For `RvInvalid`, leave the destination net unconnected.  The destination net
-- should then disappear from the visualization when `draw-onesided-nets` is
-- false (the default).
makeConnection _dest _ty RvInvalid = return ()
makeConnection dest ty RvUnset = traceShowM ("unexpected RvUnset", dest, ty)

-- Generate a NetAlias for each connection described in the `ConnTree`.
makeConnections :: ConnTree -> ExtractM ()
makeConnections ct = forM_ (connTreeToList ct) $ \(ce, ty, rv) -> do
    net <- use $ _esNetMap . at ce
    case net of
        Just net -> makeConnection net ty rv
        Nothing -> traceShowM ("failed to resolve net for lvalue", ce)
