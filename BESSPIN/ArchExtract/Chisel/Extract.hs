{-# LANGUAGE OverloadedStrings, TemplateHaskell, PatternSynonyms, DeriveFunctor #-}
module BESSPIN.ArchExtract.Chisel.Extract where

import Control.Monad
import Control.Monad.State
import Data.Foldable
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
    deriving (Show, Eq, Ord)

data ConnExpr = ConnExpr NodeId (Seq Projection)
    deriving (Show, Eq, Ord)

data Rvalue =
      RvExpr ConnExpr
    | RvMux Rvalue Rvalue Rvalue
    -- Specialized `RvMux` used for assignment to a non-constant index:
    -- `array[idx_expr] <= value`.  Such statements unroll into many copies of
    -- `when idx_expr == <i>: array[<i>] <= value`.  The unrolled form turns
    -- into an assignment `array[<i>] <= RvIndexMux idx_expr <i> value <prev>`.
    | RvIndexMux Rvalue Int Rvalue Rvalue
    -- Initial value of every node.  Also, the assignment RHS for `x is
    -- invalid` statements.
    | RvInvalid
    -- Marker indicating that the value is unset / unconnected.  This often
    -- appears on the `else` side of `RvMux`, to handle `when` with no `else`.
    -- When unioning `ConnTree`s, `RvUnset` will be replaced with the value
    -- from the older tree.
    | RvUnset
    deriving (Show)

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

mapConn :: (Ty -> Rvalue -> Rvalue) -> GConnection a -> GConnection a
mapConn f c = mapConn' (\ty rv ann -> (f ty rv, ann)) c

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

rerollConnChildren :: BiConnection -> BiConnection
rerollConnChildren c@(CGround' _ _ _) = c
rerollConnChildren (CBundle fs m) = CBundle fs $ rerollConn <$> m
rerollConnChildren (CVector ty len m) = CVector ty len $ rerollConn <$> m

rerollConn :: BiConnection -> BiConnection
rerollConn c =
    let c' = rerollConnChildren c in
    case describeChildren c' of
        Just (CdConn ce dir) -> CGround' (connTy c) (RvExpr ce) dir
        Just CdInvalid -> CGround' (connTy c) RvInvalid Fwd
        Nothing -> c'

rerollConnTree :: BiConnTree -> BiConnTree
rerollConnTree ct = rerollConn <$> ct



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



data Interface =
    -- The `Int` is the index of the port among the module's inputs/outputs.
      IfPort Direction Int Ty
    | IfBundle (Seq (Text, Interface)) (Map Text Interface)
    deriving (Show)

data Value =
      VNet A.NetId
    -- TODO: should be `VVector Value`, to handle vectors of vectors, vectors
    -- of bundles, etc.
    | VVector A.NetId
    -- `Bool` is the "flip" flag for the field.
    | VBundle (Map Text (Value, Bool))
    -- Acts like `.0` when used on the RHS and `.1` when used on the LHS of a
    -- connection statement.
    --
    -- Note that `VDff` should never appear inside a `VBundle`.
    | VDff Value Value
    -- Index of the `Logic` node.  Also stores the element type, which often is
    -- not provided at port definitions.
    | VRam Int Ty
    | VError
    deriving (Show)

data ModInfo = ModInfo
    { miName :: Text
    , miPorts :: [Port]
    , miIfc :: Interface
    , miModId :: A.ModId
    }

data ExtractState = ExtractState
    { esCurModule :: A.Module ()
    , esModMap :: Map Text ModInfo
    , esLocalScope :: Map Text Value
    , esDefTy :: Map Text Ty
    , esSplitDefs :: Set Text
    }

makeLenses' ''ModInfo
makeLenses' ''ExtractState

type ExtractM a = State ExtractState a

withModule :: A.Module () -> ExtractM a -> ExtractM (a, A.Module ())
withModule initMod act = do
    oldMod <- _esCurModule <<.= initMod
    r <- act
    newMod <- _esCurModule <<.= oldMod
    return (r, newMod)

withScope :: ExtractM a -> ExtractM a
withScope act = do
    oldScope <- _esLocalScope <<.= M.empty
    r <- act
    _esLocalScope .= oldScope
    return r

bindLocal :: Text -> Value -> ExtractM ()
bindLocal name val = _esLocalScope %= M.insert name val


extractDesign :: Config.Chisel -> Circuit -> A.Design ()
extractDesign cfg circ = evalState (extractDesign' cfg circ) initState
  where
    firMods = circuitModules circ
    modMap = M.fromList $ zipWith mkModInfo firMods [0..]

    mkModInfo m i = (name, ModInfo name ports ifc i)
      where
        name = moduleName m
        ports = modulePorts m
        ifc = convertInterface $ modulePorts m

    initState = ExtractState
        { esCurModule = error $ "current module is unset"
        , esModMap = modMap
        , esLocalScope = M.empty
        , esDefTy = M.empty
        , esSplitDefs = Set.empty
        }

extractDesign' :: Config.Chisel -> Circuit -> ExtractM (A.Design ())
extractDesign' cfg circ = do
    mods <- S.fromList <$> mapM (extractModule cfg) (circuitModules circ)
    return $ A.Design mods

extractModule :: Config.Chisel -> Module -> ExtractM (A.Module ())
extractModule cfg m = do
    ((), m') <- withModule initMod $ do
        traceM (T.unpack $ " --- begin extracting " <> moduleName m <> " ---")
        let ifc = convertInterface $ modulePorts m

        (v, ins, outs) <- buildIfcNets "" ifc (moduleSig $ modulePorts m)
        ins' <- mapM buildPort ins
        outs' <- mapM buildPort outs
        _esCurModule . A._moduleInputs .= S.fromList ins'
        _esCurModule . A._moduleOutputs .= S.fromList outs'

        forM_ (modulePorts m) $ \p -> do
            bindDef (portName p) (portTy p)

        withScope $ do
            let vars = case v of
                    VBundle fs -> M.toList fs
                    _ -> traceShow ("impossible: module buildIfcNets produced non-bundle?",
                        moduleName m, v) []

            forM_ vars $ \(name, (val, _)) -> bindLocal name val

            case moduleKind m of
                MkNormal body -> do
                    let bbox = Set.member (moduleName m) (Config.chiselBlackboxModules cfg)
                    when (not bbox) $ do
                        conns <- evalStmt body
                        traceShowM ("connections", conns)
                        makeConnections conns

                        conns' <- evalStmt' body
                        let biconns' = addRevConns $ fmap (fmap $ const Fwd) conns'
                        let biconns'' = rerollConnTree biconns'
                        traceM (T.unpack $ "new-style connections (orig): \n" <>
                            T.unlines (connTreePrint conns'))
                        traceM (T.unpack $ "new-style connections (with rev): \n" <>
                            T.unlines (biConnTreePrint biconns'))
                        traceM (T.unpack $ "new-style connections (rerolled): \n" <>
                            T.unlines (biConnTreePrint biconns''))

                MkExtern _ -> return ()

    return $
        mergeAliasedNets $
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

buildPort :: A.NetId -> ExtractM A.Port
buildPort n = do
    (name, ty) <- zoom (_esCurModule . A._moduleNet n) $ do
        name <- use A._netName
        ty <- use A._netTy
        return (head $ T.lines name, ty)
    return $ A.Port name n ty



flipDir :: Direction -> Direction
flipDir Input = Output
flipDir Output = Input

mkIfBundle :: [(Text, Interface)] -> Interface
mkIfBundle xs = IfBundle (S.fromList xs) (M.fromList xs)

convertInterface :: [Port] -> Interface
convertInterface ps = evalState (mkIfBundle <$> mapM goPort ps) (0, 0)
  where
    goPort :: Port -> State (Int, Int) (Text, Interface)
    goPort (Port _ name dir ty) = go dir ty >>= \ifc -> return (name, ifc)

    goField :: Direction -> Field -> State (Int, Int) (Text, Interface)
    goField dir (Field name ty flip) = go dir' ty >>= \ifc -> return (name, ifc)
      where dir' = if flip then flipDir dir else dir

    go :: Direction -> Ty -> State (Int, Int) Interface
    go dir (TBundle fs) = mkIfBundle <$> mapM (goField dir) fs
    go dir ty = do
        i <- assignId dir
        return $ IfPort dir i ty

    assignId Input = _1 <<%= (+1)
    assignId Output = _2 <<%= (+1)

-- Return all the input and output ports of `ifc`, in order.
interfacePorts :: Interface -> ([([Text], Ty)], [([Text], Ty)])
interfacePorts (IfPort Input _ ty) = ([([], ty)], [])
interfacePorts (IfPort Output _ ty) = ([], [([], ty)])
interfacePorts (IfBundle xs _) =
    mconcat $ map (\(name, ifc) ->
        interfacePorts ifc & each . each . _1 %~ (name :)) $ toList xs


convertTy :: Ty -> A.Ty
convertTy _ = A.TUnknown


asNet :: Value -> ExtractM A.NetId
asNet (VNet n) = return n
asNet (VVector n) = traceShow ("warning: casting vector to net", n) $ return n
asNet (VBundle fs) = traceShow ("error: casting bundle to net", fs) $ buildDummyNet "<bundle>"
asNet (VDff r l) = traceShow ("warning: casting dff to net", r, l) $ asNet r
asNet (VRam idx ty) = traceShow ("error: casting ram to net", idx, ty) $ buildDummyNet "<error>"
-- We already reported whatever error produced this `VError`.
asNet VError = buildDummyNet "<error>"

buildDummyNet :: Text -> ExtractM A.NetId
buildDummyNet name = buildNet name (-10) TUnknown


-- Compute the set of net connections made by a `Stmt`.  (A statement can also
-- disconnect nets, represented as connecting the net to `Nothing`.)  Statement
-- evaluation can also have side effects on the extraction state, such as
-- producing new logic nodes or adding variables to the current scope.
evalStmt :: Stmt -> ExtractM (Map A.NetId (Maybe A.NetId))
evalStmt (SDef _ d) = evalDef d >> return M.empty
evalStmt (SCond _ cond then_ else_) = do
    condVal <- asNet =<< evalExpr cond
    a <- evalStmt then_
    b <- evalStmt else_
    let overlap = M.intersectionWith (\x y -> (x, y)) a b
    overlap' <- traverse (\(x, y) -> Just <$> muxNetsOpt condVal x y) overlap
    return $ overlap' <> a <> b
evalStmt (SBlock stmts) =
    -- `M.union` is left-biased, so we `flip` it to make new connections
    -- override the old ones.
    foldM (\acc s -> flip M.union acc <$> evalStmt s) M.empty stmts
evalStmt (SPartialConnect _ lhs rhs) = do
    lhsVal <- evalExpr lhs
    rhsVal <- evalExpr rhs
    doConnect lhsVal rhsVal
evalStmt (SConnect _ lhs rhs) = do
    lhsVal <- evalExpr lhs
    rhsVal <- evalExpr rhs
    doConnect lhsVal rhsVal
evalStmt (SIsInvalid _ e) = do
    val <- evalExpr e
    doDisconnect val
evalStmt (SAttach src es) = traceShow ("evalStmt SAttach NYI", src, es) $ return M.empty
evalStmt (SStop _ _ _ _) = return M.empty
evalStmt (SPrint _ _ _ _ _) = return M.empty
evalStmt SEmpty = return M.empty

-- Evaluate a definition.  This typically adds new local variables.
evalDef :: Def -> ExtractM ()
evalDef (DWire name ty) = do
    (v, _) <- makeTypedValue name ty
    bindLocal name v
evalDef (DReg name ty _clk _res _init) = do
    inNet <- buildNet (name <> ".D") 15 ty
    outNet <- buildNet (name <> ".Q") 15 ty
    buildLogic (A.LkDFlipFlop name 0) [inNet] [outNet]
    inVal <- packTy (name <> ".D") ty inNet
    outVal <- unpackTy (name <> ".Q") ty outNet
    bindLocal name $ VDff outVal inVal
evalDef (DInst name modName) = use (_esModMap . at modName) >>= \x -> case x of
    Nothing ->
        traceShow ("instantiation of unknown module", name, modName) $ return ()
    Just mi -> do
        (v, ins, outs) <- buildIfcNets name (miIfc mi) (moduleSig $ miPorts mi)
        buildLogic (A.LkInst $ A.Inst (miModId mi) name S.empty) ins outs
        bindLocal name $ v
evalDef (DMemory name ty depth rds wrs rdwrs) = do
    idx <- buildRam name ty depth
    bindLocal name $ VRam idx ty

    forM_ rds $ \rd -> do
        (addrNet, dataNet) <- addRamReadPort idx rd ty
        traceShowM ("NYI: don't know how to connect up address lines of DMemory ports",
                name, rd, addrNet)
        rdVal <- unpackTy (name <> "." <> rd) ty dataNet
        bindLocal rd $ VDff rdVal VError

    forM_ wrs $ \wr -> do
        (addrNet, dataNet) <- addRamWritePort idx wr ty
        traceShowM ("NYI: don't know how to connect up address lines of DMemory ports",
                name, wr, addrNet)
        wrVal <- packTy (name <> "." <> wr) ty dataNet
        bindLocal wr $ VDff VError wrVal

    forM_ rdwrs $ \rdwr -> do
        (rAddrNet, rDataNet) <- addRamReadPort idx (rdwr <> ".rd") ty
        (wAddrNet, wDataNet) <- addRamWritePort idx (rdwr <> ".wr") ty
        traceShowM ("NYI: don't know how to connect up address lines of DMemory ports",
                name, rdwr, rAddrNet, wAddrNet)
        rdVal <- unpackTy (name <> "." <> rdwr) ty rDataNet
        wrVal <- packTy (name <> "." <> rdwr) ty wDataNet
        bindLocal rdwr $ VDff rdVal wrVal
evalDef (DNode name expr) = bindLocal name =<< nameValue name =<< evalExpr expr
evalDef (DCMem name ty depth isSync) = do
    idx <- buildRam name ty depth
    bindLocal name $ VRam idx ty
evalDef (DCMemPort name ty memName args dir)
  | [addr, _clk] <- args = use (_esLocalScope . at memName) >>= \x -> case x of
    Just (VRam idx ty') -> do
        addrNet <- asNet =<< evalExpr addr
        -- TODO: "Infer" ports are usually read-only or write-only, but we
        -- always produce both halves, usually involving big unused struct
        -- pack/unpack nodes.  We should have a postprocessing pass remove
        -- those from the architecture.
        wr <- if dir `elem` [MpdInfer, MpdWrite, MpdReadWrite] then do
                (wAddrNet, wDataNet) <- addRamWritePort idx (name <> ".wr") ty
                buildNetAlias addrNet wAddrNet
                traceShowM ("write port", name, memName, ty, ty')
                packTy (name <> ".wr") ty' wDataNet
            else return VError
        rd <- if dir `elem` [MpdInfer, MpdRead, MpdReadWrite] then do
                (rAddrNet, rDataNet) <- addRamReadPort idx (name <> ".rd") ty
                buildNetAlias addrNet rAddrNet
                unpackTy (name <> ".rd") ty' rDataNet
            else return VError
        bindLocal name $ VDff rd wr
    Just v -> traceShowM ("port references non-memory", memName, name, v)
    Nothing -> traceShowM ("unknown local memory", memName, name)
  | otherwise = traceShowM ("bad arg count for CMemPort", name, memName, args)


buildRam :: Text -> Ty -> Int -> ExtractM Int
buildRam name ty depth = do
    dummyRamIn <- buildDummyNet "<ram?>"
    dummyRamOut <- buildDummyNet "<ram?>"
    dummyClock <- buildDummyNet "<clk?>"
    idx <- buildLogic (A.LkRam name (A.EIntLit (A.Span 0 0) depth) 0 0 0)
        [dummyRamIn, dummyClock] [dummyRamOut]
    bindLocal name $ VRam idx ty
    return idx

-- Add a new read port to the indicated `LkRam` logic node.  Returns the nets
-- connected to the new read-address and read-data ports.
addRamReadPort :: Int -> Text -> Ty -> ExtractM (A.NetId, A.NetId)
addRamReadPort idx name ty = do
    -- TODO: infer address width from RAM depth, like FIRRTL compiler does
    addrNet <- buildNet (name <> ".ra") 10 (TUInt $ WInt 99)
    dataNet <- buildNet (name <> ".rd") 10 ty
    addrPin <- buildPin addrNet
    dataPin <- buildPin dataNet
    zoom (_esCurModule . A._moduleLogic idx) $ do
        numWrites <- zoom A._logicKind $ state $ \lk -> case lk of
            A.LkRam {} ->
                (A.lkRamWritePorts lk,
                    lk { A.lkRamReadPorts = A.lkRamReadPorts lk + 1 })
            _ -> (0, traceShow ("tried to add read port to non-RAM", lk, idx, name) lk)
        A._logicInputs %= \ins -> S.insertAt (S.length ins - 3 * numWrites) addrPin ins
        A._logicOutputs %= (|> dataPin)
    return (addrNet, dataNet)

-- Add a new write port to the indicated `LkRam` logic node.  Returns the nets
-- connected to the new write-address and write-data ports.
addRamWritePort :: Int -> Text -> Ty -> ExtractM (A.NetId, A.NetId)
addRamWritePort idx name ty = do
    -- TODO: infer address width from RAM depth, like FIRRTL compiler does
    addrNet <- buildNet (name <> ".wa") 10 (TUInt $ WInt 99)
    dataNet <- buildNet (name <> ".wd") 10 ty
    enableNet <- buildNet (name <> ".we") 10 (TUInt $ WInt 1)
    addrPin <- buildPin addrNet
    dataPin <- buildPin dataNet
    enablePin <- buildPin enableNet
    zoom (_esCurModule . A._moduleLogic idx) $ do
        A._logicKind %= \lk -> case lk of
            A.LkRam {} -> lk { A.lkRamWritePorts = A.lkRamWritePorts lk + 1 }
            _ -> traceShow ("tried to add write port to non-RAM", lk, idx, name) lk
        A._logicInputs %= \x -> x |> addrPin |> dataPin |> enablePin
    return (addrNet, dataNet)


evalExpr :: Expr -> ExtractM Value
evalExpr (ELit _) = VNet <$> buildDummyNet "<const>"
evalExpr (ERef name ty) = use (_esLocalScope . at name) >>= \x -> case x of
    Nothing -> traceShow ("unknown local variable", name, ty) $ return VError
    Just v -> return v
evalExpr (EField e name ty) = do
    val <- evalExpr e
    return $ valueField val name
  where
    valueField (VBundle fs) name = case M.lookup name fs of
        Just (v, _) -> v
        Nothing -> traceShow ("unknown field", name, fs) VError
    valueField (VDff r l) name = VDff (valueField r name) (valueField l name)
    valueField val name =
        traceShow ("field access of non-bundle", name, val) VError
evalExpr (EIndex e idx ty) = do
    val <- evalExpr e
    idxNet <- asNet =<< evalExpr idx
    case val of
        VVector n -> do
            outNet <- buildNet "<item>" 0 ty
            buildLogic A.LkExpr [n, idxNet] [outNet]
            return $ VNet outNet
        _ -> traceShow ("indexed into non-vector", e, idx) $ return VError
evalExpr (EIndexC e idx ty) = do
    val <- evalExpr e
    case val of
        VVector n -> do
            outNet <- buildNet ("<item " <> T.pack (show idx) <> ">") 0 ty
            buildLogic A.LkExpr [n] [outNet]
            return $ VNet outNet
        _ -> traceShow ("indexed into non-vector", e, idx) $ return VError
evalExpr (EMux cond then_ else_ ty) = do
    cond' <- asNet =<< evalExpr cond
    then_' <- asNet =<< evalExpr then_
    else_' <- asNet =<< evalExpr else_
    outNet <- buildNet "<mux>" 0 ty
    buildLogic (A.LkMux ("val" <| S.empty) 2) [cond', then_', else_'] [outNet]
    return $ VNet outNet
evalExpr (EValidIf cond then_ ty) = do
    cond' <- asNet =<< evalExpr cond
    then_' <- asNet =<< evalExpr then_
    else_' <- buildNet "<undef>" 0 ty
    outNet <- buildNet "<mux>" 0 ty
    buildLogic (A.LkMux ("val" <| S.empty) 2) [cond', then_', else_'] [outNet]
    return $ VNet outNet
evalExpr (EPrim op args _ ty) = do
    argNets <- mapM asNet =<< mapM evalExpr args
    outNet <- buildNet ("<" <> op <> ">") 0 ty
    buildLogic A.LkExpr argNets [outNet]
    return $ VNet outNet

-- Generate a new value of type `ty`, whose leaf fields are combined using an
-- `LkPack` node and output to `outNet`.  This is used for handling registers
-- of non-ground type.
packTy :: Text -> Ty -> A.NetId -> ExtractM Value
packTy name ty outNet = do
    (v, leaves) <- makeTypedValue name ty
    let (names, nets) = unzip leaves
    if length leaves == 1 then
        buildNetAlias (head nets) outNet
    else
        buildLogic (A.LkPack $ S.fromList names) nets [outNet]
    return v

unpackTy :: Text -> Ty -> A.NetId -> ExtractM Value
unpackTy name ty inNet = do
    (v, leaves) <- makeTypedValue name ty
    let (names, nets) = unzip leaves
    if length leaves == 1 then
        buildNetAlias inNet (head nets)
    else
        buildLogic (A.LkUnpack $ S.fromList names) [inNet] nets
    return v

-- Construct a `Value` that's an instance of `Ty`.  Also returns the net IDs of
-- all the leaf values, together with names.
makeTypedValue :: Text -> Ty -> ExtractM (Value, [(Text, A.NetId)])
makeTypedValue prefix (TBundle fs) = do
    (m, leaves) <- liftM mconcat $ forM fs $ \f -> do
        -- FIRRTL spec says memories contain only passive types.  I assume
        -- registers work the same way.
        when (fieldFlip f) $
            traceShowM ("unexpected flip field in bundle", f)
        (val, leaves) <- makeTypedValue (prefix <> "." <> fieldName f) (fieldTy f)
        return (M.singleton (fieldName f) (val, fieldFlip f), leaves)
    return (VBundle m, leaves)
makeTypedValue prefix (TVector ty _) = do
    (v, leaves) <- makeTypedValue (prefix <> ".items") ty
    let v' = case v of
            VNet n -> VVector n
            _ -> traceShow ("vector of non-primitive NYI", v) VError
    return (v', leaves)
makeTypedValue prefix ty = do
    n <- buildNet prefix 11 ty
    return (VNet n, [(prefix, n)])



bindDef :: Text -> Ty -> ExtractM ()
bindDef name ty = _esDefTy %= M.insert name ty

splitDef :: Text -> ExtractM ()
splitDef name = do
    _esSplitDefs %= Set.insert name
    optTy <- use $ _esDefTy . at name
    let ty = case optTy of
            Nothing -> traceShow ("splitDef: missing DefTy entry", name) TUnknown
            Just x -> x
    bindDef (name <> ".L") ty
    bindDef (name <> ".R") ty

evalDef' :: Def -> ExtractM ()
evalDef' (DWire name ty) = bindDef name ty >> splitDef name
evalDef' (DReg name ty _clk _res _init) = bindDef name ty >> splitDef name
evalDef' (DInst name modName) = use (_esModMap . at modName) >>= \x -> case x of
    Nothing ->
        traceShow ("instantiation of unknown module", name, modName) $ return ()
    Just mi -> bindDef name $ moduleSig $ miPorts mi
evalDef' (DMemory name ty depth rds wrs rdwrs) = do
    forM_ rds $ \rd -> bindDef rd ty
    forM_ wrs $ \wr -> bindDef wr ty
    forM_ rdwrs $ \rdwr -> bindDef rdwr ty >> splitDef rdwr
evalDef' (DNode name expr) = bindDef name =<< exprTy expr
evalDef' (DCMem name ty depth isSync) = return ()
evalDef' (DCMemPort name ty memName args dir)
  | [addr, _clk] <- args = use (_esLocalScope . at memName) >>= \x -> case x of
    Just (VRam idx ty') -> do
        bindDef name ty'
        when (dir `elem` [MpdInfer, MpdReadWrite]) $ splitDef name
    Just v -> traceShowM ("port references non-memory", memName, name, v)
    Nothing -> traceShowM ("unknown local memory", memName, name)
  | otherwise = traceShowM ("bad arg count for CMemPort", name, memName, args)

-- Compute the set of net connections made by a `Stmt`.  (A statement can also
-- disconnect nets, represented as connecting the net to `Nothing`.)  Statement
-- evaluation can also have side effects on the extraction state, such as
-- producing new logic nodes or adding variables to the current scope.
evalStmt' :: Stmt -> ExtractM ConnTree
evalStmt' (SDef _ d) = evalDef' d >> return M.empty
evalStmt' (SCond _ cond then_ else_) = do
    c <- evalRvalue cond
    t <- evalStmt' then_
    e <- evalStmt' else_
    return $ connTreeJoin c t e
evalStmt' (SBlock stmts) =
    -- `M.union` is left-biased, so we `flip` it to make new connections
    -- override the old ones.
    foldM (\acc s -> connTreeAppend acc <$> evalStmt' s) M.empty stmts
evalStmt' (SPartialConnect _ lhs rhs) = unrollAssign lhs rhs >>= assignTree
evalStmt' (SConnect _ lhs rhs) = unrollAssign lhs rhs >>= assignTree
evalStmt' (SIsInvalid _ e) = unrollInvalidate e >>= assignTree
evalStmt' (SAttach src es) = traceShow ("evalStmt SAttach NYI", src, es) $ return M.empty
evalStmt' (SStop _ _ _ _) = return M.empty
evalStmt' (SPrint _ _ _ _ _) = return M.empty
evalStmt' SEmpty = return M.empty

ensureNode :: NodeId -> ConnTree -> ExtractM ConnTree
ensureNode n ct = connTreeEnsureNode n <$> nodeTy n <*> pure ct

assignTree :: [(Ty, ConnExpr, Rvalue)] -> ExtractM ConnTree
assignTree assigns = foldM (\acc (_ty, l, r) -> do
    acc' <- ensureNode (connExprNode l) acc
    return $ connTreeInsertRvalue l r acc') M.empty assigns

defTy :: Text -> ExtractM Ty
defTy name = use (_esDefTy . at name) >>= \x -> case x of
    Nothing -> traceShow ("failed to resolve def", name) $ return TUnknown
    Just x -> return x

nodeTy :: NodeId -> ExtractM Ty
nodeTy n = defTy n

exprTy :: Expr -> ExtractM Ty
exprTy (ELit (LUInt _ w)) = return $ TUInt w
exprTy (ELit (LSInt _ w)) = return $ TSInt w
exprTy (ELit (LFixed _ w p)) = return $ TFixed w p
exprTy (ERef name _) = defTy name
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
exprTyFlip (ERef name _) = defTy name >>= \ty -> return (ty, False)
exprTyFlip (EField e name _) = bundleFieldTyFlip name <$> exprTyFlip e
exprTyFlip (EIndex e _ _) = vectorItemTyFlip <$> exprTyFlip e
exprTyFlip (EIndexC e _ _) = vectorItemTyFlip <$> exprTyFlip e
exprTyFlip (EMux _ t _ _) = exprTyFlip t
exprTyFlip (EValidIf _ t _) = exprTyFlip t
-- TODO: EPrim ty is probably TUnknown in most case, just like the others
exprTyFlip (EPrim _ _ _ ty) = return (ty, False)

projectConnExpr p (ConnExpr n ps) = ConnExpr n (ps |> p)

projectRvalue :: Projection -> Rvalue -> Rvalue
projectRvalue p rv = go rv
  where
    go (RvExpr (ConnExpr n ps)) = RvExpr $ ConnExpr n (ps |> p)
    go (RvMux c t e) = RvMux c (go t) (go e)
    go (RvIndexMux ie ic t e) = RvIndexMux ie ic (go t) (go e)
    go RvInvalid = RvInvalid
    go RvUnset = RvUnset

projectTy :: Projection -> Ty -> Ty
projectTy (PrIndex idx) (TVector ty len)
  | 0 <= idx && idx < len = ty
projectTy (PrField name) (TBundle fs)
  | Just f <- find (\f -> fieldName f == name) fs = fieldTy f
projectTy p t = traceShow ("bad projection (Ty)", p, t) TUnknown

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
    go (ELit _) = return $ RvExpr $ ConnExpr "<lit>" S.empty
    go (ERef name _) = do
        isSplit <- Set.member name <$> use _esSplitDefs
        let name' = if not isSplit then name else
                if flipSplit then name <> ".L" else name <> ".R"
        return $ RvExpr $ ConnExpr name' S.empty
    go (EField e name _) = projectRvalue (PrField name) <$> go e
    go (EIndex e idx _) = return $ RvExpr $ ConnExpr "<idx>" S.empty
    go (EIndexC e idx _) = projectRvalue (PrIndex idx) <$> go e
    go (EMux cond then_ else_ _) =
        RvMux <$> go cond <*> go then_ <*> go else_
    go (EValidIf cond then_ _) =
        RvMux <$> go cond <*> go then_ <*> pure RvInvalid
    go (EPrim op args _ ty) = return $ RvExpr $ ConnExpr ("<" <> op <> ">") S.empty


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

evalLvalue :: Expr -> ExtractM (Ty, [(ConnExpr, Rvalue -> Rvalue)])
evalLvalue e = evalLvalue' False e

-- Convert an lvalue `Expr` to a `ConnExpr`.  This can produce multiple
-- `ConnExpr`s in some cases, and also sometimes generates a transformation to
-- apply to the `Rvalue` of the assignment.
evalLvalue' :: Bool -> Expr -> ExtractM (Ty, [(ConnExpr, Rvalue -> Rvalue)])
evalLvalue' flipSplit e = go e
  where
    go (ERef name _) = do
        isSplit <- Set.member name <$> use _esSplitDefs
        let name' = if not isSplit then name else
                if flipSplit then name <> ".R" else name <> ".L"
        ty <- defTy name
        return (ty, [(ConnExpr name' S.empty, id)])
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
        (ty, lfs'') <- evalLvalue' (flipped /= lFlipped') l'
        traceShowM ("directions", l, r, lFlipped, rFlipped, flipped)
        return [(ty, l'', f r'') | (l'', f) <- lfs'']

unrollInvalidate :: Expr -> ExtractM [(Ty, ConnExpr, Rvalue)]
unrollInvalidate l = do
    assignsL <- unrollAggregate <$> exprTy l <*> pure l <*> pure False
    ls <- liftM concat $ forM assignsL $ \(l') -> do
        (ty, lfs'') <- evalLvalue' False l'
        return [(ty, l'', f RvInvalid) | (l'', f) <- lfs'']

    assignsR <- unrollAggregate <$> exprTy l <*> pure l <*> pure True
    rs <- liftM concat $ forM assignsR $ \(l') -> do
        (ty, lfs'') <- evalLvalue' True l'
        return [(ty, l'', f RvInvalid) | (l'', f) <- lfs'']

    return $ ls ++ rs



moduleSig :: [Port] -> Ty
moduleSig ps = TBundle $ map (\p ->
    Field (portName p) (portTy p) (portDir p == Input)) ps

buildIfcNets :: Text -> Interface -> Ty -> ExtractM (Value, [A.NetId], [A.NetId])
buildIfcNets prefix (IfBundle ifcFields _) (TBundle tyFields) = do
    parts <- forM (zip (toList ifcFields) tyFields) $ \((ifcName, subIfc), fld) -> do
        when (ifcName /= fieldName fld) $
            traceShowM ("buildIfcNets: field name mismatch", ifcName, fieldName fld)
        (v, ins, outs) <- buildIfcNets (prefix <> "." <> ifcName) subIfc (fieldTy fld)
        return (M.singleton ifcName (v, fieldFlip fld), ins, outs)
    let (fs, ins, outs) = mconcat parts
    return (VBundle fs, ins, outs)
buildIfcNets prefix (IfPort dir _ _) ty
  | TUInt _ <- ty = buildLeafNet prefix dir ty VNet
  | TSInt _ <- ty = buildLeafNet prefix dir ty VNet
  | TFixed _ _ <- ty = buildLeafNet prefix dir ty VNet
  | TVector _ _ <- ty = buildLeafNet prefix dir ty VVector
  | TClock <- ty = buildLeafNet prefix dir ty VNet
buildIfcNets prefix ifc ty = do
    traceShowM ("don't know how to build interface nets", prefix, ifc, ty)
    return (VError, [], [])

buildLeafNet name dir ty ctor = do
    n <- buildNet name 11 ty
    if dir == Input then
        return (ctor n, [n], [])
    else
        return (ctor n, [], [n])


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

doConnect :: Value -> Value -> ExtractM (Map A.NetId (Maybe A.NetId))
doConnect l (VDff r _) = doConnect l r
doConnect (VDff _ l) r = doConnect l r
doConnect (VNet l) (VNet r) = return $ M.singleton l (Just r)
doConnect (VVector l) (VVector r) = return $ M.singleton l (Just r)
doConnect (VBundle lf) (VBundle rf) =
    liftM mconcat $
    mapM (\(a, b) -> doConnect a b) $
    M.elems $
    M.intersectionWithKey (\k (l, lFlip) (r, rFlip) ->
        (if lFlip /= rFlip then
            traceShow ("flip mismatch on field", k, lFlip, rFlip)
        else id) $ if lFlip then (r, l) else (l, r)) lf rf
doConnect lv rv = traceShow ("error: bad connection", lv, rv) $ return M.empty

doDisconnect :: Value -> ExtractM (Map A.NetId (Maybe A.NetId))
doDisconnect v = go v False
  where
    go :: Value -> Bool -> ExtractM (Map A.NetId (Maybe A.NetId))
    go (VNet l) f = return $ if f then M.empty else M.singleton l Nothing
    go (VVector l) f = return $ if f then M.empty else M.singleton l Nothing
    go (VBundle lf) f =
        liftM mconcat $
        mapM (\(l, lFlip) -> go l (if lFlip then not f else f)) $
        M.elems lf
    go (VDff _ l) f = go l f
    go (VRam idx ty) _ = do
        traceShowM ("error: tried to disconnect RAM", idx, ty)
        return M.empty
    go VError _ = return M.empty

makeConnections :: Map A.NetId (Maybe A.NetId) -> ExtractM ()
makeConnections m = forM_ (M.toList m) $ \(l, optR) -> case optR of
    Just r -> void $ buildNetAlias r l
    Nothing -> return ()

nameValue :: Text -> Value -> ExtractM Value
nameValue name v = go name v False
  where
    -- `nameValue` is used on the RHS of `wire` statements.  For unflipped leaf
    -- nodes, we alias the original net `n` to a new, named net `n'`.  For
    -- flipped nodes, we do the aliasing the other way around - `n'` flows into
    -- `n`.
    go name (VNet n) f = do
        ty <- use $ _esCurModule . A._moduleNet n . A._netTy
        n' <- buildNet' name 5 ty
        if f then buildNetAlias n' n else buildNetAlias n n'
        return $ VNet n'
    go name (VVector n) f = do
        ty <- use $ _esCurModule . A._moduleNet n . A._netTy
        n' <- buildNet' name 5 ty
        if f then buildNetAlias n' n else buildNetAlias n n'
        return $ VVector n'
    go name (VBundle fs) f' = do
        VBundle <$> M.traverseWithKey (\k (v, f) ->
            go (name <> "." <> k) v (f /= f') >>= \v' -> return (v', f)) fs
    go name (VDff r l) True =
        traceShow ("impossible: naming VDff in flipped context?", r, l) $
        return $ VDff r l
    go name (VDff r l) False = do
        r' <- go name r False
        l' <- go name l True
        return $ VDff r l
    go _ (VRam idx ty) _ = return $ VRam idx ty
    go _ VError _ = return VError
