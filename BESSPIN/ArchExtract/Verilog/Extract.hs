{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Verilog.Extract where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.Array.ST
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

import Debug.Trace

import BESSPIN.ArchExtract.Architecture hiding (moduleNets)
import qualified BESSPIN.ArchExtract.Architecture as A
import BESSPIN.ArchExtract.Verilog.AST
import qualified BESSPIN.ArchExtract.Verilog.AST as V
import BESSPIN.ArchExtract.Simplify


extractArch :: V.Design -> A.Design ()
extractArch vDes =
    mapMods (\mod ->
        --filterLogics (\_ _ -> False) $
        --filterInsts (\_ i ->
            --not $ modInstName i `elem` map T.pack ["ra1reg"]) $
        --filterInsts (\_ i ->
        --    not $ moduleName (designMods d `S.index` modInstId i)
        --        `elem` map T.pack ["mux2", "eqcmp"]) $
        --filterInstsToLogic (\_ i ->
            --not $ moduleName (designMods d `S.index` modInstId i)
                --`elem` []) $
                --`elem` map T.pack ["mux2", "eqcmp"]) $
                --`elem` map T.pack ["mux2", "eqcmp", "flopenr", "flopenrc"]) $
            --`elem` map T.pack ["adder"]) mod) $
        mod) $
    d
  where
    vMods = V.designModules vDes
    d = A.Design $ fmap (extractMod vMods) vMods

extractMod :: Seq V.Module -> V.Module -> A.Module ()
extractMod vMods vMod =
    --filterLogics (\_ _ -> False) $
    filterLogics (\_ l -> logicKind l /= LkNetAlias) $
    -- Break up "uninteresting" nets early, before they can get merged with
    -- other nets.
    disconnect (\_ _ _ net ->
        let baseName = last $ T.splitOn (T.singleton '.') (netName net) in
        not $ baseName `elem` map T.pack ["clock", "clk", "reset"]) $
    -- Convert instantiations of undefined modules to logic.  (Otherwise we hit
    -- errors when trying to look up the port definitions.)
    filterInstsToLogic (\_ _ i -> instModId i /= -1) $
    traceShow ("stateful nets for ", V.moduleName vMod, stateNets) $
        buildRegisters stateNets' $
    reconnectNets $
    A.Module (V.moduleName vMod) inputs outputs (insts <> logics) nets
  where
    --instMap = buildInstMap interMap (moduleItems vMod)
    netParts = moduleNets vMods vMod
    (nets, netMap) = buildNets netParts
    (inputs, outputs) = convPorts netMap vMod
    insts = convInsts vMods netMap vMod
    logics = convItems vMods netMap vMod

    stateNets = stateHoldingNets vMod
    stateNets' = Set.map (\no -> fst $ netMap M.! no) stateNets


-- Building `moduleNets` from `V.moduleDecls`

-- Info used to build a `Net` and its corresponding `NetMap` entry.
data NetParts = NetParts
    { netPartsName :: Text
    , netPartsNamePriority :: Int
    , netPartsOrigin :: NetOrigin
    , netPartsTy :: A.Ty
    }
    deriving (Show)

data NetOrigin =
    NoDecl { noDeclId :: Int } |
    -- noDeclId: index of the `InstDecl` in `moduleDecls`
    -- noPortIdx: index of the port in the instantiated module's `modulePorts`
    NoInstPort { noDeclId :: Int, noPortIdx :: Int }
    deriving (Show, Eq, Ord)

type NetMap = Map NetOrigin (NetId, A.Ty)

buildNets :: [NetParts] -> (Seq (A.Net ()), NetMap)
buildNets nps = foldl f (S.empty, M.empty) nps
  where
    f (nets, netMap) (NetParts name prio origin ty) =
        let netId = NetId $ S.length nets in
        (nets |> Net name prio S.empty S.empty (),
            M.insert origin (netId, ty) netMap)

prioExtPort = 3
prioInstPort = 2
prioWire = 4
prioDcNet = -1

declNet :: Seq V.Module -> V.Module -> Int -> V.Decl -> [NetParts]
-- TODO: types
declNet vMods vMod i (V.PortDecl name vTy _) =
    [NetParts name prioExtPort (NoDecl i) (convTy vMod vTy)]
declNet vMods vMod i (V.ParamDecl name vTy) = []
declNet vMods vMod i (V.VarDecl name vTy) =
    [NetParts name prioWire (NoDecl i) (convTy vMod vTy)]
declNet vMods vMod i (V.TypedefDecl _ _) = []
declNet vMods vMod i (V.InstDecl name modId _) =
    let vInstMod = vMods `S.index` modId in
    zipWith (\j declId ->
            let PortDecl portName vTy _ = moduleDecls vInstMod `S.index` declId in
            let ty = convTy vInstMod vTy in
            NetParts (name <> T.pack "." <> portName) prioInstPort (NoInstPort i j) ty)
        [0..] (modulePorts vInstMod)

convTy :: V.Module -> V.Ty -> A.Ty
convTy _ (V.TTy base packed unpacked) =
    let wire = A.TWire (not $ null packed) (not $ null unpacked) in
    let sim = A.TSimVal in
    case base of
        V.TLogic -> wire
        V.TReg -> wire
        V.TTri -> wire
        V.TInt -> sim
        V.TInteger -> sim
        V.TString -> sim
convTy vMod (V.TEnum ty) = A.TEnum $ convTy vMod ty
convTy vMod (V.TRef declId) =
    let V.TypedefDecl name ty = V.moduleDecls vMod `S.index` declId in
    A.TAlias name $ convTy vMod ty
convTy vMod V.TInfer = A.TWire False False -- TODO

moduleNets :: Seq V.Module -> V.Module -> [NetParts]
moduleNets vMods vMod = S.foldMapWithIndex (declNet vMods vMod) (moduleDecls vMod)


-- Building `moduleInputs`, `moduleOutputs`, and `moduleInsts` from
-- `V.moduleDecls`.

convPorts :: NetMap -> V.Module -> (Seq A.Port, Seq A.Port)
convPorts netMap vMod =
    mconcat $ map (\i ->
        let vPort = moduleDecls vMod `S.index` i in
        let (net, ty) = netMap M.! NoDecl i in
        let port = A.Port (V.declName vPort) net ty in
        case portDeclDir vPort of
            V.Input -> (S.singleton port, S.empty)
            V.Output -> (S.empty, S.singleton port)
            V.InOut -> (S.singleton port, S.singleton port)
        ) (modulePorts vMod)

declInst :: Seq V.Module -> NetMap -> Int -> V.Decl -> Seq (A.Logic ())
declInst vMods netMap i (V.InstDecl name modId _) =
    let vMod = vMods `S.index` modId in
    let mkPins :: [PortDir] -> Seq A.Pin
        mkPins dirs = S.fromList $ do
            (j, portIdx) <- zip [0..] (modulePorts vMod)
            guard $ portDeclDir (moduleDecls vMod `S.index` portIdx) `elem` dirs
            let (net, ty) = netMap M.! NoInstPort i j
            return $ Pin net ty
    in
    let ins = mkPins [V.Input, V.InOut] in
    let outs = mkPins [V.Output, V.InOut] in
    S.singleton $ Logic (LkInst $ Inst modId name) ins outs ()
declInst _ _ _ _ = S.empty

convInsts :: Seq V.Module -> NetMap -> V.Module -> Seq (A.Logic ())
convInsts vMods netMap vMod = S.foldMapWithIndex (declInst vMods netMap) (moduleDecls vMod)


-- Building `moduleLogics` from `V.moduleItems`

-- Get the origin info for every net used in `e`.
exprVars :: Expr -> Seq NetOrigin
exprVars e = go e
  where
    go (Var defId) = S.singleton $ NoDecl defId
    go (Index base idx) = go base <> goIdx idx
    go (MemIndex base idxs) = go base <> mconcat (map goIdx idxs)
    go (Const _) = S.empty
    go (Concat es) = mconcat (map go es)
    go (MultiConcat rep es) = go rep <> mconcat (map go es)
    go (IfExpr cond then_ else_) = go cond <> go then_ <> go else_
    go (Unary arg) = go arg
    go (Binary left right) = go left <> go right
    go (Field base _) = go base
    go (AssignPat rep es) = go rep <> mconcat (map go es)
    go UnknownExpr = S.empty

    goIdx (ISingle e) = go e
    goIdx (IRange l r) = go l <> go r

-- Get the `LogicKind` for an rvalue expression.
rvalKind :: Expr -> LogicKind
rvalKind (Var defId) = LkNetAlias
rvalKind _ = LkOther

mkLogic :: NetMap -> LogicKind -> Seq NetOrigin -> Seq NetOrigin -> A.Logic ()
mkLogic netMap kind ins outs = Logic kind (foldMap f ins) (foldMap f outs) ()
  where
    f def = case M.lookup def netMap of
        Nothing -> S.empty
        Just (net, ty) -> S.singleton $ Pin net ty

stmtLogic :: NetMap -> Stmt -> Seq (A.Logic ())
stmtLogic netMap s = go S.empty s
  where
    -- `imp`: Implicit inputs to any generated `Logic`s, resulting from
    -- the conditions on enclosing `if`s and such.
    go :: Seq NetOrigin -> Stmt -> Seq (A.Logic ())
    go imp (If cond then_ else_) =
        let imp' = imp <> exprVars cond in
        foldMap (go imp') then_ <> maybe S.empty (foldMap $ go imp') else_
    go imp (Case cond cases) =
        let imp' = imp <> exprVars cond in
        foldMap (\(_, ss) -> foldMap (go imp') ss) cases
    go imp (For inits cond steps body) =
        -- Not sure how much of `For` we really need to handle, but here's a
        -- conservative guess...
        let imp' = imp <> exprVars cond in
        foldMap (go imp) inits <>
        foldMap (go imp') steps <>
        foldMap (go imp') body
    go imp (NonBlockingAssign lval rval) =
        S.singleton $ mkLogic netMap LkOther (imp <> exprVars rval) (exprVars lval)
    go imp (BlockingAssign lval rval) =
        S.singleton $ mkLogic netMap LkOther (imp <> exprVars rval) (exprVars lval)
    go imp (BlockingUpdate lval) =
        -- Something like `i++`, where the "lvalue" is also used as part of the
        -- "rvalue".
        S.singleton $ mkLogic netMap LkOther (imp <> exprVars lval) (exprVars lval)

itemLogic :: Seq V.Module -> Seq V.Decl -> NetMap -> V.Item -> Seq (A.Logic ())
itemLogic vMods vDecls netMap i = go i
  where
    go (InitVar varId e) = S.singleton $
        mkLogic netMap (rvalKind e) (exprVars e) (S.singleton $ NoDecl varId)
    go (InitInst i conns) = mconcat $ zipWith3 f [0..] (modulePorts vMod) conns
      where
        inst = vDecls `S.index` i
        vMod = vMods `S.index` instanceModId inst
        f j portIdx conn =
            let portNet = S.singleton $ NoInstPort i j in
            let connNets = exprVars conn in
            let inLogic = S.singleton $
                    mkLogic netMap (rvalKind conn) connNets portNet in
            let outLogic = S.singleton $
                    mkLogic netMap LkNetAlias portNet connNets in
            case V.portDeclDir $ moduleDecls vMod `S.index` portIdx of
                Input -> inLogic
                Output -> outLogic
                InOut -> inLogic <> outLogic
    go (ContAssign l r) = S.singleton $
        mkLogic netMap (rvalKind r) (exprVars r) (exprVars l)
    go (Always _ ss) = foldMap (stmtLogic netMap) ss
    go (Initial ss) = foldMap (stmtLogic netMap) ss

convItems :: Seq V.Module -> NetMap -> V.Module -> Seq (A.Logic ())
convItems vMods netMap vMod =
    mconcat $ map (itemLogic vMods (moduleDecls vMod) netMap) $ moduleItems vMod


-- Conversion of state-holding nets to registers

-- Identify state-holding nets.  Our current heuristic is: a net holds state if
-- it appears on the LHS of an assignment inside an edge-sensitive `always`
-- block.
stateHoldingNets :: V.Module -> Set NetOrigin
stateHoldingNets mod = foldMap goItem $ V.moduleItems mod
  where
    goItem (Always evts body) | any (not . isNothing . eventEdge) evts = goStmts body
    goItem _ = Set.empty

    goStmts ss = mconcat $ map goStmt ss

    goStmt (If _ t e) = goStmts t <> maybe Set.empty goStmts e
    goStmt (Case _ cases) = mconcat $ map (goStmts . snd) cases
    goStmt (For inits _ steps body) = goStmts inits <> goStmts steps <> goStmts body
    goStmt (NonBlockingAssign l _) = goExpr l
    goStmt (BlockingAssign l _) = goExpr l
    goStmt (BlockingUpdate l) = goExpr l

    goExpr (Var declId) = Set.singleton $ NoDecl declId
    goExpr (Index base _) = goExpr base
    goExpr (MemIndex base _) = goExpr base
    goExpr (Concat exprs) = mconcat (map goExpr exprs)
    goExpr (MultiConcat _ exprs) = mconcat (map goExpr exprs)
    goExpr _ = Set.empty

-- Replace state-holding nets with `LkRegister` logic nodes.  This consists of
-- splitting the net into input and output halves and joining the two with the
-- new logic.  We do this for all state-holding nets in a single pass.
buildRegisters :: Set NetId -> A.Module () -> A.Module ()
buildRegisters targets mod = reconnectNets $ mod
    { A.moduleOutputs = outputs'
    , A.moduleLogics = logics'
    , A.moduleNets = nets'
    }
  where
    -- Old nets, with `$in` appended to the name of each target net.
    oldNets = S.mapWithIndex (\idx net ->
        if Set.member (NetId idx) targets then
            net { netName = netName net <> "$in" }
        else net) (A.moduleNets mod)
    newNets = S.fromList $ map (\i ->
        let net = mod `A.moduleNet` i in
        net { netName = netName net <> "$out" }) $ Set.toList targets
    nets' = oldNets <> newNets

    -- Maps each target NetId to the NetId of the newly-created output net.
    -- (The input net keeps the NetId of the original net.)
    reconnMap :: Map NetId NetId
    reconnMap = M.fromList $
        zip (Set.toList targets) (map NetId [S.length $ A.moduleNets mod ..])

    goNetId :: NetId -> NetId
    goNetId i = fromMaybe i $ M.lookup i reconnMap

    outputs' =
        fmap (\port -> port { portNet = goNetId $ portNet port }) (A.moduleOutputs mod)
    oldLogics = fmap
        (\logic -> logic { logicInputs = fmap (\p ->
            Pin (goNetId $ pinNet p) TSimVal -- TODO
            ) $ logicInputs logic })
        (A.moduleLogics mod)
    newLogics = S.fromList $ map (\(old, new) ->
            let net = mod `A.moduleNet` old in
            let ty = TSimVal in -- TODO
            Logic (LkRegister $ A.netName net)
                (S.singleton $ Pin old ty)
                (S.singleton $ Pin new ty) ())
        (M.toList reconnMap)
    logics' = oldLogics <> newLogics
