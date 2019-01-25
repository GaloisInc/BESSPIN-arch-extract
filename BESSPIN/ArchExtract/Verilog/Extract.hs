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

import BESSPIN.ArchExtract.Architecture
import qualified BESSPIN.ArchExtract.Architecture as A
import BESSPIN.ArchExtract.Verilog.AST
import qualified BESSPIN.ArchExtract.Verilog.AST as V
import BESSPIN.ArchExtract.Simplify


extractArch :: V.Design -> A.Design
extractArch vDes =
    mapMods (\mod ->
        --filterLogics (\_ _ -> False) $
        --filterInsts (\_ i ->
            --not $ modInstName i `elem` map T.pack ["ra1reg"]) $
        --filterInsts (\_ i ->
        --    not $ modDeclName (designMods d `S.index` modInstId i)
        --        `elem` map T.pack ["mux2", "eqcmp"]) $
        --filterInstsToLogic (\_ i ->
            --not $ modDeclName (designMods d `S.index` modInstId i)
                --`elem` []) $
                --`elem` map T.pack ["mux2", "eqcmp"]) $
                --`elem` map T.pack ["mux2", "eqcmp", "flopenr", "flopenrc"]) $
            --`elem` map T.pack ["adder"]) mod) $
        mod) $
    d
  where
    vMods = V.designModules vDes
    d = A.Design $ fmap (extractMod vMods) vMods

extractMod :: Seq V.Module -> V.Module -> A.ModDecl
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
    filterInstsToLogic (\_ i -> modInstId i /= -1) $
    reconnectNets $
    A.ModDecl (moduleName vMod) inputs outputs insts logics nets
  where
    --instMap = buildInstMap interMap (moduleItems vMod)
    netParts = moduleNets vMods vMod
    (nets, netMap) = buildNets netParts
    (inputs, outputs) = convPorts netMap vMod
    insts = convInsts vMods netMap vMod
    logics = convItems vMods netMap vMod


-- Building `modDeclNets` from `V.moduleDecls`

-- Info used to build a `Net` and its corresponding `NetMap` entry.
data NetParts = NetParts
    { netPartsName :: Text
    , netPartsNamePriority :: Int
    , netPartsOrigin :: NetOrigin
    }
    deriving (Show)

data NetOrigin =
    NoDecl { noDeclId :: Int } |
    -- noDeclId: index of the `InstDecl` in `moduleDecls`
    -- noPortIdx: index of the port in the instantiated module's `modulePorts`
    NoInstPort { noDeclId :: Int, noPortIdx :: Int }
    deriving (Show, Eq, Ord)

type NetMap = Map NetOrigin NetId

buildNets :: [NetParts] -> (Seq A.Net, NetMap)
buildNets nps = foldl f (S.empty, M.empty) nps
  where
    f (nets, netMap) (NetParts name prio origin) =
        let netId = NetId $ S.length nets in
        (nets |> Net name prio S.empty S.empty,
            M.insert origin netId netMap)

prioExtPort = 3
prioInstPort = 2
prioWire = 4
prioDcNet = -1

declNet :: Seq V.Module -> Int -> V.Decl -> [NetParts]
declNet vMods i (V.PortDecl name _) = [NetParts name prioExtPort $ NoDecl i]
declNet vMods i (V.ParamDecl name) = []
declNet vMods i (V.VarDecl name) = [NetParts name prioWire $ NoDecl i]
declNet vMods i (V.InstDecl name modId _) =
    let vMod = vMods `S.index` modId in
    zipWith (\j declId ->
            let portName = V.declName $ moduleDecls vMod `S.index` declId in
            NetParts (name <> T.pack "." <> portName) prioInstPort (NoInstPort i j))
        [0..] (modulePorts vMod)

moduleNets :: Seq V.Module -> V.Module -> [NetParts]
moduleNets vMods vMod = S.foldMapWithIndex (declNet vMods) (moduleDecls vMod)


-- Building `modDeclInputs`, `modDeclOutputs`, and `modDeclInsts` from
-- `V.moduleDecls`.

convPorts :: NetMap -> V.Module -> (Seq A.PortDecl, Seq A.PortDecl)
convPorts netMap vMod =
    mconcat $ map (\i ->
        let vPort = moduleDecls vMod `S.index` i in
        let port = A.PortDecl (V.declName vPort) (netMap M.! NoDecl i) in
        case portDeclDir vPort of
            V.Input -> (S.singleton port, S.empty)
            V.Output -> (S.empty, S.singleton port)
            V.InOut -> (S.singleton port, S.singleton port)
        ) (modulePorts vMod)

declInst :: Seq V.Module -> NetMap -> Int -> V.Decl -> Seq A.ModInst
declInst vMods netMap i (V.InstDecl name modId _) =
    let vMod = vMods `S.index` modId in
    let ins = [netMap M.! NoInstPort i j | (j, portIdx) <- zip [0..] (modulePorts vMod),
            portDeclDir (moduleDecls vMod `S.index` portIdx) `elem` [V.Input, V.InOut]] in
    let outs = [netMap M.! NoInstPort i j | (j, portIdx) <- zip [0..] (modulePorts vMod),
            portDeclDir (moduleDecls vMod `S.index` portIdx) `elem` [V.Output, V.InOut]] in
    S.singleton $ ModInst modId name (S.fromList ins) (S.fromList outs)
declInst _ _ _ _ = S.empty

convInsts :: Seq V.Module -> NetMap -> V.Module -> Seq A.ModInst
convInsts vMods netMap vMod = S.foldMapWithIndex (declInst vMods netMap) (moduleDecls vMod)


-- Building `modDeclLogics` from `V.moduleItems`

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

mkLogic :: NetMap -> LogicKind -> Seq NetOrigin -> Seq NetOrigin -> Logic
mkLogic netMap kind ins outs = Logic kind (foldMap f ins) (foldMap f outs)
  where
    f def = case M.lookup def netMap of
        Nothing -> S.empty
        Just n -> S.singleton n

stmtLogic :: NetMap -> Stmt -> Seq Logic
stmtLogic netMap s = go S.empty s
  where
    -- `imp`: Implicit inputs to any generated `Logic`s, resulting from
    -- the conditions on enclosing `if`s and such.
    go :: Seq NetOrigin -> Stmt -> Seq Logic
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

itemLogic :: Seq V.Module -> Seq V.Decl -> NetMap -> V.Item -> Seq Logic
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
    go (Always ss) = foldMap (stmtLogic netMap) ss
    go (Initial ss) = foldMap (stmtLogic netMap) ss

convItems :: Seq V.Module -> NetMap -> V.Module -> Seq Logic
convItems vMods netMap vMod =
    mconcat $ map (itemLogic vMods (moduleDecls vMod) netMap) $ moduleItems vMod
