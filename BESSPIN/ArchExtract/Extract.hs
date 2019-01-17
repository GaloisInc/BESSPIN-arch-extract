module BESSPIN.ArchExtract.Extract where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T

import Debug.Trace

import BESSPIN.ArchExtract.Architecture
import qualified BESSPIN.ArchExtract.Architecture as A
import BESSPIN.ArchExtract.Verilog
import qualified BESSPIN.ArchExtract.Verilog as V


extractArch :: [V.ModuleDecl] -> A.Design
extractArch vMods = A.Design $ S.fromList $ map (extractMod interMap) vMods
  where
    interMap = buildInterMap vMods

extractMod :: InterMap -> V.ModuleDecl -> A.ModDecl
extractMod interMap vMod = reconnectNets $
    A.ModDecl (moduleName vMod) inputs outputs insts logics nets
  where
    instMap = buildInstMap interMap (moduleItems vMod)
    netParts = moduleNets instMap vMod
    (nets, netMap) = buildNets netParts
    (inputs, outputs) = convModulePorts netMap vMod
    insts = convModuleInsts instMap netMap vMod
    logics = convModuleLogic netMap vMod




-- Public interface information, used when processing `ModInst`s.
data ModInter = ModInter
    { modInterId :: A.ModId
    , modInterPorts :: [V.PortDecl]
    }
    deriving (Show)

type InterMap = Map V.NodeId ModInter

buildInterMap :: [V.ModuleDecl] -> InterMap
buildInterMap mods = M.fromList $ zipWith f [0..] mods
  where
    f modId vMod = (moduleId vMod, ModInter modId (modulePorts vMod))


-- Port resolution data for module instances.

-- A map from `Instance.modItemId` to the `ModInter` for the instantiated
-- module.  This map always includes every `Instance` in the current module.
type InstMap = Map V.NodeId ModInter

buildInstMap :: InterMap -> [V.ModItem] -> InstMap
buildInstMap interMap items = M.fromList $ mapMaybe go items
  where
    go :: V.ModItem -> Maybe (V.NodeId, ModInter)
    go (Instance instId vModId name _ _) =
        case M.lookup vModId interMap of
            Nothing ->
                trace ("no declaration found for module instantiation " ++ T.unpack name ++
                    " (node " ++ show vModId ++ ")") $
                Just (instId, ModInter (-1) [])
            Just inter -> Just (instId, inter)
    go _ = Nothing


-- Info used to build a `Net` and its corresponding `NetMap` entry.
data NetParts = NetParts
    { netPartsName :: Text
    , netPartsOrigin :: NetOrigin
    }
    deriving (Show)

data NetOrigin = NoDef V.NodeId | NoInstPort V.NodeId Int
    deriving (Show, Eq, Ord)

type NetMap = Map NetOrigin NetId

buildNets :: [NetParts] -> (Seq A.Net, NetMap)
buildNets nps = foldl f (S.empty, M.empty) nps
  where
    f (nets, netMap) (NetParts name origin) =
        let netId = S.length nets in
        (nets |> Net name S.empty S.empty,
            M.insert origin netId netMap)

portNet :: V.PortDecl -> NetParts
portNet (V.PortDecl id name _ _) = NetParts name (NoDef id)

itemNets :: InstMap -> V.ModItem -> [NetParts]
itemNets instMap (Instance instId _ name _ _) = zipWith f (modInterPorts inter) [0..]
  where
    inter = instMap M.! instId
    f port idx = NetParts
        (name <> T.pack "." <> V.portDeclName port)
        (NoInstPort instId idx)
itemNets _ (VarDecl id name _ _ _) = [NetParts name (NoDef id)]
itemNets _ _ = []

moduleNets :: InstMap -> V.ModuleDecl -> [NetParts]
moduleNets instMap vMod =
    map portNet (modulePorts vMod) ++
    concat (map (itemNets instMap) (moduleItems vMod))


-- Module port handling

convPort :: NetMap -> V.PortDecl -> (Seq A.PortDecl, Seq A.PortDecl)
convPort netMap (V.PortDecl id name _ dir) =
    let port = A.PortDecl name (netMap M.! NoDef id) in
    case dir of
        Input -> (S.singleton port, S.empty)
        Output -> (S.empty, S.singleton port)
        InOut -> (S.singleton port, S.singleton port)

convPortVar :: NetMap -> V.ModItem -> (Seq A.PortDecl, Seq A.PortDecl)
convPortVar netMap (V.VarDecl id name _ dir _) =
    let port = A.PortDecl name (netMap M.! NoDef id) in
    case dir of
        Just Input -> (S.singleton port, S.empty)
        Just Output -> (S.empty, S.singleton port)
        Just InOut -> (S.singleton port, S.singleton port)
        Nothing -> (S.empty, S.empty)
convPortVar _ _ = (S.empty, S.empty)

convModulePorts :: NetMap -> V.ModuleDecl -> (Seq A.PortDecl, Seq A.PortDecl)
convModulePorts netMap vMod =
    mconcat (map (convPort netMap) $ modulePorts vMod) <>
    mconcat (map (convPortVar netMap) $ moduleItems vMod)


-- Module instantiation handling (`ModInst` only, no `Logic`)

convInst :: InstMap -> NetMap -> V.ModItem -> Maybe A.ModInst
convInst instMap netMap (Instance instId _ name _ portConns) =
    Just $ A.ModInst (modInterId inter) name inputs outputs
  where
    inter = instMap M.! instId
    (inputs, outputs) = mconcat $ zipWith3 f (modInterPorts inter) portConns [0..]
    f decl conn idx =
        let netId = netMap M.! NoInstPort instId idx in
        case portDeclDir decl of
            Input -> (S.singleton netId, S.empty)
            Output -> (S.empty, S.singleton netId)
            InOut -> (S.singleton netId, S.singleton netId)
convInst _ _ _ = Nothing

convModuleInsts :: InstMap -> NetMap -> V.ModuleDecl -> Seq A.ModInst
convModuleInsts instMap netMap vMod =
    S.fromList $ mapMaybe (convInst instMap netMap) $ moduleItems vMod


-- Logic element handling.  Includes logic statements, variable initializers,
-- and port connection expressions.

-- Get the def IDs of all wires/ports/etc used in `e`.
exprVars :: Expr -> Seq NetOrigin
exprVars e = go e
  where
    go (Var defId) = S.singleton $ NoDef defId
    go (Index base idx) = go base <> goIdx idx
    go (MemIndex base idxs) = go base <> mconcat (map goIdx idxs)
    go (Const _) = S.empty
    go (Concat es) = mconcat (map go es)
    go (MultiConcat rep es) = go rep <> mconcat (map go es)
    go (IfExpr cond then_ else_) = go cond <> go then_ <> go else_
    go (Unary _ arg) = go arg
    go (Binary _ left right) = go left <> go right
    go (Field base _) = go base
    go (AssignPat rep es) = go rep <> mconcat (map go es)
    go UnknownExpr = S.empty

    goIdx (ISingle e) = go e
    goIdx (IRange l r) = go l <> go r

mkLogic :: NetMap -> Seq NetOrigin -> Seq NetOrigin -> Logic
mkLogic netMap ins outs = Logic (foldMap f ins) (foldMap f outs)
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
    go imp (Block ss) = mconcat (map (go imp) ss)
    go imp (If cond then_ else_) =
        let imp' = imp <> exprVars cond in
        go imp' then_ <> maybe S.empty (go imp') else_
    go imp (Case cond cases) =
        let imp' = imp <> exprVars cond in
        mconcat (map (go imp' . snd) cases)
    go imp (For inits cond steps body) =
        -- Not sure how much of `For` we really need to handle, but here's a
        -- conservative guess...
        let imp' = imp <> exprVars cond in
        mconcat (map (go imp') inits) <>
        mconcat (map (go imp') steps) <>
        go imp' body
    go imp (NonBlockingAssign lval rval) =
        S.singleton $ mkLogic netMap (imp <> exprVars rval) (exprVars lval)
    go imp (BlockingAssign lval rval) =
        S.singleton $ mkLogic netMap (imp <> exprVars rval) (exprVars lval)
    go imp (BlockingUpdate lval _) =
        -- Something like `i++`, where the "lvalue" is also used as part of the
        -- "rvalue".
        S.singleton $ mkLogic netMap (imp <> exprVars lval) (exprVars lval)
    go imp NullStmt = S.empty
    go imp UnknownStmt = S.empty

itemLogic :: NetMap -> ModItem -> Seq Logic
itemLogic netMap i = go i
  where
    go (Instance instId _ _ _ portConns) =
        S.fromList $ zipWith f portConns [0..]
      where
        f conn idx = mkLogic netMap (exprVars conn) (S.singleton $ NoInstPort instId idx)
    go (VarDecl id _ _ _ init) = case init of
        Nothing -> S.empty
        Just e -> S.singleton $
            mkLogic netMap (exprVars e) (S.singleton $ NoDef id)
    go (ContAssign _ lval rval) = S.singleton $
        mkLogic netMap (exprVars rval) (exprVars lval)
    go (Always s) = stmtLogic netMap s
    go (Initial s) = stmtLogic netMap s

convModuleLogic :: NetMap -> V.ModuleDecl -> Seq Logic
convModuleLogic netMap vMod = mconcat $ map (itemLogic netMap) $ moduleItems vMod


-- Rebuild the `netSources` and `netSinks` connection lists for all nets in
-- `mod`, based on the `Inputs` and `Outputs` lists for the module itself and
-- all its `ModInst`s and `Logic`s.
reconnectNets :: A.ModDecl -> A.ModDecl
reconnectNets mod = runST $ do
    sources <- newArray (0, S.length (modDeclNets mod) - 1) S.empty
    sinks <- newArray (0, S.length (modDeclNets mod) - 1) S.empty

    connect (modDeclInputs mod) portDeclNet sources ExtPort
    connect (modDeclOutputs mod) portDeclNet sinks ExtPort
    void $ flip S.traverseWithIndex (modDeclInsts mod) $ \i inst -> do
        connect (modInstInputs inst) id sinks (InstPort i)
        connect (modInstOutputs inst) id sources (InstPort i)
    void $ flip S.traverseWithIndex (modDeclLogics mod) $ \i inst -> do
        connect (logicInputs inst) id sinks (LogicPort i)
        connect (logicOutputs inst) id sources (LogicPort i)

    nets' <- flip S.traverseWithIndex (modDeclNets mod) $ \i net -> do
        netSources <- readArray sources i
        netSinks <- readArray sinks i
        return $ net { netSources = netSources, netSinks = netSinks }
    return $ mod { modDeclNets = nets' }

  where
    connect :: Seq a -> (a -> NetId) -> STArray s NetId (Seq Conn) -> (Int -> Conn) -> ST s ()
    connect ports portNet netConns mkConn =
        void $ flip S.traverseWithIndex ports $ \i p -> do
            let netId = portNet p
            conns <- readArray netConns netId
            let conns' = conns |> mkConn i
            writeArray netConns netId conns'
