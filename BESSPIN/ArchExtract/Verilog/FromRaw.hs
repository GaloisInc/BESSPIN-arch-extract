module BESSPIN.ArchExtract.Verilog.FromRaw where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set

import BESSPIN.ArchExtract.Verilog.AST
import BESSPIN.ArchExtract.Verilog.Raw (NodeId, Node)
import qualified BESSPIN.ArchExtract.Verilog.Raw as R

import Debug.Trace


data Numbering v = Numbering (Map NodeId Int) (Seq v)

data S = S
    { nodeMap :: Map NodeId Node
    , modules :: Numbering Module
    , decls :: Numbering Decl
    }

type FromRawM a = State S a

getNode :: NodeId -> FromRawM Node
getNode i = gets $ \s -> case M.lookup i $ nodeMap s of
    Nothing -> error $ "no entry for " ++ show i ++ "?"
    Just x -> x


mkRef :: (S -> Numbering a) -> (Numbering a -> S -> S) ->
    (NodeId -> FromRawM a) -> NodeId -> FromRawM Int
mkRef getN setN mkDef i = do
    Numbering m s <- gets getN
    case M.lookup i m of
        Nothing -> do
            -- Assign an ID to the def, and insert a placeholder element.  This
            -- way, `mkDef` can build a ref to the definition itself, as long
            -- as it only looks at the ID and doesn't try to inspect the actual
            -- value.
            let j = S.length s
            let m' = M.insert i j m
            let s' = s |> error "cyclic module access"
            modify $ setN $ Numbering m' s'

            -- Build the definition
            x <- mkDef i

            -- Replace the `error` placeholder with the definition.
            Numbering m s <- gets getN
            let s' = S.update j x s
            modify $ setN $ Numbering m s'
            return j

        Just j -> return j

moduleRef :: NodeId -> FromRawM Int
moduleRef i = mkRef modules (\x s -> s { modules = x }) mkModule i

declRef :: NodeId -> FromRawM Int
declRef i = mkRef decls (\x s -> s { decls = x }) mkDecl i

collectDefs :: (S -> Numbering a) -> (Numbering a -> S -> S) ->
    FromRawM b -> FromRawM (b, Seq a)
collectDefs getN setN m = do
    old <- gets getN
    modify $ setN $ Numbering M.empty S.empty
    x <- m
    Numbering _ s <- gets getN
    modify $ setN old
    return (x, s)

collectModules :: FromRawM a -> FromRawM (a, Seq Module)
collectModules m = collectDefs modules (\x s -> s { modules = x }) m

collectDecls :: FromRawM a -> FromRawM (a, Seq Decl)
collectDecls m = collectDefs decls (\x s -> s { decls = x }) m


toList :: FromRawM a -> FromRawM [a]
toList m = m >>= \x -> return [x]


mkDesign :: [NodeId] -> FromRawM Design
mkDesign is = do
    ((), mods) <- collectModules $ mapM_ moduleRef is
    return $ Design mods

mkModule :: NodeId -> FromRawM Module
mkModule i = do
    n <- getNode i
    case n of
        R.Module name ports params items -> do
            (items, decls) <- collectDecls $ do
                mapM_ declRef ports
                mapM_ declRef params
                items <- concat <$> mapM mkItems items
                return items
            let ports = S.foldMapWithIndex (\i d -> case d of
                    PortDecl _ _ -> [i]
                    _ -> []) decls
            return $ Module name decls ports items
        _ -> error $ "expected module at " ++ show i

mkDecl :: NodeId -> FromRawM Decl
mkDecl i = do
    n <- getNode i
    case n of
        R.Variable name dims init (Just dir) ->
            PortDecl <$> pure name <*> pure dir
        R.Variable name dims init Nothing ->
            VarDecl <$> pure name
        R.ParamId name init dims ->
            ParamDecl <$> pure name
        R.InstId parent name conns -> do
            n' <- getNode parent
            case n' of
                R.ModuleInstantiation mod params _ ->
                    InstDecl <$> pure name <*> moduleRef mod <*> mapM mkExpr params
                _ -> error $ "expected module instantiation at " ++ show i
        _ -> error $ "expected declaration at " ++ show i

mkItems :: NodeId -> FromRawM [Item]
mkItems i = do
    n <- getNode i
    case n of
        R.DataDecl ids -> concat <$> mapM mkDeclItems ids
        R.NetDecl ids -> concat <$> mapM mkDeclItems ids
        R.ModuleInstantiation _ _ ids -> concat <$> mapM mkDeclItems ids
        R.ContinuousAssign assigns ->
            forM assigns $ \i -> getNode i >>= \n -> case n of
                R.NetRegAssign l r -> ContAssign <$> mkExpr l <*> mkExpr r
                _ -> error $ "expected NetRegAssign at " ++ show i
        R.AlwaysConstruct body -> toList $ Always <$> mkStmts body
        R.InitialConstruct body -> toList $ Initial <$> mkStmts body
        _ -> trace ("unknown mod item at " ++ show i) $ return []

mkDeclItems :: NodeId -> FromRawM [Item]
mkDeclItems i = do
    n <- getNode i
    case n of
        R.Variable _ _ (Just init) _ ->
            toList $ InitVar <$> declRef i <*> mkExpr init
        R.Variable _ _ _ _ -> return []
        R.ParamId _ (Just init) _ ->
            toList $ InitVar <$> declRef i <*> mkExpr init
        R.ParamId _ _ _ -> return []
        R.TypeId -> return []
        R.InstId _ _ portConns ->
            toList $ InitInst <$> declRef i <*> mapM mkExpr portConns
        _ -> error $ "expected item-like decl at " ++ show i

mkStmts :: NodeId -> FromRawM [Stmt]
mkStmts i = do
    n <- getNode i
    case n of
        R.SeqBlock ds ss -> mapM_ declRef ds >> concat <$> mapM mkStmts ss
        R.EventControlStatement s -> mkStmts s
        R.ConditionalStatement cond then_ (Just else_) -> toList $
            If <$> mkExpr cond <*> mkStmts then_ <*> (Just <$> mkStmts else_)
        R.ConditionalStatement cond then_ Nothing -> toList $
            If <$> mkExpr cond <*> mkStmts then_ <*> pure Nothing
        R.CaseStatement cond cases -> toList $
            Case <$> mkExpr cond <*> mapM mkCase cases
        R.For inits cond steps body -> toList $
            For <$> (concat <$> mapM mkStmts inits)
                <*> mkExpr cond
                <*> (concat <$> mapM mkStmts steps)
                <*> mkStmts body
        R.NonBlockingAssign l r -> toList $
            NonBlockingAssign <$> mkExpr l <*> mkExpr r
        R.BlockingAssign l r -> toList $
            BlockingAssign <$> mkExpr l <*> mkExpr r
        R.BlockingAssignInPlace l -> toList $
            BlockingUpdate <$> mkExpr l
        R.DelayControlStatement s -> mkStmts s
        R.NullStatement -> return []
        _ -> trace ("unknown statement at " ++ show i) $ return []
        --_ -> error $ "expected statement at " ++ show i

mkCase i = do
    n <- getNode i
    case n of
        R.CaseItem es body -> do
            a <- mapM mkExpr es
            b <- mkStmts body
            return (a, b)
        _ -> error $ "expected CaseItem at " ++ show i

mkExpr :: NodeId -> FromRawM Expr
mkExpr i = do
    n <- getNode i
    case n of
        R.IdRef i -> Var <$> declRef i
        R.IndexedId base ix -> Index <$> mkExpr base <*> mkIndex ix
        R.IndexedMemoryId base ixs -> MemIndex <$> mkExpr base <*> mapM mkIndex ixs
        R.ConstVal t -> return $ Const t
        R.IntVal t -> return $ Const t
        R.RealVal t -> return $ Const t
        R.Concat es -> Concat <$> mapM mkExpr es
        R.MultiConcat rep es -> MultiConcat <$> mkExpr rep <*> mapM mkExpr es
        R.QuestionColon cond then_ else_ ->
            IfExpr <$> mkExpr cond <*> mkExpr then_ <*> mkExpr else_
        R.UnaryOperator arg -> Unary <$> mkExpr arg
        R.BinaryOperator lhs rhs -> Binary <$> mkExpr lhs <*> mkExpr rhs
        R.SelectedName base name -> Field <$> mkExpr base <*> pure name
        R.MultiAssignmentPattern rep es ->
            AssignPat <$> mkExpr rep <*> mapM mkExpr es
        _ -> trace ("unknown expression at " ++ show i) $ return UnknownExpr
        --_ -> error $ "expected expression at " ++ show i

mkIndex :: NodeId -> FromRawM Index
mkIndex i = do
    n <- getNode i
    case n of
        R.Range l (Just r) -> IRange <$> mkExpr l <*> mkExpr r
        R.Range l Nothing -> ISingle <$> mkExpr l
        _ -> ISingle <$> mkExpr i


fromRaw :: Map NodeId Node -> [NodeId] -> Design
fromRaw nodes modIds = evalState (mkDesign modIds) $
    S nodes (Numbering M.empty S.empty) (Numbering M.empty S.empty)
