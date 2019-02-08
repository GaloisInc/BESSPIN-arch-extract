module BESSPIN.ArchExtract.Verilog.FromRaw where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import BESSPIN.ArchExtract.Verilog.AST
import BESSPIN.ArchExtract.Verilog.Raw (NodeId, Node)
import qualified BESSPIN.ArchExtract.Verilog.Raw as R

import Debug.Trace


data Numbering v = Numbering (Map NodeId Int) (Seq v)

data S = S
    { nodeMap :: Map NodeId Node
    , blackboxModules :: Set Text
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

getModule :: Int -> FromRawM Module
getModule i = do
    Numbering _ xs <- gets modules
    return $ xs `S.index` i

getDecl :: Int -> FromRawM Decl
getDecl i = do
    Numbering _ xs <- gets decls
    return $ xs `S.index` i

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


mkDesign :: [NodeId] -> FromRawM Design
mkDesign is = do
    ((), mods) <- collectModules $ mapM_ moduleRef is
    return $ Design mods

mkModule :: NodeId -> FromRawM Module
mkModule i = do
    n <- getNode i
    case n of
        R.Module name ports params items -> do
            blackbox <- gets $ Set.member name . blackboxModules
            (items, decls) <- collectDecls $ do
                mapM_ declRef ports
                mapM_ declRef params
                if not blackbox then 
                    mconcat <$> mapM mkItems items
                else
                    return S.empty
            let ports = S.foldMapWithIndex (\i d -> case d of
                    PortDecl _ _ _ -> S.singleton i
                    _ -> S.empty) decls
            let params = S.foldMapWithIndex (\i d -> case d of
                    ParamDecl _ _ _ -> S.singleton i
                    _ -> S.empty) decls
            return $ Module name decls ports params items
        _ -> error $ "expected module at " ++ show i

mkDecl :: NodeId -> FromRawM Decl
mkDecl i = do
    n <- getNode i
    case n of
        R.Variable name optTy dims init (Just dir) ->
            PortDecl <$> pure name <*> mkTy dims optTy <*> pure dir
        R.Variable name optTy dims init Nothing ->
            VarDecl <$> pure name <*> mkTy dims optTy
        R.ParamId name optTy init dims ->
            ParamDecl <$> pure name <*> mkTy dims optTy <*> mapM mkExpr init
        R.InstId parent name conns -> do
            n' <- getNode parent
            case n' of
                R.ModuleInstantiation mod params _ ->
                    InstDecl <$> pure name <*> moduleRef mod <*> mapM (mapM mkExpr) params
                _ -> error $ "expected module instantiation at " ++ show i
        R.TypeId name ty ->
            TypedefDecl <$> pure name <*> mkTy Nothing (Just ty)
        _ -> error $ "expected declaration at " ++ show i

mkTy :: Maybe NodeId -> Maybe NodeId -> FromRawM Ty
-- SV standard 1800-2012 sec. 6.10: implicitly declared nets default to scalar
-- (1-bit) type.
mkTy Nothing Nothing = return $ TTy TWire [] []
mkTy (Just dims) Nothing = error $
    "impossible: decl has dims but no datatype? (dims = " ++ show dims ++ ")"
mkTy varDims (Just i) = do
    n <- getNode i
    unpackedDims <- mkDims varDims
    case n of
        R.DataType ty _signed dims -> do
            packedDims <- mkDims dims
            return $ TTy ty packedDims unpackedDims
        R.Enum ty _variants -> TEnum <$> mkTy Nothing ty
        R.TypeRef def -> TRef <$> declRef def
        _ -> error $ "expected datatype at " ++ show i

mkItems :: NodeId -> FromRawM (Seq Item)
mkItems i = do
    n <- getNode i
    case n of
        R.DataDecl ids -> mconcat <$> mapM mkDeclItems ids
        R.NetDecl ids -> mconcat <$> mapM mkDeclItems ids
        R.ModuleInstantiation _ _ ids -> mconcat <$> mapM mkDeclItems ids
        R.ContinuousAssign assigns -> liftM S.fromList $
            forM assigns $ \i -> getNode i >>= \n -> case n of
                R.NetRegAssign l r -> ContAssign <$> mkExpr l <*> mkExpr r
                _ -> error $ "expected NetRegAssign at " ++ show i
        R.AlwaysConstruct kind body -> getNode body >>= \n' -> case n' of
            R.EventControlStatement evts s ->
                liftM S.singleton $ Always <$> mapM mkEvent evts <*> mkStmts s
            _ -> liftM S.singleton $ Always <$> pure [] <*> mkStmts body
        R.InitialConstruct body -> liftM S.singleton $ Initial <$> mkStmts body
        _ -> trace ("unknown mod item at " ++ show i) $ return S.empty

mkDeclItems :: NodeId -> FromRawM (Seq Item)
mkDeclItems i = do
    n <- getNode i
    case n of
        R.Variable _ _ _ (Just init) _ ->
            liftM S.singleton $ InitVar <$> declRef i <*> mkExpr init
        R.Variable _ _ _ _ _ -> return S.empty
        R.ParamId _ _ (Just init) _ ->
            liftM S.singleton $ InitVar <$> declRef i <*> mkExpr init
        R.ParamId _ _ _ _ -> return S.empty
        R.TypeId _ _ -> declRef i >> return S.empty
        R.InstId _ _ portConns ->
            liftM S.singleton $ InitInst <$> declRef i <*> mapM mkExpr portConns
        _ -> error $ "expected item-like decl at " ++ show i

mkStmts :: NodeId -> FromRawM [Stmt]
mkStmts i = do
    n <- getNode i
    case n of
        R.SeqBlock ds ss -> mapM_ declRef ds >> concat <$> mapM mkStmts ss
        R.EventControlStatement _ s -> mkStmts s
        R.ConditionalStatement cond then_ (Just else_) -> liftM pure $
            If <$> mkExpr cond <*> mkStmts then_ <*> (Just <$> mkStmts else_)
        R.ConditionalStatement cond then_ Nothing -> liftM pure $
            If <$> mkExpr cond <*> mkStmts then_ <*> pure Nothing
        R.CaseStatement cond cases -> liftM pure $
            Case <$> mkExpr cond <*> mapM mkCase cases
        R.For inits cond steps body -> liftM pure $
            For <$> (concat <$> mapM mkStmts inits)
                <*> mkExpr cond
                <*> (concat <$> mapM mkStmts steps)
                <*> mkStmts body
        R.NonBlockingAssign l r -> liftM pure $
            NonBlockingAssign <$> mkExpr l <*> mkExpr r
        R.BlockingAssign l r -> liftM pure $
            BlockingAssign <$> mkExpr l <*> mkExpr r
        R.BlockingAssignInPlace l -> liftM pure $
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
        R.IdRef i -> do
            declId <- declRef i
            decl <- getDecl declId
            case decl of
                VarDecl {} -> return $ Var declId
                PortDecl {} -> return $ Var declId
                ParamDecl {} -> return $ Param declId
                _ -> error $ "expected reference to var, port, or param at " ++ show i
        R.IndexedId base ix -> Index <$> mkExpr base <*> mkIndex ix
        R.IndexedMemoryId base ixs -> MemIndex <$> mkExpr base <*> mapM mkIndex ixs
        R.ConstVal t -> return $ Const t
        R.IntVal t i
            | toInteger (minBound :: Int) <= i && i <= toInteger (maxBound :: Int) ->
                return $ ConstInt t (fromInteger i)
            | otherwise -> return $ Const t
        R.RealVal t -> return $ Const t
        R.Concat es -> Concat <$> mapM mkExpr es
        R.MultiConcat rep es -> MultiConcat <$> mkExpr rep <*> mapM mkExpr es
        R.QuestionColon cond then_ else_ ->
            IfExpr <$> mkExpr cond <*> mkExpr then_ <*> mkExpr else_
        R.UnaryOperator op arg -> Unary op <$> mkExpr arg
        R.BinaryOperator op lhs rhs -> Binary op <$> mkExpr lhs <*> mkExpr rhs
        R.SelectedName base name -> Field <$> mkExpr base <*> pure name
        R.MultiAssignmentPattern rep es ->
            AssignPat <$> mkExpr rep <*> mapM mkExpr es
        R.SystemFunctionCall name es -> case T.unpack name of
            "clog2" -> Builtin BkClog2 <$> mapM mkExpr es
            _ -> trace ("unknown system function call at " ++ show i) $
                return UnknownExpr
        _ -> trace ("unknown expression at " ++ show i) $ return UnknownExpr
        --_ -> error $ "expected expression at " ++ show i

mkExprVar :: NodeId -> FromRawM Int
mkExprVar i = mkExpr i >>= \e -> case e of
    Var declId -> return declId
    _ -> error $ "expected Var expression at " ++ show i

mkEvent :: NodeId -> FromRawM Event
mkEvent i = do
    n <- getNode i
    case n of
        R.EventExpression optEdge e ->
            Event <$> pure optEdge <*> mkExprVar e
        _ -> error $ "expected EventExpression at " ++ show i

mkRange :: NodeId -> FromRawM (Range, Maybe NodeId)
mkRange i = do
    n <- getNode i
    case n of
        -- Special case - [$] shows up in some testbenches.  For now we encode
        -- it as `Range UnknownExpr UnknownExpr`.
        R.Range l Nothing next -> do
            n' <- getNode l
            case n' of
                R.Dollar -> return (Range UnknownExpr UnknownExpr, next)
                _ -> error $ "expected double-ended Range or [$] at " ++ show i
        R.Range l (Just r) next -> do
            range <- Range <$> mkExpr l <*> mkExpr r
            return (range, next)
        _ -> error $ "expected double-ended Range or [$] at " ++ show i

mkDims :: Maybe NodeId -> FromRawM [Range]
mkDims Nothing = return []
mkDims (Just i) = do
    (r, next) <- mkRange i
    rs <- mkDims next
    return $ r : rs

mkIndex :: NodeId -> FromRawM Index
mkIndex i = do
    n <- getNode i
    case n of
        R.Range _ _ (Just _) -> error $ "unexpected nested ranges in expr at " ++ show i
        R.Range l (Just r) _ -> IRange <$> mkExpr l <*> mkExpr r
        R.Range l Nothing _ -> ISingle <$> mkExpr l
        _ -> ISingle <$> mkExpr i


fromRaw :: Map NodeId Node -> Set Text -> [NodeId] -> Design
fromRaw nodes blackboxModules modIds = evalState (mkDesign modIds) $
    S nodes blackboxModules (Numbering M.empty S.empty) (Numbering M.empty S.empty)
