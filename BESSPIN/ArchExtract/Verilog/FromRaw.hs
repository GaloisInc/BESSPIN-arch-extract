module BESSPIN.ArchExtract.Verilog.FromRaw where

import Control.Monad.State
import Data.Foldable
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
    { nodeMap :: Map NodeId (Node, Span)
    , blackboxModules :: Set Text
    , modules :: Numbering Module
    , decls :: Numbering Decl
    }

type FromRawM a = State S a

getNode :: NodeId -> FromRawM (Node, Span)
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
    (n, sp) <- getNode i
    case n of
        R.Module name ports params items -> do
            blackbox <- gets $ Set.member name . blackboxModules
            (partialMod, decls) <- collectDecls $ do
                ports <- S.fromList <$> mapM declRef ports
                params <- S.fromList <$> mapM declRef params
                items <- if not blackbox then
                        mconcat <$> mapM mkItems items
                    else
                        return S.empty
                return $ Module' name (error "decls not yet set") ports params items sp
            let externalParamSet = Set.fromList $ toList $ moduleParams partialMod
            let internalParams = flip S.foldMapWithIndex decls $ \idx decl ->
                    case decl of
                        ParamDecl _ _ _
                            | not $ Set.member idx externalParamSet -> S.singleton idx
                        _ -> S.empty
            return $ partialMod
                { moduleDecls = decls
                , moduleParams = moduleParams partialMod <> internalParams
                }
        _ -> error $ "expected module at " ++ show i

mkDecl :: NodeId -> FromRawM Decl
mkDecl i = do
    (n, sp) <- getNode i
    case n of
        R.Variable name optTy dims init (Just dir) ->
            PortDecl' <$> pure name <*> mkTy dims optTy <*> pure dir <*> pure sp
        R.Variable name optTy dims init Nothing ->
            VarDecl' <$> pure name <*> mkTy dims optTy <*> pure sp
        R.ParamId name optTy init dims ->
            ParamDecl' <$> pure name <*> mkTy dims optTy <*> mapM mkExpr init <*> pure sp
        R.InstId parent name conns -> do
            (n', _sp') <- getNode parent
            case n' of
                R.ModuleInstantiation mod params _ ->
                    InstDecl' <$> pure name <*> moduleRef mod
                        <*> (S.fromList <$> mapM (mapM mkExpr) params)
                        <*> pure sp
                _ -> error $ "expected module instantiation at " ++ show i
        R.TypeId name ty ->
            TypedefDecl' <$> pure name <*> mkTy Nothing (Just ty) <*> pure sp
        _ -> error $ "expected declaration at " ++ show i

mkTy :: Maybe NodeId -> Maybe NodeId -> FromRawM Ty
-- SV standard 1800-2012 sec. 6.10: implicitly declared nets default to scalar
-- (1-bit) type.
mkTy Nothing Nothing = return $ TTy' TWire [] [] dummySpan
mkTy (Just dims) Nothing = error $
    "impossible: decl has dims but no datatype? (dims = " ++ show dims ++ ")"
mkTy varDims (Just i) = do
    (n, sp) <- getNode i
    unpackedDims <- mkDims varDims
    case n of
        R.DataType ty _signed dims -> do
            packedDims <- mkDims dims
            return $ TTy' ty packedDims unpackedDims sp
        R.Enum ty _variants -> TEnum' <$> mkTy Nothing ty <*> pure sp
        R.TypeRef def -> TRef' <$> declRef def <*> pure sp
        _ -> error $ "expected datatype at " ++ show i

mkItems :: NodeId -> FromRawM (Seq Item)
mkItems i = do
    (n, sp) <- getNode i
    case n of
        R.DataDecl ids -> mconcat <$> mapM mkDeclItems ids
        R.NetDecl ids -> mconcat <$> mapM mkDeclItems ids
        R.ModuleInstantiation _ _ ids -> mconcat <$> mapM mkDeclItems ids
        R.ContinuousAssign assigns -> liftM S.fromList $
            forM assigns $ \i -> getNode i >>= \(n, sp) -> case n of
                R.NetRegAssign l r -> ContAssign' <$> mkExpr l <*> mkExpr r <*> pure sp
                _ -> error $ "expected NetRegAssign at " ++ show i
        R.AlwaysConstruct kind body -> getNode body >>= \(n', _sp') -> case n' of
            R.EventControlStatement evts s ->
                liftM S.singleton $ Always' <$> mapM mkEvent evts <*> mkStmts s <*> pure sp
            _ -> liftM S.singleton $ Always' <$> pure [] <*> mkStmts body <*> pure sp
        R.InitialConstruct body -> liftM S.singleton $ Initial' <$> mkStmts body <*> pure sp
        _ -> trace ("unknown mod item at " ++ show i) $ return S.empty

mkDeclItems :: NodeId -> FromRawM (Seq Item)
mkDeclItems i = do
    (n, sp) <- getNode i
    case n of
        R.Variable _ _ _ (Just init) _ ->
            liftM S.singleton $ InitVar' <$> declRef i <*> mkExpr init <*> pure sp
        R.Variable _ _ _ _ _ -> return S.empty
        R.ParamId _ _ (Just init) _ ->
            liftM S.singleton $ InitVar' <$> declRef i <*> mkExpr init <*> pure sp
        R.ParamId _ _ _ _ -> return S.empty
        R.TypeId _ _ -> declRef i >> return S.empty
        R.InstId _ _ portConns ->
            liftM S.singleton $ InitInst' <$> declRef i
                <*> (S.fromList <$> mapM mkExpr portConns)
                <*> pure sp
        _ -> error $ "expected item-like decl at " ++ show i

mkStmts :: NodeId -> FromRawM [Stmt]
mkStmts i = do
    (n, sp) <- getNode i
    case n of
        R.SeqBlock ds ss -> mapM_ declRef ds >> concat <$> mapM mkStmts ss
        R.EventControlStatement _ s -> mkStmts s
        R.ConditionalStatement cond then_ (Just else_) -> liftM pure $
            If' <$> mkExpr cond <*> mkStmts then_ <*> (Just <$> mkStmts else_) <*> pure sp
        R.ConditionalStatement cond then_ Nothing -> liftM pure $
            If' <$> mkExpr cond <*> mkStmts then_ <*> pure Nothing <*> pure sp
        R.CaseStatement cond cases -> liftM pure $
            Case' <$> mkExpr cond <*> mapM mkCase cases <*> pure sp
        R.For inits cond steps body -> liftM pure $
            For' <$> (concat <$> mapM mkStmts inits)
                 <*> mkExpr cond
                 <*> (concat <$> mapM mkStmts steps)
                 <*> mkStmts body
                 <*> pure sp
        R.NonBlockingAssign l r -> liftM pure $
            NonBlockingAssign' <$> mkExpr l <*> mkExpr r <*> pure sp
        R.BlockingAssign l r -> liftM pure $
            BlockingAssign' <$> mkExpr l <*> mkExpr r <*> pure sp
        R.BlockingAssignInPlace l -> liftM pure $
            BlockingUpdate' <$> mkExpr l <*> pure sp
        R.DelayControlStatement s -> mkStmts s
        R.NullStatement -> return []
        _ -> trace ("unknown statement at " ++ show i) $ return []
        --_ -> error $ "expected statement at " ++ show i

mkCase i = do
    (n, _sp) <- getNode i
    case n of
        R.CaseItem es body -> do
            a <- mapM mkExpr es
            b <- mkStmts body
            return (a, b)
        _ -> error $ "expected CaseItem at " ++ show i

mkExpr :: NodeId -> FromRawM Expr
mkExpr i = do
    (n, sp) <- getNode i
    case n of
        R.IdRef i -> do
            declId <- declRef i
            decl <- getDecl declId
            case decl of
                VarDecl {} -> return $ Var' declId sp
                PortDecl {} -> return $ Var' declId sp
                ParamDecl {} -> return $ Param' declId sp
                _ -> error $ "expected reference to var, port, or param at " ++ show i
        R.IndexedId base ix -> Index' <$> mkExpr base <*> mkIndex ix <*> pure sp
        R.IndexedMemoryId base ixs ->
            MemIndex' <$> mkExpr base <*> mapM mkIndex ixs <*> pure sp
        R.ConstVal t -> return $ Const' t sp
        R.IntVal t i
            | toInteger (minBound :: Int) <= i && i <= toInteger (maxBound :: Int) ->
                return $ ConstInt' t (fromInteger i) sp
            | otherwise -> return $ Const' t sp
        R.RealVal t -> return $ Const' t sp
        R.Concat es -> Concat' <$> mapM mkExpr es <*> pure sp
        R.MultiConcat rep es -> MultiConcat' <$> mkExpr rep <*> mapM mkExpr es <*> pure sp
        R.QuestionColon cond then_ else_ ->
            IfExpr' <$> mkExpr cond <*> mkExpr then_ <*> mkExpr else_ <*> pure sp
        R.UnaryOperator op arg -> Unary' op <$> mkExpr arg <*> pure sp
        R.BinaryOperator op lhs rhs -> Binary' op <$> mkExpr lhs <*> mkExpr rhs <*> pure sp
        R.SelectedName base name -> Field' <$> mkExpr base <*> pure name <*> pure sp
        R.MultiAssignmentPattern rep es ->
            AssignPat' <$> mkExpr rep <*> mapM mkExpr es <*> pure sp
        R.SystemFunctionCall name es -> case T.unpack name of
            "clog2" -> Builtin' BkClog2 <$> mapM mkExpr es <*> pure sp
            "size" -> Builtin' BkSize <$> mapM mkExpr es <*> pure sp
            _ -> trace ("unknown system function call at " ++ show i) $
                return $ UnknownExpr' sp
        _ -> trace ("unknown expression at " ++ show i) $ return $ UnknownExpr' sp
        --_ -> error $ "expected expression at " ++ show i

mkExprVar :: NodeId -> FromRawM Int
mkExprVar i = mkExpr i >>= \e -> case e of
    Var declId -> return declId
    _ -> error $ "expected Var expression at " ++ show i

mkEvent :: NodeId -> FromRawM Event
mkEvent i = do
    (n, sp) <- getNode i
    case n of
        R.EventExpression optEdge e ->
            Event' <$> pure optEdge <*> mkExprVar e <*> pure sp
        _ -> error $ "expected EventExpression at " ++ show i

mkRange :: NodeId -> FromRawM (Range, Maybe NodeId)
mkRange i = do
    (n, sp) <- getNode i
    case n of
        -- Special case - [$] shows up in some testbenches.  For now we encode
        -- it as `Range UnknownExpr UnknownExpr`.
        R.Range l Nothing next -> do
            (n', sp') <- getNode l
            case n' of
                R.Dollar -> return (Range (UnknownExpr' sp') (UnknownExpr' sp'), next)
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
    (n, _sp) <- getNode i
    case n of
        R.Range _ _ (Just _) -> error $ "unexpected nested ranges in expr at " ++ show i
        R.Range l (Just r) _ -> IRange <$> mkExpr l <*> mkExpr r
        R.Range l Nothing _ -> ISingle <$> mkExpr l
        _ -> ISingle <$> mkExpr i


fromRaw :: Map NodeId (Node, Span) -> Set Text -> [NodeId] -> Design
fromRaw nodes blackboxModules modIds = evalState (mkDesign modIds) $
    S nodes blackboxModules (Numbering M.empty S.empty) (Numbering M.empty S.empty)
