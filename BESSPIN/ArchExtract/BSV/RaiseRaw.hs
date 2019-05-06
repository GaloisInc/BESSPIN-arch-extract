{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module BESSPIN.ArchExtract.BSV.RaiseRaw where

import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.Generics
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro.Platform

import BESSPIN.ArchExtract.BSV.Raw
import BESSPIN.ArchExtract.BSV.PrintRaw
import BESSPIN.ArchExtract.GraphOps

import Debug.Trace


-- This is generally meant to be used on the full [Package].  It will run on
-- other parts of the AST, but functionality may be limited.
raiseRaw :: Data a => a -> a
raiseRaw x =
    convertTypeclassAccessors $
    postSimplify $
    --removeTcDicts $
    rewrite $
    preSimplify $
    reconstructAllLets $
    --cleanDefs $
    convertTypedefs $
    prefixNames $
    x

prefixNames :: Data a => a -> a
prefixNames x = everywhere (mkT goPackage) x
  where
    goPackage (Package i imps ds ss ts) =
        let pkgName = idName i in
        Package i imps
            (fmap (goDef pkgName) ds)
            (fmap (goStruct pkgName) ss)
            (fmap (goTypedef pkgName) ts)

    goDef pkgName d@(Def { defId = Id name l c })
      | (pkgName <> ".") `T.isPrefixOf` name && "~" `T.isInfixOf` name = d
      | otherwise = d { defId = Id (pkgName <> "." <> name) l c }

    goStruct pkgName s@(Struct { structId = Id name l c })
      | otherwise = s { structId = Id (pkgName <> "." <> name) l c }

    goTypedef pkgName t@(Typedef { typedefId = Id name l c })
      | otherwise = t { typedefId = Id (pkgName <> "." <> name) l c }

cleanDefs :: Data a => a -> a
cleanDefs x = everywhere (mkT goPackage) x
  where
    goPackage (Package i imps ds ss ts) = Package i imps (S.filter checkDef ds) ss ts
    checkDef (Def (Id t _ _) _ _) = not $ "Prelude.Prim" `T.isInfixOf` t

-- At the level where we're operating, typeclass definitions have already been
-- converted into structs, but the accessors are still `EVar "Foo.f"` instead
-- of `EStatic "Foo.Tc" "Foo.f"`.  This pass does the conversion.
convertTypeclassAccessors :: Data a => a -> a
convertTypeclassAccessors x = everywhere (mkT go) x
  where
    go (EVar i@(Id name _ _)) | Just sId <- M.lookup name fieldMap = EStatic sId i
    go e = e

    fieldMap = everything (<>) (M.empty `mkQ` gather) x
    gather (Struct sId _ fs _) = M.fromList $ map (gatherField sId) fs
    gatherField sId (Field fId _ _) =
        let (pkgPrefix, _) = T.breakOnEnd "." $ idName sId in
        (pkgPrefix <> idName fId, sId)

rewrite :: Data a => a -> a
rewrite x = everywhere (mkT goExpr `extT` goTy) x
  where
    -- Monad handling
    goExpr (EApp (EVar (Id "Prelude.bind" _ _)) [_tM, tA, _tB] [_dct, m, ELam [p] k]) =
        goExpr $ EDo [SBind p tA m 0] k
    goExpr (EApp (EVar (Id "Prelude.bind_" _ _)) _tys [_dct, m, k]) =
        goExpr $ EDo [SBind' m 0] k
    goExpr (EDo ss1 (EDo ss2 e)) =
        goExpr $ EDo (ss1 ++ ss2) e

    -- Uninteresting prelude functions
    goExpr (EApp (EVar (Id "Prelude.forceIsModule" _ _)) _tys [_dct, e]) = goExpr e
    goExpr (EApp (EVar (Id "Prelude.fromInteger" _ _)) [_] [_dct, e]) = goExpr e
    goExpr (EApp (EVar (Id "Prelude.fromSizedInteger" _ _)) [_, _] [_dct, e]) = goExpr e
    goExpr (EApp (EVar (Id "Prelude.fromString" _ _)) [_] [_dct, e]) = goExpr e

    -- Baked-in primitives
    goExpr (EApp (EVar (Id "Prelude.setStateName" _ _)) _tys [_dct, nameExpr, val])
      | EApp (EVar (Id "Prelude.primGetName" _ _)) [] [nameArg] <- nameExpr
      , EVar (Id name _ _) <- nameArg
      = EApp (EPrim $ PSetName name) [] [val]
    goExpr (EApp (EVar (Id "Prelude.return" _ _)) _tys [_dct, x]) =
        EApp (EPrim PReturn) [] [x]
    goExpr (EApp (EVar (Id "Prelude.mkReg" _ _)) [elemTy, elemWidth, _, _] [_dct1, _dct2, init]) =
        EApp (EPrim PMkReg) [elemTy, elemWidth] [init]
    goExpr (EApp (EVar (Id "Prelude.mkRegU" _ _)) [elemTy, elemWidth, _, _] [_dct1, _dct2]) =
        EApp (EPrim PMkRegU) [elemTy, elemWidth] []
    goExpr (EApp (EVar (Id "Prelude.addRules" _ _)) [_, _] [_dct, ERules rs])
      | Just rs' <- mapM convRule rs = EAddRules rs'
    goExpr (EApp (EVar (Id "Prelude.pack" _ _)) [_, _] [_d1, e]) =
        EApp (EPrim PPack) [] [e]
    goExpr (EApp (EVar (Id "Prelude.unpack" _ _)) [_, _] [_d1, e]) =
        EApp (EPrim PUnpack) [] [e]
    goExpr (EApp (EVar (Id "Prelude.truncate" _ _)) [m, _n, _] [_d1, e]) =
        EApp (EPrim $ PResize "truncate") [m] [e]
    goExpr (EApp (EVar (Id "Prelude.extend" _ _)) [_m, n, _] [_d1, e]) =
        EApp (EPrim $ PResize "extend") [n] [e]
    goExpr (EApp (EVar (Id "Prelude.signExtend" _ _)) [_m, n, _] [_d1, e]) =
        EApp (EPrim $ PResize "signExtend") [n] [e]
    goExpr (EApp (EVar (Id "Prelude.zeroExtend" _ _)) [_m, n, _] [_d1, e]) =
        EApp (EPrim $ PResize "zeroExtend") [n] [e]
    goExpr (EApp (EVar (Id "Prelude.primSelectFn" _ _))
            [_tIn, _tOut, _tIdx, _tUnk] [_d1, _d2, _pos, e, idx]) =
        EApp (EPrim PIndex) [] [e, idx]
    goExpr (EApp (EVar (Id "Prelude.primExtract" _ _))
            [_tIdx, _tIn, _tArr, _tOut] [_dct, _pos, e, hi, lo]) =
        EApp (EPrim PSlice) [] [e, hi, lo]
    goExpr (EApp (EVar (Id "Prelude._if" _ _)) [ty] [c, t, e]) =
        EApp (EPrim PIf) [ty] [c, t, e]
    goExpr (EApp (EStatic (Id "Prelude.Reg" _ _) (Id "Prelude._read" _ _)) [_] [e]) =
        EApp (EPrim PRegRead) [] [e]
    goExpr (EApp (EStatic (Id "Prelude.Reg" _ _) (Id "Prelude._write" _ _)) [_] [l, r]) =
        EApp (EPrim PRegWrite) [] [l, r]

    goExpr (EApp (EVar (Id "Prelude.primValueOf" _ _)) [_] [_]) = EConst "primValueOf"
    goExpr (EVar (Id "Prelude.constantWithAllBitsSet" _ _)) = EConst "constantWithAllBitsSet"
    goExpr (EVar (Id "Prelude.constantWithAllBitsUnset" _ _)) = EConst "constantWithAllBitsUnset"

    goExpr (EApp (EVar (Id "Assert.staticAssert" _ _)) [_, _] [_dct, _, _]) =
        buildLambda [PWild, PWild] (primApp PReturn [] [EConst "staticAssert"])
    goExpr (EApp (EVar (Id "Assert.dynamicAssert" _ _)) [_, _] [_dct, _, _]) =
        buildLambda [PWild, PWild] (primApp PReturn [] [EConst "dynamicAssert"])

    -- Unary and binary ops
    goExpr (EApp (EVar (Id "Prelude.+" _ _)) [_] [_d1, l, r]) = binOp "+" l r
    goExpr (EApp (EVar (Id "Prelude.-" _ _)) [_] [_d1, l, r]) = binOp "-" l r

    goExpr (EApp (EVar (Id "Prelude.==" _ _)) [_] [_d1, l, r]) = binOp "==" l r
    goExpr (EApp (EVar (Id "Prelude./=" _ _)) [_] [_d1, l, r]) = binOp "/=" l r
    goExpr (EApp (EVar (Id "Prelude.<" _ _)) [_] [_d1, l, r]) = binOp "<" l r
    goExpr (EApp (EVar (Id "Prelude.<=" _ _)) [_] [_d1, l, r]) = binOp "<=" l r
    goExpr (EApp (EVar (Id "Prelude.>" _ _)) [_] [_d1, l, r]) = binOp ">" l r
    goExpr (EApp (EVar (Id "Prelude.>=" _ _)) [_] [_d1, l, r]) = binOp ">=" l r

    goExpr (EApp (EVar (Id "Prelude.&" _ _)) [_] [_d1, l, r]) = binOp "&" l r
    goExpr (EApp (EVar (Id "Prelude.|" _ _)) [_] [_d1, l, r]) = binOp "|" l r
    goExpr (EApp (EVar (Id "Prelude.^" _ _)) [_] [_d1, l, r]) = binOp "^" l r
    goExpr (EApp (EVar (Id "Prelude.~^" _ _)) [_] [_d1, l, r]) = binOp "~^" l r
    -- NB: ~^ and ^~ are synonyms - we normalize to ~^
    goExpr (EApp (EVar (Id "Prelude.^~" _ _)) [_] [_d1, l, r]) = binOp "~^" l r
    goExpr (EApp (EVar (Id "Prelude.invert" _ _)) [_] [_d1, e]) = unOp "invert" e
    goExpr (EApp (EVar (Id "Prelude.<<" _ _)) [_, _, _] [_d1, _d2, l, r]) = binOp "<<" l r
    goExpr (EApp (EVar (Id "Prelude.>>" _ _)) [_, _, _] [_d1, _d2, l, r]) = binOp ">>" l r
    goExpr (EApp (EVar (Id "Prelude.msb" _ _)) [_] [_d1, e]) = unOp "msb" e
    goExpr (EApp (EVar (Id "Prelude.lsb" _ _)) [_] [_d1, e]) = unOp "lsb" e

    goExpr (EApp (EVar (Id "Prelude.reduceAnd" _ _)) [_, _] [_d1, e]) = unOp "reduceAnd" e
    goExpr (EApp (EVar (Id "Prelude.reduceOr" _ _)) [_, _] [_d1, e]) = unOp "reduceOr" e
    goExpr (EApp (EVar (Id "Prelude.reduceXor" _ _)) [_, _] [_d1, e]) = unOp "reduceXor" e
    goExpr (EApp (EVar (Id "Prelude.reduceNand" _ _)) [_, _] [_d1, e]) = unOp "reduceNand" e
    goExpr (EApp (EVar (Id "Prelude.reduceNor" _ _)) [_, _] [_d1, e]) = unOp "reduceNor" e
    goExpr (EApp (EVar (Id "Prelude.reduceXnor" _ _)) [_, _] [_d1, e]) = unOp "reduceXnor" e

    goExpr (EApp (EVar (Id "Prelude.&&" _ _)) [] [l, r]) = binOp "&&" l r
    goExpr (EApp (EVar (Id "Prelude.||" _ _)) [] [l, r]) = binOp "||" l r
    goExpr (EApp (EVar (Id "Prelude.not" _ _)) [] [e]) = unOp "not" e

    goExpr e = e

    goTy (TCon (Id "Prelude.Unit" _ _)) = TUnit
    goTy (TCon (Id "Prelude.Bool" _ _)) = TBool
    goTy (TApp (TIfc (Id "Prelude.Reg" _ _)) [t]) = TReg t
    goTy (TApp (TCon (Id "Prelude.Bit" _ _)) [t]) = TBit t
    goTy (TApp (TCon (Id "Prelude.Module" _ _)) [t]) = TModule t
    goTy (TCon (Id "Prelude.Action" _ _)) = TAction TUnit
    goTy (TApp (TCon (Id "Prelude.ActionValue" _ _)) [t]) = TAction t
    goTy (TApp (TCon (Id "Prelude.IsModule" _ _)) [t1, t2]) = TIsModule t1 t2
    goTy t = t

    primApp p tys args = EApp (EPrim p) tys args

    -- Convert a `RawRule` into a `Rule` suitable for use in `EAddRules`.
    convRule :: RawRule -> Maybe Rule
    convRule (RrRule optNameExpr guards body) = do
        optName' <- case optNameExpr of
            Nothing -> return Nothing
            Just (ELit (LStr s)) -> return (Just s)
            _ -> Nothing
        conds' <- forM guards $ \g -> case g of
            GCond e -> return e
            _ -> Nothing
        body' <- case body of
            EApp (EVar (Id "Prelude.toPrimAction" _ _)) [_] [e] -> return e
            _ -> Nothing
        return $ Rule optName' conds' body'
    convRule _ = Nothing

    unOp op a = EApp (EPrim $ PUnOp op) [] [a]
    binOp op a b = EApp (EPrim $ PBinOp op) [] [a, b]

preSimplify :: Data a => a -> a
preSimplify x = everywhere (mkT goExpr `extT` goTy) x
  where
    goTy (TApp (TApp a bs) cs) = goTy $ TApp a (bs ++ cs)
    goTy (TApp (TCon (Id "Prelude.->" _ _)) [x, y]) = goTy $ TArrow x y
    goTy t = t

    goExpr (EApp (EApp f tys1 args1) [] args2) = goExpr $ EApp f tys1 (args1 ++ args2)
    goExpr (ELet (Def i ty [Clause [] [] body]) (EVar i') _ _)
      | i == i' = goExpr $ body
    goExpr (ELet (Def i ty [Clause pats [] body]) (EVar i') _ _)
      | not $ null pats, i == i' = goExpr $ ELam pats body
    goExpr e = e

postSimplify :: Data a => a -> a
postSimplify x = x --everywhere (mkT goExpr) x

removeTcDicts :: Data a => a -> a
removeTcDicts x = everywhere (mkT goExpr `extT` goDefs `extT` goPat) x
  where
    goExpr (EVar (Id t _ _)) | "_tcdict" `T.isPrefixOf` t = ETcDict
    goExpr (ELet (Def (Id t _ _) _ _) body _ _) | "_tcdict" `T.isPrefixOf` t = body
    goExpr e = e

    goDefs (Def (Id t _ _) _ _ : rest) | "_tcdict" `T.isPrefixOf` t = rest
    goDefs ds = ds

    goPat (PVar (Id t _ _)) | "_tcdict" `T.isPrefixOf` t = PTcDict
    goPat p = p

-- Analyze all lets and letrecs, and rebuild them in simplified form.
--
--  * Moves bindings from `letrec`s into separate `let`s if they are not
--    actually recursive.
--  * Breaks up recursive `letrec`s into their strongly-connected components.
--  * Removes definitions whose values are never used.
--  * Topologically sorts bindings.  In particular, in `let ... in x`, the
--    binding for `x` is always in the most deeply nested let(rec).
reconstructAllLets :: Data a => a -> a
reconstructAllLets x = everywhere (mkT go) x
  where
    go :: Expr -> Expr
    go (ELetRec ds body) = reconstructLet ds body
    go (ELet d body _ _) = reconstructLet [d] body
    go e = e

reconstructLet :: [Def] -> Expr -> Expr
reconstructLet dsList body =
    -- `tail` skips over the `sccOrder` entry that represents the root/body.
    foldl (flip buildScc) body (tail $ toList sccOrder)
  where
    ds = S.fromList dsList

    -- The Def where each Id is defined
    idDef :: Map Text Int
    idDef = S.foldMapWithIndex (\idx d -> M.singleton (idName $ defId d) idx) ds

    idsToDefs :: Set Text -> Set Int
    idsToDefs is = Set.map (\i -> idDef M.! i) $
        Set.filter (\i -> M.member i idDef) is

    collectIds :: Data a => a -> Set Text
    collectIds x = everything (<>) (Set.empty `mkQ` go) x
      where go (Id name _ _) = Set.singleton name

    -- For each def, the indices of other defs referenced from this one.
    defEdges :: Seq (Set Int)
    defEdges = fmap (idsToDefs . collectIds . defClauses) ds

    -- The indices of all defs referenced by the body.
    bodyEdges :: Set Int
    bodyEdges = idsToDefs $ collectIds body

    -- Strongly-connected components of the def graph, represented as sets
    -- of vertex indices.  The first SCC is empty, representing the body.
    sccs :: Seq (Set Int)
    sccs = S.fromList $
        Set.empty : labelSccs (\i -> Set.toList $ defEdges `S.index` i) (S.length ds)

    -- SCC index of each def.
    defScc :: Seq Int
    defScc = flip execState (S.fromFunction (S.length ds) (const (-1))) $ do
        flip S.traverseWithIndex sccs $ \scc ds ->
            forM_ (Set.toList ds) $ \d ->
                ix d .= scc

    -- SCC graph edges
    sccEdges :: Seq (Set Int)
    sccEdges = flip execState (S.fromFunction (S.length sccs) (const Set.empty)) $ do
        flip S.traverseWithIndex defEdges $ \src dests -> do
            let srcScc = defScc `S.index` src
            forM_ (Set.toList dests) $ \dest -> do
                let destScc = defScc `S.index` dest
                ix srcScc %= Set.insert destScc

        forM_ (Set.toList bodyEdges) $ \dest -> do
            let destScc = defScc `S.index` dest
            ix 0 %= Set.insert destScc

    -- SCC order, starting at the root node.
    sccOrder :: Seq Int
    sccOrder = topoSort (\i -> Set.toList $ sccEdges `S.index` i) 0

    buildScc :: Int -> Expr -> Expr
    buildScc scc body
      | Set.size dIdxs == 1, not selfRec = ELet (ds `S.index` dIdx) body 0 []
      | otherwise = ELetRec (map (\dIdx -> ds `S.index` dIdx) $ Set.toList dIdxs) body
      where
        dIdxs = sccs `S.index` scc
        dIdx = Set.findMin dIdxs
        selfRec = Set.member scc $ sccEdges `S.index` scc


convertTypedefs :: Data a => a -> a
convertTypedefs x = everywhere (mkT go) x
  where
    tdMap = everything (<>) (M.empty `mkQ` go) x
      where
        go (Typedef i [] body) = M.singleton (idName i) body
        go (Typedef i params body) = M.singleton (idName i) (TLam params body)

    go (TCon i) | Just def <- M.lookup (idName i) tdMap = TAlias i def
    go t = t


lastMaybe [] = Nothing
lastMaybe xs = Just $ last xs

usesVar i e = everything (||) (False `mkQ` go) e
  where
    go (EVar i') = i == i'
    go _ = False
