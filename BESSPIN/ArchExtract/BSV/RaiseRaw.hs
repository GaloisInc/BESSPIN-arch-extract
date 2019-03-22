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


raiseRaw :: Data a => a -> a
raiseRaw x =
    postSimplify $
    removeTcDicts $
    rewrite $
    preSimplify $
    reconstructAllLets $
    x

rewrite :: Data a => a -> a
rewrite x = everywhere (mkT goExpr) x
  where
    -- Monad handling
    goExpr (EApp (EVar (Id "Prelude.bind" _ _)) _tys [_dct, m, ELam [p] k]) =
        goExpr $ EDo [SBind p m] k
    goExpr (EApp (EVar (Id "Prelude.bind_" _ _)) _tys [_dct, m, k]) =
        goExpr $ EDo [SBind' m] k
    goExpr (EDo ss1 (EDo ss2 e)) =
        goExpr $ EDo (ss1 ++ ss2) e
    -- Uninteresting prelude functions
    goExpr (EApp (EVar (Id "Prelude.setStateName" _ _)) _tys [_dct, _name, val]) =
        goExpr val
    goExpr (EApp (EVar (Id "Prelude.forceIsModule" _ _)) _tys [_dct, e]) = goExpr e
    goExpr (EApp (EVar (Id "Prelude.fromInteger" _ _)) [_] [_dct, e]) = goExpr e
    -- Baked-in primitives
    goExpr (EApp (EVar (Id "Prelude.return" _ _)) _tys [_dct, x]) =
        goExpr $ EApp (EPrim PReturn) [] [x]
    goExpr (EApp (EVar (Id "Prelude.mkRegU" _ _)) [elemTy, elemWidth, _, _] [_dct1, _dct2]) =
        goExpr $ EApp (EPrim PMkRegU) [elemTy, elemWidth] []
    goExpr (EApp (EVar (Id "Prelude.addRules" _ _)) [_, _] [_dct, ERules rs])
      | Just rs' <- mapM convRule rs = goExpr $ EAddRules rs'
    goExpr (EApp (EVar (Id "Prelude.pack" _ _)) [_, _] [_d1, e]) =
        goExpr $ EApp (EPrim PPack) [] [e]
    goExpr (EApp (EVar (Id "Prelude.unpack" _ _)) [_, _] [_d1, e]) =
        goExpr $ EApp (EPrim PUnpack) [] [e]
    goExpr (EApp (EStatic (Id "Prelude.Reg" _ _) (Id "Prelude._read" _ _)) [_] [e]) =
        ERegRead e
    goExpr (EApp (EStatic (Id "Prelude.Reg" _ _) (Id "Prelude._write" _ _)) [_] [l, r]) =
        ERegWrite l r

    -- Binary ops
    goExpr (EApp (EVar (Id "Prelude.+" _ _)) [_] [_d1, l, r]) = EBinOp "+" l r
    goExpr (EApp (EVar (Id "Prelude.-" _ _)) [_] [_d1, l, r]) = EBinOp "-" l r
    goExpr (EApp (EVar (Id "Prelude.==" _ _)) [_] [_d1, l, r]) = EBinOp "==" l r
    goExpr (EApp (EVar (Id "Prelude.<<" _ _)) [_, _, _] [_d1, _d2, l, r]) = EBinOp "<<" l r
    goExpr (EApp (EVar (Id "Prelude.>>" _ _)) [_, _, _] [_d1, _d2, l, r]) = EBinOp ">>" l r

    goExpr e = e

    -- Convert a `Rule` into an entry suitable for use in `EAddRules`.
    convRule :: Rule -> Maybe (Maybe Text, Expr)
    convRule (RRule optNameExpr body) = do
        optNameExpr' <- matchNameExpr optNameExpr
        body' <- matchBody body
        return (optNameExpr', body')
      where
        matchNameExpr Nothing = Just Nothing
        matchNameExpr (Just (EApp (EVar (Id "Prelude.fromString" _ _)) [_] [_dct, lit]))
          | ELit (LStr s) <- lit = Just (Just s)
        matchNameExpr _ = Nothing

        matchBody (EApp (EVar (Id "Prelude.toPrimAction" _ _)) [_] [e]) = Just e
        matchBody _ = Nothing
    convRule _ = Nothing

preSimplify :: Data a => a -> a
preSimplify x = everywhere (mkT goExpr `extT` goTy) x
  where
    goTy (TApp (TApp a bs) cs) = goTy $ TApp a (bs ++ cs)
    goTy (TApp (TCon (Id "Prelude.->" _ _)) [x, y]) = goTy $ TArrow x y
    goTy t = t

    goExpr (EApp (EApp f tys1 args1) [] args2) = goExpr $ EApp f tys1 (args1 ++ args2)
    goExpr (ELet (Def i ty [Clause [] body]) (EVar i'))
      | i == i' = goExpr $ body
    goExpr (ELet (Def i ty [Clause pats body]) (EVar i'))
      | not $ null pats, i == i' = goExpr $ ELam pats body
    goExpr e = e

postSimplify :: Data a => a -> a
postSimplify x = x --everywhere (mkT goExpr) x

removeTcDicts :: Data a => a -> a
removeTcDicts x = everywhere (mkT goExpr `extT` goDefs `extT` goPats) x
  where
    goExpr (EVar (Id t _ _)) | "_tcdict" `T.isPrefixOf` t = ETcDict
    goExpr (ELet (Def (Id t _ _) _ _) body) | "_tcdict" `T.isPrefixOf` t = body
    goExpr e = e

    goDefs (Def (Id t _ _) _ _ : rest) | "_tcdict" `T.isPrefixOf` t = rest
    goDefs ds = ds

    goPats (PVar (Id t _ _) : rest) | "_tcdict" `T.isPrefixOf` t = rest
    goPats ps = ps

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
    go (ELet d body) = reconstructLet [d] body
    go e = e

reconstructLet :: [Def] -> Expr -> Expr
reconstructLet dsList body =
    -- `tail` skips over the `sccOrder` entry that represents the root/body.
    foldl (flip buildScc) body (tail $ toList sccOrder)
  where
    ds = S.fromList dsList

    -- The Def where each Id is defined
    idDef :: Map Id Int
    idDef = S.foldMapWithIndex (\idx d -> M.singleton (defId d) idx) ds

    idsToDefs :: Set Id -> Set Int
    idsToDefs is = Set.map (\i -> idDef M.! i) $
        Set.filter (\i -> M.member i idDef) is

    collectIds :: Data a => a -> Set Id
    collectIds x = everything (<>) (Set.empty `mkQ` go) x
      where go i@(Id _ _ _) = Set.singleton i

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
      | Set.size dIdxs == 1, not selfRec = ELet (ds `S.index` dIdx) body
      | otherwise = ELetRec (map (\dIdx -> ds `S.index` dIdx) $ Set.toList dIdxs) body
      where
        dIdxs = sccs `S.index` scc
        dIdx = Set.findMin dIdxs
        selfRec = Set.member scc $ sccEdges `S.index` scc


lastMaybe [] = Nothing
lastMaybe xs = Just $ last xs

usesVar i e = everything (||) (False `mkQ` go) e
  where
    go (EVar i') = i == i'
    go _ = False
