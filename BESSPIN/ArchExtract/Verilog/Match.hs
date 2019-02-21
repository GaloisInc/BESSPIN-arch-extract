{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Verilog.Match where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.State
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

import BESSPIN.ArchExtract.Verilog.AST

import Debug.Trace


collectVars :: Expr -> [Int]
collectVars e = everything (<>) ([] `mkQ` go) e
  where
    go (Var declId) = [declId]
    go _ = []


data FlattenState = FlattenState
    { fsAssigns :: Map Int Expr
    -- Set of variables that have been the target of a blocking assignment (on
    -- at least one path to the current location).  If one of these gets read
    -- later, then the block's effects can't be represented as a set of
    -- nonblocking assigns.
    , fsBlockingAssigned :: Set Int
    -- Set of variables that have been the target of a nonblocking assignment.
    -- If one of these is later the target of a blocking assignment,
    -- `flattenAssigns` will bail out.
    , fsNonblockingAssigned :: Set Int
    }
    deriving (Show)

-- Given a block of procedural statements, try to reduce its effect to a set of
-- nonblocking assignments.  If `precise` is true, the flattening fails on
-- assignments to array elements and struct fields; if it's false, it simply
-- treats such assignments as an assignment to the underlying variable
-- (producing ill-typed assignments as a result).
--
-- Note that blocking assignments can be converted to nonblocking ones as long
-- as later statements in the same block don't read the assigned value.
-- However, this will likely still fail on any nontrivial combinational logic.
flattenAssigns :: Bool -> [Stmt] -> Maybe (Map Int Expr)
flattenAssigns precise ss = fsAssigns <$>
    execStateT (mapM_ go ss) (FlattenState M.empty Set.empty Set.empty)
  where
    go stmt = case stmt of
        If cond then_ optElse -> do
            checkVars cond
            s <- get
            s1 <- lift $ execStateT (mapM go then_) s
            s2 <- case optElse of
                Nothing -> return s
                Just else_ -> lift $ execStateT (mapM go else_) s
            joinBranches cond s1 s2
        Case _ _ -> lift Nothing
        For _ _ _ _ -> lift Nothing
        NonBlockingAssign l r -> do
            var <- lift $ getLvalVar l
            modify $ \s -> s
                { fsAssigns = M.insert var r $ fsAssigns s
                , fsNonblockingAssigned = Set.insert var $ fsNonblockingAssigned s }
        BlockingAssign l r -> do
            var <- lift $ getLvalVar l

            checkVars r
            -- If the variable was previously the target of a nonblocking
            -- assign, then the resolution of the NBA will overwrite the value
            -- stored by the current assignment, so we shouldn't actually
            -- update `fsAssigns`.  However, we still add the var to
            -- `fsBlockingAssigned` because later reads would use the assigned
            -- value, and are thus incompatible with nonblocking assignment.
            assign <- gets $ not . Set.member var . fsNonblockingAssigned

            modify $ \s -> s
                { fsAssigns =
                    if assign then M.insert var r $ fsAssigns s else fsAssigns s
                , fsBlockingAssigned = Set.insert var $ fsBlockingAssigned s
                }
        BlockingUpdate _ -> lift Nothing

    joinBranches :: Expr -> FlattenState -> FlattenState -> StateT FlattenState Maybe ()
    joinBranches cond s1 s2 = do
        let shared = M.intersectionWith
                (\t e -> if t == e then t else IfExpr' cond t e dummySpan)
                (fsAssigns s1) (fsAssigns s2)
        let only1 = M.mapWithKey (\i t -> IfExpr' cond t (Var' i dummySpan) dummySpan) $
                fsAssigns s1 M.\\ fsAssigns s2
        let only2 = M.mapWithKey (\i e -> IfExpr' cond (Var' i dummySpan) e dummySpan) $
                fsAssigns s2 M.\\ fsAssigns s1

        -- The expressions in `only1` and `only2` introduce new references to
        -- some variables.  We need to check that those variables weren't
        -- previously blocking-assigned.  We only need to check the pre-branch
        -- `fsBlockingAssigned` set, because if a variable `i` is in `only1`,
        -- then the new reference is 
        ba <- gets fsBlockingAssigned
        guard $ not $ any (flip Set.member ba) $
            Set.toList (M.keysSet only1 <> M.keysSet only2)

        put $ FlattenState
            { fsAssigns = shared <> only1 <> only2
            , fsBlockingAssigned =
                fsBlockingAssigned s1 <> fsBlockingAssigned s2
            , fsNonblockingAssigned =
                fsNonblockingAssigned s1 <> fsNonblockingAssigned s2
            }

    checkVars :: Expr -> StateT FlattenState Maybe ()
    checkVars e = do
        ba <- gets fsBlockingAssigned
        guard $ not $ any (flip Set.member ba) $ collectVars e

    getLvalVar =
        if precise then
            let go (Var declId) = Just declId
                go _ = Nothing
            in go
        else
            let go (Var declId) = Just declId
                go (Index base _) = go base
                go (MemIndex base _) = go base
                go _ = Nothing
            in go


data DFlipFlop = DFlipFlop
    { dffClk :: Expr
    -- Input expression.
    , dffD :: Expr
    -- Output net, identified by the decl ID of the net's `VarDecl`.
    , dffQ :: Int
    -- Async reset inputs.
    , dffAsyncResets :: [Expr]
    }
    deriving (Show)

guardMsg True _ = return ()
guardMsg False msg = Left msg

fromJustMsg (Just x) _ = return x
fromJustMsg Nothing msg = Left msg

-- Try to recognize an `Always` item as a set of D flip-flops.  Returns
-- `Nothing` if the item doesn't appear to be D flip-flops.
inferDFlipFlop :: Item -> Either Text [DFlipFlop]
inferDFlipFlop (Always evts ss) = do
    assigns <- fromJustMsg (flattenAssigns True ss)
        "failed to interpret `always` block as a compound nonblocking assignment"
    guardMsg (all (isJust . eventEdge) evts)
        "sensitivity list contains non-edge-sensitive signals "
    let edgeMap = M.fromList $
            map (\e -> (eventVarDeclId e, fromJust $ eventEdge e)) evts
    guardMsg (M.size edgeMap == length evts)
        "sensitivity list contains the same signal multiple times"
    guardMsg (Set.null $ Set.intersection (M.keysSet assigns) (M.keysSet edgeMap))
        "target of assignment appears in sensitivity list"

    mapM (go edgeMap) $ M.toList assigns
  where
    go edgeMap (var, expr) = do
        guardMsg (not $ Set.null clocks) $
            "detected no clocks controlling variable " <> T.pack (show var)
        guardMsg (Set.size clocks == 1) $
            "detected multiple clocks controlling variable " <> T.pack (show var)
        return $ DFlipFlop
            { dffClk = Var' (Set.findMin clocks) dummySpan
            , dffD = dataExpr
            , dffQ = var
            , dffAsyncResets = map (\i -> Var' i dummySpan) asyncResetSigs
            }
      where
        cvars = condVars expr
        asyncResets = findAsyncResets edgeMap expr
        asyncResetSigs = map (\(v,_,_) -> v) asyncResets
        clocks = M.keysSet edgeMap Set.\\ Set.fromList asyncResetSigs
        dataExpr = evalConds (M.fromList $ map (\(v,l,_) -> (v, not l)) asyncResets) expr

    -- Find async reset inputs to `expr`.  A signal is an async reset if:
    --  1. It appears in the sensitivity list (`edgeMap`)
    --  2. Each time it makes the transition given in `edgeMap`, the expression
    --     evaluates to a constant.
    --  3. It has precedence over all other inputs, except possibly other async
    --     resets.
    --
    -- This function returns the list of detected async resets, indicating for
    -- each one the variable that controls the reset, the level at which the
    -- reset is active, and the (constant) expression that gets assigned when
    -- active.  Resets are listed in priority order, from highest to lowest.
    findAsyncResets :: Map Int Edge -> Expr -> [(Int, Bool, Expr)]
    findAsyncResets edgeMap e = go initCandMap
      where
        -- Candidate signals are those that appear both in the sensitivity list
        -- (edge-triggered) and in a conditional.  The `Bool` is the value at
        -- which the signal might cause a reset.
        cands :: Map Int Bool
        cands = M.restrictKeys (fmap edgeLevel edgeMap) (Set.fromList $ condVars e)

        -- Strategy:
        --  1. For each candidate `v`, build a partial assignment that maps `v`
        --     to its trigger value (`cands M.! v`) and maps all previously
        --     discovered async resets to the opposite of their trigger values
        --     (`not $ cands M.! v'`).
        --  2. Pick a candidate `v` for which `e` evaluates to a constant
        --     expression under `v`'s partial assignment.  `v` is an async
        --     reset (and has priority at least as high as all other async
        --     resets not previously discovered).
        --  3. Remove `v` from the set of candidates, and repeat from step 1.

        -- The actual implementation inlines a few steps.  In particular, we
        -- never build a partial assignment explicitly.  Instead, we keep the
        -- result of `evalConds partialAssign e`, and update this expression
        -- incrementally as new async resets are discovered.  This works
        -- because `evalConds m1 (evalConds m2 e) == evalConds (m1 <> m2) e`,
        -- as long as `m1` and `m2` share no variables.

        -- `candMap` stores both the trigger level, as well as the result of
        -- evaluating `e` under each candidate's partial assignment.
        go :: Map Int (Bool, Expr) -> [(Int, Bool, Expr)]
        go candMap = traceShow ("found reset", foundAsyncReset, "in", candMap) $ case foundAsyncReset of
            Nothing -> []
            Just v ->
                let (level, resetVal) = candMap M.! v in
                -- Remove `v`, then add `v' |-> not level` to all remaining
                -- partial assignments.
                let candMap' =
                        fmap (\(l,e) -> (l, evalConds (M.singleton v $ not level) e)) $
                        M.delete v candMap in
                (v, level, resetVal) : go candMap'
          where
            -- Find the first candidate whose partial assignment produces a
            -- constant expression.
            foundAsyncReset = fst <$> M.lookupMin (M.filter (isConstExpr . snd) candMap)

        -- Initialize each `v`'s partial assignment with `v |-> level`.
        initCandMap = M.mapWithKey (\v l ->
            (l, evalConds (M.singleton v l) e)) cands

inferDFlipFlop _ = Left "expected an `always` block"


edgeLevel PosEdge = True
edgeLevel NegEdge = False
        

-- Find all variables that appear in outermost `IfExpr` conditions (i.e.,
-- `IfExpr`s that aren't nested under non-`IfExpr` expressions).
condVars :: Expr -> [Int]
condVars (IfExpr cond then_ else_) =
    collectVars cond <> condVars then_ <> condVars else_
condVars _ = []

isConstExpr :: Expr -> Bool
isConstExpr e = everything (&&) (True `mkQ` go) e
  where
    go (Var _) = False
    go (Param _) = True
    go (Const _) = True
    go (ConstBool _ _) = True
    go _ = True

-- Evaluate `e` under partial assignment `assign`, specifically with the goal
-- of eliminating conditionals (`IfExpr`).
evalConds :: Map Int Bool -> Expr -> Expr
evalConds assign e = everywhere (mkT go) e
  where
    go (Var' i sp) = case M.lookup i assign of
        Just False -> ConstBool' "1'b0" False sp
        Just True -> ConstBool' "1'b1" True sp
        Nothing -> Var' i sp
    go (IfExpr' c t e sp) = case boolEval assign c of
        Just True -> t
        Just False -> e
        Nothing -> IfExpr' c t e sp
    go x = x

-- Simple evaluator for boolean expressions.  Returns `Nothing` for expressions
-- whose results are indeterminate under the given assignment.
boolEval :: Map Int Bool -> Expr -> Maybe Bool
boolEval assign e = go e
  where
    go (Var i) = M.lookup i assign
    go (ConstBool _ b) = Just b
    go (Unary UNot e) = not <$> go e
    go (Unary ULogNot e) = not <$> go e
    go (Binary BAnd l r) = and3 (go l) (go r)
    go (Binary BOr l r) = or3 (go l) (go r)
    go (Binary BLogAnd l r) = and3 (go l) (go r)
    go (Binary BLogOr l r) = or3 (go l) (go r)
    go _ = Nothing

    and3 a b = case (a, b) of
        (Just True, Just True) -> Just True
        (Just False, _) -> Just False
        (_, Just False) -> Just False
        _ -> Nothing

    or3 a b = case (a, b) of
        (Just False, Just False) -> Just False
        (Just True, _) -> Just True
        (_, Just True) -> Just True
        _ -> Nothing
