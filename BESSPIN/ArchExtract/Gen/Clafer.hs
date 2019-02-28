{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Gen.Clafer where

import Control.Monad
import Control.Monad.Writer
import Data.Foldable
import Data.Generics
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T

import Debug.Trace

import Language.Clafer hiding (Module)
import Language.Clafer.Common
import Language.Clafer.Front.AbsClafer hiding (Module)
import qualified Language.Clafer.Front.AbsClafer as C
import Language.Clafer.Front.PrintClafer
import qualified Language.Clafer.ClaferArgs as Args

import BESSPIN.ArchExtract.Architecture hiding (Span)
import qualified BESSPIN.ArchExtract.Config as Config

joinName parts = T.intercalate "_" parts


mkPath :: [Text] -> Exp
mkPath (name : names) = foldl (\e n -> EJoin noSpan e (go n)) (go name) names
  where
    go :: Text -> Exp
    go n = ClaferId noSpan $ Path noSpan [ModIdIdent noSpan $ mkIdent $ T.unpack n]

mkClafer :: Text -> (Span -> Abstract) -> Maybe [Text] -> [Element] -> Element
mkClafer name abs super elts = Subclafer noSpan $ Clafer noSpan
    (abs noSpan)
    []
    (GCardEmpty noSpan)
    (mkIdent $ T.unpack name)
    (case super of
        Just names -> SuperSome noSpan $ mkPath names
        Nothing -> SuperEmpty noSpan)
    (ReferenceEmpty noSpan)
    (CardEmpty noSpan)
    (InitEmpty noSpan)
    (TransitionEmpty noSpan)
    (ElementsList noSpan elts)

mkRef :: Text -> Exp -> Element
mkRef name target = Subclafer noSpan $ Clafer noSpan
    (AbstractEmpty noSpan)
    []
    (GCardEmpty noSpan)
    (mkIdent $ T.unpack name)
    (SuperEmpty noSpan)
    (ReferenceSet noSpan target)
    (CardEmpty noSpan)
    (InitEmpty noSpan)
    (TransitionEmpty noSpan)
    (ElementsEmpty noSpan)

mkIntLit :: Int -> Exp
mkIntLit i = EInt noSpan $ PosInteger ((0, 0), show i)

mkStrLit :: Text -> Exp
mkStrLit s = EStr noSpan $ PosString ((0, 0), show $ T.unpack s)

mkEqCon :: Exp -> Exp -> Element
mkEqCon a b = Subconstraint noSpan $ C.Constraint noSpan [EEq noSpan a b]

mkEqCon' :: [Text] -> Exp -> Element
mkEqCon' ts b = mkEqCon (mkPath ts) b

mkSet :: [Exp] -> Exp
mkSet [] = error "mkSet: no expressions"
mkSet es = foldl1 (EUnionCom noSpan) es

mkEqSetCon' :: [Text] -> [Exp] -> [Element]
mkEqSetCon' ts [] =
    [ mkEqCon (ECard noSpan $ mkPath ts) (mkIntLit 0)
    ]
mkEqSetCon' ts es =
    [ mkEqCon (ECard noSpan $ mkPath ts) (mkIntLit $ length es)
    , mkEqCon' ts (mkSet es)
    ]

concrete = AbstractEmpty


cfrPortName Source idx = joinName ["port", "in", T.pack (show idx)]
cfrPortName Sink idx = joinName ["port", "out", T.pack (show idx)]
cfrLogicName idx = joinName ["logic", T.pack (show idx)]
cfrNetName idx subIdx = joinName ["net", T.pack (show idx), T.pack (show subIdx)]
cfrParamName idx param = joinName [paramName param, T.pack (show idx)]

convModule :: Config.Clafer -> Design a -> Module a -> [Element]
convModule cfg design mod = [ modCfr ]
  where
    name = moduleName mod
    originName = joinName ["origin"]
    modName = joinName ["module", name]

    origin = mkClafer originName concrete originParentName originParts
    originParentName =
        if Config.claferUseBackgroundTheory cfg then
            Just ["verilog_implementation"] else Nothing
    originParts = execWriter $ do
        if Config.claferUseBackgroundTheory cfg then
            tell [mkEqCon' ["name"] (mkStrLit name)]
        else
            tell [mkRef "name" (mkStrLit name)]

    inPorts = [mkPath [cfrPortName Source i] | i <- [0 .. S.length (moduleInputs mod) - 1]]
    outPorts = [mkPath [cfrPortName Sink i] | i <- [0 .. S.length (moduleOutputs mod) - 1]]

    baseParts = execWriter $ do
        when (Config.claferUseBackgroundTheory cfg) $
            tell [mkEqCon' ["name"] (mkStrLit name)]

        when (Config.claferEmitOrigin cfg) $ do
            tell [origin]
            when (Config.claferUseBackgroundTheory cfg) $
                tell [mkEqCon' ["refinement"] (mkPath [originName])]

    paramParts = toList (S.mapWithIndex convParam (moduleParams mod))

    netParts = S.foldMapWithIndex (convNet cfg) (moduleNets mod)

    portParts = execWriter $ do
        tell $ toList (S.mapWithIndex (convPort cfg Source) (moduleInputs mod))
        tell $ toList (S.mapWithIndex (convPort cfg Sink) (moduleOutputs mod))
        when (Config.claferUseBackgroundTheory cfg) $ do
            tell $ mkEqSetCon' ["in_ports"] inPorts
            tell $ mkEqSetCon' ["unbound_in_ports"] inPorts
            tell $ mkEqSetCon' ["out_ports"] outPorts
            tell $ mkEqSetCon' ["unbound_out_ports"] outPorts

    -- List of the indexes of all logics we should emit.
    emitLogicIdxs = S.foldMapWithIndex (\idx l -> case logicKind l of
        LkInst _ | Config.claferEmitInsts cfg -> [idx]
        _ | Config.claferEmitLogics cfg -> [idx]
        _ -> []) (moduleLogics mod)
    logicParts = execWriter $ do
        forM_ emitLogicIdxs $ \idx -> do
            let l = mod `moduleLogic` idx
            tell $ convLogic cfg design mod idx l

        when (Config.claferUseBackgroundTheory cfg) $
            tell $ mkEqSetCon' ["parts"] [mkPath [cfrLogicName i] | i <- emitLogicIdxs]

    parentName =
        if Config.claferUseBackgroundTheory cfg then Just ["component"] else Nothing
    modCfr = mkClafer modName Abstract parentName (
        baseParts
        ++ (if Config.claferEmitParams cfg then paramParts else [])
        ++ (if Config.claferEmitNets cfg then netParts else [])
        ++ (if Config.claferEmitPorts cfg then portParts else [])
        -- Logic-related config settings are handled during construction of
        -- logicParts.
        ++ logicParts
        )

convParam :: Int -> Param -> Element
convParam idx param =
    mkRef (cfrParamName idx param) (mkPath ["integer"])

convLogic :: Config.Clafer -> Design a -> Module a -> Int -> Logic a -> [Element]
convLogic cfg design mod idx logic@(Logic { logicKind = LkInst inst }) =
    [ mkClafer (cfrLogicName idx) concrete (Just [instModName]) parts]
  where
    instMod = design `designMod` instModId inst
    instModName = joinName ["module", moduleName instMod]

    parts = execWriter $ do
        when (Config.claferUseBackgroundTheory cfg) $
            tell [mkEqCon' ["name"] (mkStrLit $ instName inst)]
        when (Config.claferEmitParams cfg) $
            tell $ S.foldMapWithIndex go (moduleParams instMod)

    parentVar i = ["parent", cfrParamName i $ mod `moduleParam` i]
    childVar i = [cfrParamName i $ instMod `moduleParam` i]

    go idx decl = case join $ S.lookup idx (instParams inst) of
        Just expr -> maybeToList $
            mkEqCon' [cfrParamName idx decl] <$> convParamExpr parentVar expr
        Nothing -> case paramDefault decl of
            Just expr -> maybeToList $
                mkEqCon' [cfrParamName idx decl] <$> convParamExpr childVar expr
            Nothing ->
                traceShow ("no default available for", decl) []

convLogic cfg _ _ idx logic =
    [ mkClafer (cfrLogicName idx) concrete parentName parts ]
  where
    parts = execWriter $ do
        tell $ toList $ S.mapWithIndex (convPin cfg Source) $ logicInputs logic
        tell $ toList $ S.mapWithIndex (convPin cfg Sink) $ logicOutputs logic
        when (Config.claferUseBackgroundTheory cfg) $ do
            tell $ case displayName of
                Nothing -> []
                Just t -> [mkEqCon' ["name"] (mkStrLit t)]
            let inPortExps = [mkPath [cfrPortName Source i] |
                    i <- [0 .. S.length (logicInputs logic) - 1]]
            let outPortExps = [mkPath [cfrPortName Sink i] |
                    i <- [0 .. S.length (logicOutputs logic) - 1]]
            tell $ mkEqSetCon' ["in_ports"] inPortExps
            tell $ mkEqSetCon' ["unbound_in_ports"] inPortExps
            tell $ mkEqSetCon' ["out_ports"] outPortExps
            tell $ mkEqSetCon' ["unbound_out_ports"] outPortExps

    displayName = case logicKind logic of
        LkRegister t -> Just t
        LkDFlipFlop t _ -> Just t
        LkRam t _ _ _ _ -> Just t
        _ -> Nothing

    parentName =
        if Config.claferUseBackgroundTheory cfg then Just ["logic"] else Nothing

convNet :: Config.Clafer -> Int -> Net a -> [Element]
convNet cfg idx net | not $ Config.claferUseBackgroundTheory cfg =
    [ mkClafer (cfrNetName idx 0) concrete Nothing
        [ mkRef "name" (mkStrLit $ netName net) ] ]
convNet cfg idx net =
    [ mkClafer (cfrNetName idx j) concrete (Just ["connector"])
        [ mkEqCon' ["name"] (mkStrLit $ netName net)
        , mkEqCon' ["first"] (mkPath $ connComponent conn1)
        , mkEqCon' ["first_port"] (mkPath $ connComponent conn1 ++ connPort Source conn1)
        , mkEqCon' ["second"] (mkPath $ connComponent conn2)
        , mkEqCon' ["second_port"] (mkPath $ connComponent conn2 ++ connPort Sink conn2)
        , mkEqCon' ["type"] (mkPath [typeName $ netTy net])
        ]
    | (j, (conn1, conn2)) <- zip [0..] pairs ]
  where
    pairs = [(a,b) | a <- toList $ netSources net, b <- toList $ netSinks net]

    connComponent (ExtPort _) = ["parent"]
    connComponent (LogicPort i _) = ["parent", cfrLogicName i]

    connPort side (ExtPort i) = [cfrPortName side i]
    connPort side (LogicPort _ j) = [cfrPortName (flipSide side) j]


-- TODO: handle dimension expressions
typeName (TWire [] []) = "ty_wire"
typeName (TWire _ []) = "ty_bus"
typeName (TWire _ _) = "ty_memory"
typeName _ = "ty_unknown"

convPort :: Config.Clafer -> Side -> Int -> Port -> Element
convPort cfg side idx port =
    mkClafer (cfrPortName side idx) concrete parentName parts
  where
    parts = execWriter $ do
        when (Config.claferUseBackgroundTheory cfg) $ do
            tell [mkEqCon' ["name"] (mkStrLit $ portName port)]
            tell [mkEqCon' ["type"] (mkPath [typeName $ portTy port])]
        tell [mkClafer (if side == Source then "input" else "output") concrete Nothing []]
        tell [mkClafer "unbound" concrete Nothing []]

    parentName =
        if Config.claferUseBackgroundTheory cfg then Just ["port"] else Nothing

convPin :: Config.Clafer -> Side -> Int -> Pin -> Element
convPin cfg side idx pin =
    mkClafer (cfrPortName side idx) concrete parentName parts
  where
    parts = execWriter $ do
        when (Config.claferUseBackgroundTheory cfg) $ do
            tell [mkEqCon' ["type"] (mkPath [typeName $ pinTy pin])]
        tell [mkClafer (if side == Source then "input" else "output") concrete Nothing []]
        tell [mkClafer "unbound" concrete Nothing []]

    parentName =
        if Config.claferUseBackgroundTheory cfg then Just ["port"] else Nothing


convRoot :: Config.Clafer -> Design a -> Int -> Element
convRoot cfg design modId =
    mkClafer (joinName ["root", moduleName instMod]) concrete (Just [instModName]) parts
  where
    instMod = design `designMod` modId
    instModName = joinName ["module", moduleName instMod]

    parts = execWriter $ do
        when (Config.claferUseBackgroundTheory cfg) $
            tell [mkEqCon' ["name"] (mkStrLit $ "root " <> moduleName instMod)]
        when (Config.claferEmitParams cfg) $
            tell $ S.foldMapWithIndex go (moduleParams instMod)

    childVar i = [cfrParamName i $ instMod `moduleParam` i]

    paramCons = S.foldMapWithIndex go (moduleParams instMod)
    go idx decl = case paramDefault decl of
        Just expr -> maybeToList $
            mkEqCon' [cfrParamName idx decl] <$> convParamExpr childVar expr
        Nothing ->
            traceShow ("no default available for", decl) []


convParamExpr :: (Int -> [Text]) -> ConstExpr -> Maybe Exp
convParamExpr paramPath e = go e
  where
    -- TODO: warn when dropping a constraint due to unsupported expressions
    go (EIntLit _ i) = Just $ mkIntLit i
    go (EParam _ idx) = Just $ mkPath $ paramPath idx
    go (EInstParam _ [] idx) = Just $ mkPath $ paramPath idx
    go (EInstParam _ _ _) = Nothing
    go (EUnArith _ UClog2 _) = Nothing
    go (EUnArith _ UIsPow2 _) = Nothing
    go (EBinArith _ BAdd l r) = EAdd noSpan <$> go l <*> go r
    go (EBinArith _ BSub l r) = ESub noSpan <$> go l <*> go r
    go (EBinArith _ BMul l r) = EMul noSpan <$> go l <*> go r
    go (EBinCmp _ BEq l r) = EEq noSpan <$> go l <*> go r
    go (EBinCmp _ BNe l r) = ENeq noSpan <$> go l <*> go r
    go (EBinCmp _ BLt l r) = ELt noSpan <$> go l <*> go r
    go (EBinCmp _ BLe l r) = ELte noSpan <$> go l <*> go r
    go (EBinCmp _ BGt l r) = EGt noSpan <$> go l <*> go r
    go (EBinCmp _ BGe l r) = EGte noSpan <$> go l <*> go r
    go (ERangeSize _ _ _) = Nothing


countClafers :: C.Module -> (Int, Int)
countClafers m =
    ( everything (+) (0 `mkQ` \e -> case e of
        Subclafer _ _ -> 1
        _ -> 0) m
    , everything (+) (0 `mkQ` \e -> case e of
        Subconstraint _ _ -> 1
        _ -> 0) m
    ) 


genClafer cfg design rootIds = C.Module noSpan $ map (ElementDecl noSpan) elts
  where
    elts =
        concatMap (convModule cfg design) (toList $ designMods design)
        ++ map (convRoot cfg design) rootIds

putAst a = do
    env <- getEnv
    putEnv env { cAst = Just a }

compiled ast = runClafer defaultClaferArgs { mode = [Args.Clafer] } $ do
    putAst ast
    iModule <- desugar Nothing
    compile iModule
    generate

