{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Gen.Clafer where

import Control.Monad
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

import BESSPIN.ArchExtract.Architecture

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
mkEqCon a b = Subconstraint noSpan $ Constraint noSpan [EEq noSpan a b]

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

convModule :: Design a -> Module a -> [Element]
convModule design mod = [ modCfr ]
  where
    name = moduleName mod
    implName = joinName ["origin"]
    modName = joinName ["module", name]

    impl = mkClafer implName concrete (Just ["verilog_implementation"])
        [ mkEqCon' ["name"] (mkStrLit name)
        -- TODO: file
        ]

    inPorts = [mkPath [cfrPortName Source i] | i <- [0 .. S.length (moduleInputs mod) - 1]]
    outPorts = [mkPath [cfrPortName Sink i] | i <- [0 .. S.length (moduleOutputs mod) - 1]]

    modCfr = mkClafer modName Abstract (Just ["component"]) (
        [ mkEqCon' ["name"] (mkStrLit name)
        , impl
        , mkEqCon' ["refinement"] (mkPath [implName])
        ]
        ++ toList (S.mapWithIndex convParam (moduleParams mod))
        ++ S.foldMapWithIndex (convLogic design mod) (moduleLogics mod)
        ++ S.foldMapWithIndex convNet (moduleNets mod)
        ++ toList (S.mapWithIndex (convPort Source) (moduleInputs mod))
        ++ toList (S.mapWithIndex (convPort Sink) (moduleOutputs mod))
        ++ mkEqSetCon' ["parts"] [mkPath [cfrLogicName i] |
            i <- [0 .. S.length (moduleLogics mod) - 1]]
        ++ mkEqSetCon' ["in_ports"] inPorts
        ++ mkEqSetCon' ["unbound_in_ports"] inPorts
        ++ mkEqSetCon' ["out_ports"] outPorts
        ++ mkEqSetCon' ["unbound_out_ports"] outPorts
        )

convParam :: Int -> Param -> Element
convParam idx param =
    mkRef (cfrParamName idx param) (mkPath ["integer"])

convLogic :: Design a -> Module a -> Int -> Logic a -> [Element]
convLogic design mod idx logic@(Logic { logicKind = LkInst inst }) =
    [ mkClafer (cfrLogicName idx) concrete (Just [instModName]) (
        [ mkEqCon' ["name"] (mkStrLit $ instName inst)
        ] ++ paramCons
        )]
  where
    instMod = design `designMod` instModId inst
    instModName = joinName ["module", moduleName instMod]

    parentVar i = ["parent", cfrParamName i $ mod `moduleParam` i]
    childVar i = [cfrParamName i $ instMod `moduleParam` i]

    paramCons = S.foldMapWithIndex go (moduleParams instMod)
    go idx decl = case join $ S.lookup idx (instParams inst) of
        Just expr -> maybeToList $
            mkEqCon' [cfrParamName idx decl] <$> convParamExpr parentVar expr
        Nothing -> case paramDefault decl of
            Just expr -> maybeToList $
                mkEqCon' [cfrParamName idx decl] <$> convParamExpr childVar expr
            Nothing ->
                traceShow ("no default available for", decl) []
convLogic _ _ idx logic =
    let displayName = case logicKind logic of
            LkRegister t -> Just t
            LkDFlipFlop t _ -> Just t
            _ -> Nothing
    in
    let inPorts = toList $ S.mapWithIndex (convPin Source) $ logicInputs logic in
    let outPorts = toList $ S.mapWithIndex (convPin Sink) $ logicOutputs logic in
    let inPortExps = [mkPath [cfrPortName Source i] | i <- [0 .. length inPorts - 1]] in
    let outPortExps = [mkPath [cfrPortName Sink i] | i <- [0 .. length outPorts - 1]] in
    let nameCon = case displayName of
            Nothing -> []
            Just t -> [mkEqCon' ["name"] (mkStrLit t)]
    in
    [ mkClafer (cfrLogicName idx) concrete (Just ["logic"])
        ( inPorts
        ++ outPorts
        ++ nameCon
        ++ mkEqSetCon' ["in_ports"] inPortExps
        ++ mkEqSetCon' ["unbound_in_ports"] inPortExps
        ++ mkEqSetCon' ["out_ports"] outPortExps
        ++ mkEqSetCon' ["unbound_out_ports"] outPortExps
        ) ]

flipSide Source = Sink
flipSide Sink = Source

convNet :: Int -> Net a -> [Element]
convNet idx net =
    [ mkClafer (cfrNetName idx j) concrete (Just ["connector"])
        [ mkEqCon' ["name"] (mkStrLit $ netName net)
        , mkEqCon' ["first"] (mkPath $ connComponent conn1)
        , mkEqCon' ["first_port"] (mkPath $ connComponent conn1 ++ connPort Source conn1)
        , mkEqCon' ["second"] (mkPath $ connComponent conn2)
        , mkEqCon' ["second_port"] (mkPath $ connComponent conn2 ++ connPort Sink conn2)
        -- TODO: type
        ]
    | (j, (conn1, conn2)) <- zip [0..] pairs ]
  where
    pairs = [(a,b) | a <- toList $ netSources net, b <- toList $ netSinks net]

    connComponent (ExtPort _) = ["parent"]
    connComponent (LogicPort i _) = ["parent", cfrLogicName i]

    connPort side (ExtPort i) = [cfrPortName side i]
    connPort side (LogicPort _ j) = [cfrPortName (flipSide side) j]


convPort :: Side -> Int -> Port -> Element
convPort side idx port =
    mkClafer (cfrPortName side idx) concrete (Just ["port"])
        [ mkEqCon' ["name"] (mkStrLit $ portName port)
        , mkClafer (if side == Source then "input" else "output") concrete Nothing []
        , mkClafer "unbound" concrete Nothing []
        -- TODO: type
        ]

convPin :: Side -> Int -> Pin -> Element
convPin side idx pin =
    mkClafer (cfrPortName side idx) concrete (Just ["port"])
        [ mkClafer (if side == Source then "input" else "output") concrete Nothing []
        , mkClafer "unbound" concrete Nothing []
        -- TODO: type
        ]


convParamExpr :: (Int -> [Text]) -> ConstExpr -> Maybe Exp
convParamExpr paramPath e = go e
  where
    go (EIntLit i) = Just $ mkIntLit i
    go (EParam idx) = Just $ mkPath $ paramPath idx
    go (EBinOp CbAdd l r) = EAdd noSpan <$> go l <*> go r
    go (EBinOp CbSub l r) = ESub noSpan <$> go l <*> go r
    go (EBinOp CbMul l r) = EMul noSpan <$> go l <*> go r
    go (ELog2 _) = Nothing


countClafers :: C.Module -> (Int, Int)
countClafers m =
    ( everything (+) (0 `mkQ` \e -> case e of
        Subclafer _ _ -> 1
        _ -> 0) m
    , everything (+) (0 `mkQ` \e -> case e of
        Subconstraint _ _ -> 1
        _ -> 0) m
    ) 


genClafer design = C.Module noSpan $ map (ElementDecl noSpan) elts
  where
    elts = concatMap (convModule design) (toList $ designMods design)

putAst a = do
    env <- getEnv
    putEnv env { cAst = Just a }

compiled ast = runClafer defaultClaferArgs { mode = [Args.Clafer] } $ do
    putAst ast
    iModule <- desugar Nothing
    compile iModule
    generate

