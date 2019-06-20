{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Print where

import Data.Foldable
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Traversable

import BESSPIN.ArchExtract.Architecture

labeled :: Doc ann -> Doc ann -> Doc ann
labeled l x = vsep [l <+> lbrace, indent 4 x, rbrace]

numbered :: Pretty a => Doc ann -> Seq a -> [Doc ann]
numbered l xs = numbered' l $ fmap pretty xs

numbered' :: Doc ann -> Seq (Doc ann) -> [Doc ann]
numbered' l xs = toList $
    flip S.mapWithIndex xs $ \idx x -> l <+> pretty idx <> colon <+> x

instance Pretty (Design a) where
    pretty (Design mods) = vsep $ punctuate line $ numbered "module" mods

instance Pretty (Module a) where
    pretty (Module name kind params inputs outputs logics nets constraints) =
        labeled (pretty name <+> parens (viaShow kind)) $ vsep $
            numbered "param" params <>
            numbered "input" inputs <>
            numbered "output" outputs <>
            numbered "logic" logics <>
            numbered "net" nets <>
            numbered "constraint" constraints

instance Pretty Param where
    pretty x = "param" <+> viaShow x

instance Pretty Port where
    pretty x = "port" <+> viaShow x

instance Pretty (Logic x) where
    pretty (Logic kind inputs outputs _ann) =
        let header = case kind of
                LkInst inst -> "inst" <+> pretty (instName inst)
                LkNetAlias -> "netAlias"
                LkRegister name -> "reg" <+> pretty name
                LkDFlipFlop name _ -> "dff" <+> pretty name
                LkRam name _ _ _ _ -> "ram" <+> pretty name
                LkMux _ _ -> "mux"
                LkPriorityMux _ _ -> "prioMux"
                LkRuleMux _ _ -> "ruleMux"
                LkMatch _ _ -> "match"
                LkRuleEnable name -> "ruleEnable" <+> pretty name
                LkPack _ -> "pack"
                LkUnpack _ -> "unpack"
                LkRepack (Just name) _ _ -> "repack" <+> pretty name
                LkRepack Nothing _ _ -> "repack"
                LkExpr -> "expr"
                LkOther -> emptyDoc
        in
        let details = case kind of
                LkInst inst ->
                    [ "modId" <+> pretty (instModId inst) ] <>
                    numbered' "param" (fmap (\p -> case p of
                        Nothing -> "(default)"
                        Just x -> pretty x) $ instParams inst)
                LkDFlipFlop _ numResets ->
                    [ "numResets" <+> pretty numResets ]
                LkMux names numInputs ->
                    [ "pinNames" <+> pretty (toList names)
                    , "numInputs" <+> pretty numInputs
                    ]
                LkPriorityMux names numInputs ->
                    [ "pinNames" <+> pretty (toList names)
                    , "numInputs" <+> pretty numInputs
                    ]
                LkRuleMux rules pins ->
                    [ "ruleNames" <+> pretty (toList rules)
                    , "pinNames" <+> pretty (toList pins)
                    ]
                LkMatch numInputs outNames ->
                    [ "numInputs" <+> pretty numInputs
                    , "outNames" <+> pretty (toList outNames)
                    ]
                LkRam _ depth resets readPorts writePorts ->
                    [ "depth" <+> pretty depth
                    , "numResets" <+> pretty resets
                    , "numReadPorts" <+> pretty readPorts
                    , "numWritePorts" <+> pretty writePorts
                    ]
                LkPack names -> [ "names" <+> pretty (toList names) ]
                LkUnpack names -> [ "names" <+> pretty (toList names) ]
                LkRepack _ ins outs ->
                    [ "inputs" <+> pretty (toList ins)
                    , "outputs" <+> pretty (toList outs)
                    ]
                _ -> []
        in
        labeled header $ vsep $
            details <>
            numbered "input" inputs <>
            numbered "output" outputs

instance Pretty (Net x) where
    pretty (Net name _prio sources sinks ty _ann) =
        let shortName = head $ T.lines name in
        labeled (pretty shortName <+> colon <+> pretty ty) $ vsep $
            numbered "source" sources <>
            numbered "sink" sinks

instance Pretty Pin where
    pretty p = "pin" <+> viaShow p

instance Pretty Conn where
    pretty c = "conn" <+> viaShow c

instance Pretty ConstExpr where
    pretty (EIntLit _ i) = pretty i
    pretty (EParam _ i) = "param_" <> pretty i
    pretty (EInstParam _ is j) = hcat $ ["param_"] ++ map pretty is ++ [pretty j]
    pretty (EUnArith _ UClog2 e) = "$clog2" <> parens (pretty e)
    pretty (EUnArith _ UIsPow2 e) = "$ispow2" <> parens (pretty e)
    pretty (EBinArith _ op l r) = parens $ hsep [pretty l, pretty op, pretty r]
    pretty (EBinCmp _ op l r) = parens $ hsep [pretty l, pretty op, pretty r]
    pretty (ERangeSize _ l r) = "$rangesize" <> parens (pretty l <> comma <+> pretty r)
    pretty (EOverride i e) = "override_" <> pretty i <> parens (pretty e)
    pretty (EOverrideLocalParam i e) = "override_local_" <> pretty i <> parens (pretty e)
    pretty (EOverrideInstParam i j e) =
        "override_inst_" <> pretty i <> "_" <> pretty j <> parens (pretty e)

instance Pretty BinArithOp where
    pretty BAdd = "+"
    pretty BSub = "-"
    pretty BMul = "*"

instance Pretty BinCmpOp where
    pretty BEq = "=="
    pretty BNe = "!="
    pretty BLt = "<"
    pretty BLe = "<="
    pretty BGt = ">"
    pretty BGe = ">="

instance Pretty Constraint where
    pretty e = viaShow e

instance Pretty Ty where
    pretty e = viaShow e


showPretty :: Pretty a => a -> Text
showPretty x = renderStrict $ layoutPretty defaultLayoutOptions $ pretty x

printArchitecture :: Design a -> Text
printArchitecture d = showPretty d
