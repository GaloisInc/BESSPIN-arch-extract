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
    pretty (Module name params inputs outputs logics nets constraints) =
        labeled (pretty name) $ vsep $
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
                LkRam _ depth resets readPorts writePorts ->
                    [ "depth" <+> pretty depth
                    , "numResets" <+> pretty resets
                    , "numReadPorts" <+> pretty readPorts
                    , "numWritePorts" <+> pretty writePorts
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
    pretty e = viaShow e

instance Pretty Constraint where
    pretty e = viaShow e

instance Pretty Ty where
    pretty e = viaShow e


printArchitecture :: Design a -> Text
printArchitecture d = renderStrict $ layoutPretty defaultLayoutOptions $ pretty d
