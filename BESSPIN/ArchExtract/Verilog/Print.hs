{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Verilog.Print where

import Data.Foldable
import qualified Data.Sequence as S
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import BESSPIN.ArchExtract.Verilog.AST

instance Pretty Design where
    pretty (Design mods) = vsep $ punctuate line $ toList $
        flip S.mapWithIndex mods $ \idx mod ->
            pretty idx <> colon <+> pretty mod

instance Pretty Module where
    pretty (Module name decls ports params items) =
        vsep
            [ "module " <> pretty name <+> lbrace
            , indent 4 $ vsep
                [ vsep $ toList $ flip S.mapWithIndex decls $ \idx decl ->
                    pretty idx <> colon <+> pretty decl
                , "ports" <+> equals <+> hsep (punctuate comma $ map pretty $ toList ports)
                , "params" <+> equals <+> hsep (punctuate comma $ map pretty $ toList params)
                , vsep $ toList $ flip S.mapWithIndex items $ \idx item ->
                    pretty idx <> colon <+> pretty item
                ]
            , rbrace
            ]

instance Pretty Decl where
    pretty d = "decl" <+> viaShow d

instance Pretty Item where
    pretty i = "item" <+> viaShow i


printVerilog :: Design -> Text
printVerilog d = renderStrict $ layoutPretty defaultLayoutOptions $ pretty d
