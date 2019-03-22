{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.BSV.PrintRaw where

import Data.Foldable
import qualified Data.Sequence as S
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import BESSPIN.ArchExtract.BSV.Raw

instance Pretty Package where
    pretty (Package name defs) = vsep $ punctuate line $ map pretty $ toList defs

instance Pretty Def where
    pretty (Def id ty clauses) =
        vsep
            [ pretty id <+> colon <> colon <+> pretty ty
            , vsep $ map pretty clauses
            ]

instance Pretty Clause where
    pretty (Clause pats body) =
        vsep
            [ hsep ("__" : map pretty pats) <+> equals
            , indent 4 $ pretty body
            ]

instance Pretty Id where
    pretty (Id name line col) = pretty name <> brackets (pretty line <> colon <> pretty col)

instance Pretty Expr where
    pretty (EVar i) = pretty i
    pretty (ELam pats body) =
        "\\" <> hsep (map pretty pats) <+> "->" <+> pretty body
    pretty (EApp f tys args) = parens $ hsep $
        [pretty f] ++ map (\t -> "@" <> pretty t) tys ++ map pretty args
    pretty (ELet def body) =
        vsep
            [ "let" <+> hang 0 (pretty def) <+> "in"
            , pretty body
            ]
    pretty (ELetRec defs body) =
        vsep
            [ "letrec" <+> hang 0 (vsep $ map pretty defs) <+> "in"
            , pretty body
            ]
    pretty (ELit l) = pretty l
    pretty (ERules rs) = vsep ["rules", indent 4 $ vsep $ map pretty rs]
    pretty (EStatic p f) = pretty p <> "::" <> pretty f
    pretty (EStruct ty fs) =
        vsep $ [ "struct" <+> pretty ty <+> lbrace ] ++ map (indent 4 . go) fs ++ [ rbrace ]
      where go (f, e) = pretty f <> colon <+> pretty e <> comma

    pretty (EPrim p) = pretty p
    pretty (EDo stmts last) =
        "do" <+> hang 0 (vsep $ map pretty stmts ++ [pretty last])
    pretty (EAddRules rs) = vsep [ "addRules", indent 2 $ vsep $ map go rs ]
      where
        go (Just name, body) = vsep [ "rule" <+> pretty name, indent 2 $ pretty body ]
        go (Nothing, body) = vsep [ "rule", indent 2 $ pretty body ]
    pretty (ERegRead e) = "*" <> pretty e
    pretty (ERegWrite l r) = pretty l <+> "<=" <+> pretty r
    pretty (EBinOp op l r) = parens $ pretty l <+> pretty op <+> pretty r
    pretty e = viaShow e

instance Pretty Rule where
    pretty r = viaShow r

instance Pretty Lit where
    pretty (LStr s) = pretty $ show s
    pretty (LChar c) = pretty $ show c
    pretty (LInt i) = pretty i
    pretty (LDouble d) = pretty d

instance Pretty Prim where
    pretty p = viaShow p

instance Pretty Stmt where
    pretty (SBind p e) = pretty p <+> "<-" <+> pretty e <> semi
    pretty (SBind' e) = pretty e <> semi

instance Pretty Pat where
    pretty (PVar i) = pretty i
    pretty p = viaShow p

instance Pretty Ty where
    pretty (TVar i) = pretty i
    pretty (TCon i) = pretty i <> "#"
    pretty (TNat n) = pretty n
    pretty (TApp t1 t2) = parens $ hsep $ map pretty (t1 : t2)
    pretty (TArrow t1 t2) = parens $ pretty t1 <+> "->" <+> pretty t2
    pretty t = viaShow t


printBSV :: Package -> Text
printBSV p = renderStrict $ layoutPretty defaultLayoutOptions $ pretty p

printAny p = renderStrict $ layoutPretty defaultLayoutOptions $ pretty p
