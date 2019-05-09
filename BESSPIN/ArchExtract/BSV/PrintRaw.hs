{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.BSV.PrintRaw where

import Data.Foldable
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import BESSPIN.ArchExtract.BSV.Raw

instance Pretty Package where
    pretty (Package name imports defs structs unions typedefs) =
        vsep $ punctuate line $
            map prettyImport imports
            <> map pretty (toList defs)
            <> map pretty (toList structs)
            <> map pretty (toList unions)
            <> map pretty (toList typedefs)

prettyImport name = "import" <+> pretty name

instance Pretty Struct where
    pretty (Struct i tyParams fields isIfc) =
        vsep
            [ (if isIfc then "interface" else "struct") <+> pretty i
                <> brackets (hsep $ map pretty tyParams) <+> lbrace
            , indent 4 $ vsep $ map pretty fields
            , rbrace
            ]

instance Pretty Field where
    pretty (Field name ty _) = pretty name <+> colon <> colon <+> pretty ty

instance Pretty Union where
    pretty (Union i tyParams variants) =
        vsep
            [ "union" <+> pretty i
                <> brackets (hsep $ map pretty tyParams) <+> lbrace
            , indent 4 $ vsep $ map pretty variants
            , rbrace
            ]

instance Pretty Variant where
    pretty (Variant i ty) =
        pretty i <+> "::" <+> pretty ty

instance Pretty Typedef where
    pretty (Typedef i tyParams ty) =
        "type" <+> hsep (map pretty $ i : tyParams) <+> equals <+> pretty ty <> semi

instance Pretty Def where
    pretty (Def id ty clauses) =
        vsep
            [ pretty id <+> colon <> colon <+> pretty ty
            , vsep $ map pretty clauses
            ]

instance Pretty Clause where
    pretty (Clause pats [] body) =
        vsep
            [ hsep ("__" : map pretty pats) <+> equals
            , indent 4 $ pretty body
            ]
    pretty (Clause pats guards body) =
        vsep
            [ hsep ("__" : map pretty pats)
            , indent 2 $ vsep $ zipWith (<+>) ("|" : repeat ",") (map pretty guards)
            , indent 2 $ equals <+> pretty body
            ]

instance Pretty Guard where
    pretty (GPat pat ty expr) = pretty pat <+> "<-" <+> pretty expr
    pretty (GCond expr) = pretty expr

instance Pretty Id where
    pretty (Id name line col) = pretty name <> brackets (pretty line <> colon <> pretty col)

instance Pretty Expr where
    pretty (EVar i) = pretty i
    pretty (ELam pats body) =
        "\\" <> hsep (map pretty pats) <+> "->" <+> pretty body
    pretty (EApp f tys args) = parens $ hsep $
        [pretty f] ++ map (\t -> "@" <> pretty t) tys ++ map pretty args
    pretty (ELet def body nid msgs) =
        vsep $
            [ "let" <+> hang 0 (pretty def) <+> "in" <+> "--" <+> pretty nid ]
            ++ map (\l -> "--" <+> pretty l) msgs
            ++ [pretty body]
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
    pretty (EAddRules rs) = vsep [ "addRules", indent 2 $ vsep $ map pretty rs ]
    pretty e = viaShow e

instance Pretty RawRule where
    pretty r = viaShow r

instance Pretty Rule where
    pretty (Rule optName conds body) = vsep $
        [header]
        ++ map (\c -> indent 2 $ "|" <+> pretty c) conds
        ++ [indent 2 $ pretty body]
      where
        header = case optName of
            Just name -> "rule" <+> pretty name
            Nothing -> "rule"

instance Pretty Lit where
    pretty (LStr s) = pretty $ show s
    pretty (LChar c) = pretty $ show c
    pretty (LInt i) = pretty i
    pretty (LDouble d) = pretty d

instance Pretty Prim where
    pretty p = viaShow p

instance Pretty Stmt where
    pretty (SBind p t e nid) =
        pretty p <+> "::" <+> pretty t <+> "<-" <+> pretty e <> semi
            <+> "--" <+> pretty nid
    pretty (SBind' e nid) =
        pretty e <> semi <+> "--" <+> pretty nid
    pretty (SNote text) = vsep $ map (\l -> "--" <+> pretty l) $ T.lines text

instance Pretty Pat where
    pretty (PVar i) = pretty i
    pretty p = viaShow p

instance Pretty Ty where
    pretty (TVar i) = pretty i
    pretty (TCon i) = pretty i <> "#"
    pretty (TIfc i) = pretty i <> "%"
    pretty (TNat n) = pretty n
    pretty (TApp t1 t2) = parens $ hsep $ map pretty (t1 : t2)
    pretty (TForall vars ty) = "forall" <+> hsep (map pretty vars) <> "." <+> pretty ty

    pretty (TArrow t1 t2) = parens $ pretty t1 <+> "->" <+> pretty t2
    pretty TUnit = "Unit"
    pretty TBool = "Bool"
    pretty (TReg t) = parens $ "Reg" <+> pretty t
    pretty (TBit t) = parens $ "Bit" <+> pretty t
    pretty (TModule t) = parens $ "Module" <+> pretty t

    pretty (TAlias i ty) = pretty i <> "@" <> pretty ty
    pretty (TLam is body) = parens $ "\\" <> hsep (map pretty is) <+> "->" <+> pretty body

    pretty t = viaShow t


printBSV :: Package -> Text
printBSV p = renderStrict $ layoutPretty defaultLayoutOptions $ pretty p

printAny p = renderStrict $ layoutPretty defaultLayoutOptions $ pretty p
