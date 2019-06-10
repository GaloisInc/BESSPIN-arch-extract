{-# LANGUAGE OverloadedStrings #-}
module BESSPIN.ArchExtract.Chisel.FIRRTL.Print where

import Data.Foldable
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc hiding (SEmpty)
import Data.Text.Prettyprint.Doc.Render.Text

import BESSPIN.ArchExtract.Chisel.FIRRTL.AST


instance Pretty Circuit where
    pretty (Circuit _ mods main) =
        vsep $ punctuate line $ map pretty mods ++ ["main =" <+> pretty main <> semi]

instance Pretty Module where
    pretty (Module _ name ports (MkNormal body)) = vsep
        [ "module" <+> pretty name <> lparen
        , indent 4 $ vsep $ map (<> comma) $ map pretty ports
        , rparen <+> lbrace
        , indent 4 $ vsep $ map pretty $ getStmts body
        , rbrace
        ]
    pretty (Module _ name ports (MkExtern defName)) = vsep
        [ "module" <+> pretty name <> lparen
        , indent 4 $ vsep $ map (<> comma) $ map pretty ports
        , rparen <+> equals <+> "extern" <+> pretty defName <> semi
        ]

instance Pretty Port where
    pretty (Port _ name dir ty) =
        pretty dir <+> pretty name <> colon <+> pretty ty

instance Pretty Direction where
    pretty Input = "input"
    pretty Output = "output"

instance Pretty Ty where
    pretty (TUInt w) = "u" <> pretty w
    pretty (TSInt w) = "s" <> pretty w
    pretty (TFixed w p) = "f" <> pretty w <> "." <> pretty p
    pretty (TBundle fs) = vsep 
        [ lbrace
        , indent 4 $ vsep $ punctuate comma $ map pretty fs
        , rbrace
        ]
    pretty (TVector t len) = pretty t <> brackets (pretty len)
    pretty TClock = "clk"
    pretty (TAnalog w) = "a" <> pretty w
    pretty TUnknown = "?"

instance Pretty Width where
    pretty (WInt i) = pretty i
    pretty WUnknown = "?"

instance Pretty Field where
    pretty (Field name ty False) = pretty name <> colon <+> pretty ty
    pretty (Field name ty True) = "flip" <+> pretty name <> colon <+> pretty ty
        

getStmts (SBlock stmts) = stmts
getStmts s = [s]

atExpr2 clk en = "@" <> parens (pretty clk <> comma <+> pretty en)

instance Pretty Stmt where
    pretty (SDef _ d) = pretty d
    pretty (SCond _ cond then_ else_)
      | not $ null $ getStmts else_ = vsep
        [ "if" <+> pretty cond <+> lbrace
        , indent 4 $ vsep $ map pretty $ getStmts then_
        , rbrace
        ]
      | otherwise = vsep
        [ "if" <+> pretty cond <+> lbrace
        , indent 4 $ vsep $ map pretty $ getStmts then_
        , rbrace <+> "else" <+> lbrace
        , indent 4 $ vsep $ map pretty $ getStmts else_
        , rbrace
        ]
    pretty (SBlock stmts) = vsep [ lbrace, indent 4 $ vsep $ map pretty stmts, rbrace ]
    pretty (SPartialConnect _ loc expr) = pretty loc <+> "<-" <+> pretty expr <> semi
    pretty (SConnect _ loc expr) = pretty loc <+> "<=" <+> pretty expr <> semi
    pretty (SIsInvalid _ e) = "invalid" <+> pretty e
    pretty (SAttach _ es) =
        "attach" <+> hsep (punctuate comma $ map pretty es) <> semi
    pretty (SStop _ ret clk en) =
        "stop" <> atExpr2 clk en <+> pretty ret <> semi
    pretty (SPrint _ str args clk en) =
        "print" <> atExpr2 clk en <+>
            hsep (punctuate comma $ viaShow str : map pretty args) <> semi
    pretty SEmpty = "skip" <> semi

instance Pretty Def where
    pretty (DWire name ty) =
        "wire" <+> pretty name <> colon <+> pretty ty <> semi
    pretty (DReg name ty clk reset init) =
        "reg" <> atExpr2 clk reset <+> pretty name <> colon <+> pretty ty <+>
            equals <+> pretty init <> semi
    pretty (DInst name modName) =
        "inst" <+> pretty name <> colon <+> pretty modName <> semi
    pretty (DMemory name ty depth rds wrs rdwrs) = vsep
        [ "mem" <+> pretty name <> colon <+> pretty ty <+> "*" <+> pretty depth <+> lbrace
        , vsep $ map (\x -> "read" <+> pretty x <> semi) rds
        , vsep $ map (\x -> "write" <+> pretty x <> semi) wrs
        , vsep $ map (\x -> "readwrite" <+> pretty x <> semi) rdwrs
        , rbrace
        ]
    pretty (DNode name expr) =
        "node" <+> pretty name <+> equals <+> pretty expr <> semi
    pretty (DCMem name ty depth seq) =
        "mem" <> parens (if seq then "seq" else "comb") <+> pretty name <> colon <+>
            pretty ty <+> "*" <+> pretty depth <> semi
    pretty (DCMemPort name ty memName exprs dir) =
        "memport" <+> pretty name <> colon <+> pretty ty <+> equals <+>
            pretty dir <+> pretty memName <>
            parens (hsep $ punctuate comma $ map pretty exprs) <> semi

instance Pretty MemPortDir where
    pretty MpdInfer = "_"
    pretty MpdRead = "read"
    pretty MpdWrite = "write"
    pretty MpdReadWrite = "readwrite"

instance Pretty Expr where
    pretty (ELit l) = pretty l
    pretty (ERef name _) = pretty name
    pretty (EField e name _) = pretty e <> dot <> pretty name
    pretty (EIndex a i _) = pretty a <> brackets (pretty i)
    pretty (EIndexC a i _) = pretty a <> brackets (pretty i)
    pretty (EMux c t e _) = "mux" <> parens (hsep $ punctuate comma $ map pretty [c, t, e])
    pretty (EValidIf c t _) = "mux" <> parens (hsep $ punctuate comma $ [pretty c, pretty t, "?"])
    pretty (EPrim op args consts _) =
        pretty op <> parens (hsep $ punctuate comma $ map pretty args ++ map pretty consts)

instance Pretty Lit where
    pretty (LUInt x w) = pretty x <> "_u" <> pretty w
    pretty (LSInt x w) = pretty x <> "_s" <> pretty w
    pretty (LFixed x w p) = pretty x <> "_f" <> pretty w <> dot <> pretty p


printFIRRTL :: Circuit -> Text
printFIRRTL p = renderStrict $ layoutPretty defaultLayoutOptions $ pretty p

printAny p = renderStrict $ layoutPretty defaultLayoutOptions $ pretty p
