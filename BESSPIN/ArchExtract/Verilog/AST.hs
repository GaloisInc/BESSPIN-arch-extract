{-# LANGUAGE DeriveDataTypeable, PatternSynonyms #-}
module BESSPIN.ArchExtract.Verilog.AST
    ( module BESSPIN.ArchExtract.Verilog.AST
    , PortDir(..)
    , Edge(..)
    , BaseType(..)
    , UnOp(..)
    , BinOp(..)
    , Span(..)
    ) where

import Control.Monad

import Data.Data
import Data.Sequence (Seq)
import Data.Text (Text)

import BESSPIN.ArchExtract.Verilog.Raw
    ( PortDir(..)
    , Edge(..)
    , BaseType(..)
    , UnOp(..)
    , BinOp(..)
    , Span(..)
    )


-- Verilog AST.  This only supports the constructs we currently use for
-- architecture extraction.


data Design = Design
    { designModules :: Seq Module
    }
    deriving (Show, Data, Typeable)

data Module = Module'
    { moduleName :: Text
    , moduleDecls :: Seq Decl
    -- Indices of `PortDecl`s in `moduleDecls`
    , modulePorts :: Seq Int
    -- Indices of `ParamDecl`s in `moduleDecls`
    , moduleParams :: Seq Int
    , moduleItems :: Seq Item
    , moduleSpan :: Span
    }
    deriving (Show, Data, Typeable)

data Decl =
    PortDecl'
    { declName :: Text
    , portDeclTy :: Ty
    , portDeclDir :: PortDir
    , declSpan :: Span
    } |
    ParamDecl'
    { declName :: Text
    , paramDeclTy :: Ty
    , paramDeclInit :: Maybe Expr
    , declSpan :: Span
    } |
    VarDecl'
    { declName :: Text
    , varDeclTy :: Ty
    , declSpan :: Span
    } |
    TypedefDecl'
    { declName :: Text
    , typedefDeclTy :: Ty
    , declSpan :: Span
    } |
    InstDecl'
    { declName :: Text
    , instanceModId :: Int
    , instanceParamVals :: Seq (Maybe Expr)
    , declSpan :: Span
    }
    deriving (Show, Data, Typeable)

data Ty =
    TTy'
    { tWireBase :: BaseType
    , tWirePackedDims :: [Range]
    , tWireUnpackedDims :: [Range]
    , tySpan :: Span
    } |
    TEnum'
    { tEnumTy :: Ty
    -- Indexes of variants' `ParamDecl`s in `moduleDecls`.
    , tEnumVariants :: [Int]
    , tySpan :: Span
    } |
    TRef'
    { tRefDeclId :: Int
    , tySpan :: Span
    }
    deriving (Show, Data, Typeable)

data Item =
    InitVar'
    { initVarDeclId :: Int
    , initVarExpr :: Expr
    , itemSpan :: Span
    } |
    InitInst'
    { initInstDeclId :: Int
    , initInstPortConns :: Seq Expr
    , itemSpan :: Span
    } |
    ContAssign'
    { contAssignLval :: Expr
    , contAssignRval :: Expr
    , itemSpan :: Span
    } |
    Always'
    -- If `alwaysEvents` is empty, this is an `always @(*)` statement.
    { alwaysEvents :: [Event]
    , alwaysBody :: [Stmt]
    , itemSpan :: Span
    } |
    Initial'
    { initialBody :: [Stmt]
    , itemSpan :: Span
    }
    deriving (Show, Data, Typeable)

data Stmt =
    If'
    { ifCond :: Expr
    , ifThenBody :: [Stmt]
    , ifElseBody :: Maybe [Stmt]
    , stmtSpan :: Span
    } |
    Case'
    { caseCond :: Expr
    , caseCases :: [([Expr], [Stmt])]
    , stmtSpan :: Span
    } |
    For'
    { forInits :: [Stmt]
    , forCond :: Expr
    , forSteps :: [Stmt]
    , forBody :: [Stmt]
    , stmtSpan :: Span
    } |
    NonBlockingAssign'
    { nonBlockingAssignLval :: Expr
    , nonBlockingAssignRval :: Expr
    , stmtSpan :: Span
    } |
    BlockingAssign'
    { blockingAssignLval :: Expr
    , blockingAssignRval :: Expr
    , stmtSpan :: Span
    } |
    BlockingUpdate'
    { blockingUpdateLval :: Expr
    , stmtSpan :: Span
    }
    deriving (Show, Data, Typeable)

data Expr =
    Var'
    { varDeclId :: Int
    , exprSpan :: Span
    } |
    Param'
    { paramDeclId :: Int
    , exprSpan :: Span
    } |
    Index'
    { indexBase :: Expr
    , indexIndex :: Index
    , exprSpan :: Span
    } |
    MemIndex'
    { memIndexBase :: Expr
    , memIndexIndexes :: [Index]
    , exprSpan :: Span
    } |
    Const'
    { constText :: Text
    , exprSpan :: Span
    } |
    ConstInt'
    { constText :: Text
    , constIntVal :: Int
    , exprSpan :: Span
    } |
    ConstBool'
    { constText :: Text
    , constBoolVal :: Bool
    , exprSpan :: Span
    } |
    Concat'
    { concatExprs :: [Expr]
    , exprSpan :: Span
    } |
    MultiConcat'
    { multiConcatRep :: Expr
    , multiConcatExprs :: [Expr]
    , exprSpan :: Span
    } |
    IfExpr'
    { ifExprCond :: Expr
    , ifExprThen :: Expr
    , ifExprElse :: Expr
    , exprSpan :: Span
    } |
    Unary'
    { unaryOp :: UnOp
    , unaryArg :: Expr
    , exprSpan :: Span
    } |
    Binary'
    { binaryOp :: BinOp
    , binaryLeft :: Expr
    , binaryRight :: Expr
    , exprSpan :: Span
    } |
    Field'
    { fieldBase :: Expr
    , fieldName :: Text
    , exprSpan :: Span
    } |
    AssignPat'
    { assignPatRepeat :: Expr
    , assignPatExprs :: [Expr]
    , exprSpan :: Span
    } |
    Builtin'
    { builtinKind :: BuiltinKind
    , builtinArgs :: [Expr]
    , exprSpan :: Span
    } |
    UnknownExpr'
    { exprSpan :: Span
    }
    deriving (Show, Eq, Data, Typeable)

data Event = Event'
    { eventEdge :: Maybe Edge
    , eventVarDeclId :: Int
    , eventSpan :: Span
    }
    deriving (Show, Data, Typeable)


pattern Module a b c d e <- Module' a b c d e _

pattern PortDecl a b c <- PortDecl' a b c _
pattern ParamDecl a b c <- ParamDecl' a b c _
pattern VarDecl a b <- VarDecl' a b _
pattern TypedefDecl a b <- TypedefDecl' a b _
pattern InstDecl a b c <- InstDecl' a b c _

pattern TTy a b c <- TTy' a b c _
pattern TEnum a b <- TEnum' a b _
pattern TRef a <- TRef' a _

pattern InitVar a b <- InitVar' a b _
pattern InitInst a b <- InitInst' a b _
pattern ContAssign a b <- ContAssign' a b _
pattern Always a b <- Always' a b _
pattern Initial a <- Initial' a _

pattern If a b c <- If' a b c _
pattern Case a b <- Case' a b _
pattern For a b c d <- For' a b c d _
pattern NonBlockingAssign a b <- NonBlockingAssign' a b _
pattern BlockingAssign a b <- BlockingAssign' a b _
pattern BlockingUpdate a <- BlockingUpdate' a _

pattern Var a <- Var' a _
pattern Param a <- Param' a _
pattern Index a b <- Index' a b _
pattern MemIndex a b <- MemIndex' a b _
pattern Const a <- Const' a _
pattern ConstInt a b <- ConstInt' a b _
pattern ConstBool a b <- ConstBool' a b _
pattern Concat a <- Concat' a _
pattern MultiConcat a b <- MultiConcat' a b _
pattern IfExpr a b c <- IfExpr' a b c _
pattern Unary a b <- Unary' a b _
pattern Binary a b c <- Binary' a b c _
pattern Field a b <- Field' a b _
pattern AssignPat a b <- AssignPat' a b _
pattern Builtin a b <- Builtin' a b _
pattern UnknownExpr <- UnknownExpr' _

pattern Event a b <- Event' a b _


class HasSpan a where
    spanOf :: a -> Span

dummySpan = Span 0 0

instance HasSpan Module where
    spanOf = moduleSpan

instance HasSpan Decl where
    spanOf = declSpan

instance HasSpan Ty where
    spanOf = tySpan

instance HasSpan Item where
    spanOf = itemSpan

instance HasSpan Stmt where
    spanOf = stmtSpan

instance HasSpan Expr where
    spanOf = exprSpan

instance HasSpan Event where
    spanOf = eventSpan


data Range = Range Expr Expr
    deriving (Show, Eq, Data, Typeable)

data Index = ISingle Expr | IRange Expr Expr
    deriving (Show, Eq, Data, Typeable)


data BuiltinKind = BkClog2 | BkSize
    deriving (Show, Eq, Data, Typeable)
