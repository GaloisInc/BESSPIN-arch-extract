{-# LANGUAGE DeriveDataTypeable #-}
module BESSPIN.ArchExtract.Verilog.AST
    ( module BESSPIN.ArchExtract.Verilog.AST
    , PortDir(..)
    , Edge(..)
    , BaseType(..)
    ) where

import Control.Monad

import Data.Data
import Data.Sequence (Seq)
import Data.Text (Text)

import BESSPIN.ArchExtract.Verilog.Raw (PortDir(..), Edge(..), BaseType(..))


-- Verilog AST.  This only supports the constructs we currently use for
-- architecture extraction.

data Design = Design
    { designModules :: Seq Module
    }
    deriving (Show, Data, Typeable)

data Module = Module
    { moduleName :: Text
    , moduleDecls :: Seq Decl
    -- Indices of `PortDecl`s in `moduleDecls`
    , modulePorts :: [Int]
    , moduleItems :: [Item]
    }
    deriving (Show, Data, Typeable)

data Decl =
    PortDecl
    { declName :: Text
    , portDeclTy :: Ty
    , portDeclDir :: PortDir
    } |
    ParamDecl
    { declName :: Text
    , paramDeclTy :: Ty
    } |
    VarDecl
    { declName :: Text
    , varDeclTy :: Ty
    } |
    TypedefDecl
    { declName :: Text
    , typedefDeclTy :: Ty
    } |
    InstDecl
    { declName :: Text
    , instanceModId :: Int
    , instanceParamVals :: [Expr]
    }
    deriving (Show, Data, Typeable)

data Ty =
    TTy
    { tWireBase :: BaseType
    , tWirePackedDims :: [Range]
    , tWireUnpackedDims :: [Range]
    } |
    TEnum
    { tEnumTy :: Ty
    -- TODO: variants (as paramdecl refs)
    } |
    TRef
    { tRefDeclId :: Int
    } |
    TInfer
    deriving (Show, Data, Typeable)

data Item =
    InitVar
    { initVarDeclId :: Int
    , initVarExpr :: Expr
    } |
    InitInst
    { initInstDeclId :: Int
    , initInstPortConns :: [Expr]
    } |
    ContAssign
    { contAssignLval :: Expr
    , contAssignRval :: Expr
    } |
    Always
    -- If `alwaysEvents` is empty, this is an `always @(*)` statement.
    { alwaysEvents :: [Event]
    , alwaysBody :: [Stmt]
    } |
    Initial
    { initialBody :: [Stmt]
    }
    deriving (Show, Data, Typeable)

data Stmt =
    If
    { ifCond :: Expr
    , ifThenBody :: [Stmt]
    , ifElseBody :: Maybe [Stmt]
    } |
    Case
    { caseCond :: Expr
    , caseCases :: [([Expr], [Stmt])]
    } |
    For
    { forInits :: [Stmt]
    , forCond :: Expr
    , forSteps :: [Stmt]
    , forBody :: [Stmt]
    } |
    NonBlockingAssign
    { nonBlockingAssignLval :: Expr
    , nonBlockingAssignRval :: Expr
    } |
    BlockingAssign
    { blockingAssignLval :: Expr
    , blockingAssignRval :: Expr
    } |
    BlockingUpdate
    { blockingUpdateLval :: Expr
    }
    deriving (Show, Data, Typeable)

data Expr =
    Var
    { varDeclId :: Int
    } |
    Param
    { paramDeclId :: Int
    } |
    Index
    { indexBase :: Expr
    , indexIndex :: Index
    } |
    MemIndex
    { memIndexBase :: Expr
    , memIndexIndexes :: [Index]
    } |
    Const
    { constText :: Text
    } |
    Concat
    { concatExprs :: [Expr]
    } |
    MultiConcat
    { multiConcatRep :: Expr
    , multiConcatExprs :: [Expr]
    } |
    IfExpr
    { ifExprCond :: Expr
    , ifExprThen :: Expr
    , ifExprElse :: Expr
    } |
    Unary
    { unaryArg :: Expr
    } |
    Binary
    { binaryLeft :: Expr
    , binaryRight :: Expr
    } |
    Field
    { fieldBase :: Expr
    , fieldName :: Text
    } |
    AssignPat
    { assignPatRepeat :: Expr
    , assignPatExprs :: [Expr]
    } |
    UnknownExpr
    {}
    deriving (Show, Eq, Data, Typeable)

data Event = Event
    { eventEdge :: Maybe Edge
    , eventVarDeclId :: Int
    }
    deriving (Show, Data, Typeable)

data Range = Range Expr Expr
    deriving (Show, Eq, Data, Typeable)

data Index = ISingle Expr | IRange Expr Expr
    deriving (Show, Eq, Data, Typeable)
