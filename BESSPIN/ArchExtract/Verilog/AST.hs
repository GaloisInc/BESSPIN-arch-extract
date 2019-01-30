module BESSPIN.ArchExtract.Verilog.AST
    ( module BESSPIN.ArchExtract.Verilog.AST
    , PortDir(..)
    , Edge(..)
    ) where

import Control.Monad

import Data.Sequence (Seq)
import Data.Text (Text)

import BESSPIN.ArchExtract.Verilog.Raw (PortDir(..), Edge(..))


-- Verilog AST.  This only supports the constructs we currently use for
-- architecture extraction.  For example, it doesn't store any type
-- information.

data Design = Design
    { designModules :: Seq Module
    }
    deriving (Show)

data Module = Module
    { moduleName :: Text
    , moduleDecls :: Seq Decl
    -- Indices of `PortDecl`s in `moduleDecls`
    , modulePorts :: [Int]
    , moduleItems :: [Item]
    }
    deriving (Show)

data Decl =
    PortDecl
    { declName :: Text
    , portDeclDir :: PortDir
    } |
    ParamDecl
    { declName :: Text
    } |
    VarDecl
    { declName :: Text
    } |
    InstDecl
    { declName :: Text
    , instanceModId :: Int
    , instanceParamVals :: [Expr]
    }
    deriving (Show)

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
    deriving (Show)

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
    deriving (Show)

data Expr =
    Var
    { varDeclId :: Int
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
    deriving (Show)

data Event = Event
    { eventEdge :: Maybe Edge
    , eventVarDeclId :: Int
    }
    deriving (Show)

data Index = ISingle Expr | IRange Expr Expr
    deriving (Show)
