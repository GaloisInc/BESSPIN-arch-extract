-- Verilog AST and parsing from CBOR
module BESSPIN.ArchExtract.Verilog where

import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T


-- Verilog AST

type NodeId = Word

data ModuleDecl = ModuleDecl
    { moduleId :: NodeId
    , moduleName :: Text
    , moduleParams :: [ParamDecl]
    , modulePorts :: [PortDecl]
    , moduleItems :: [ModItem]
    }
    deriving (Show)

data ModItem =
    Instance
    { modItemId :: NodeId
    , instanceModId :: NodeId
    , instanceName :: Text
    } |
    VarDecl
    { modItemId :: NodeId
    , varDeclName :: Text
    , varDeclDir :: Integer
    } |
    ContAssign
    { modItemId :: NodeId
    } |
    Always
    { alwaysStmt :: Stmt
    } |
    Initial
    { initialStmt :: Stmt
    }
    deriving (Show)

data Stmt =
    Block
    { blockStmts :: [Stmt]
    } |
    If
    { ifCond :: Expr
    , ifThenBody :: Stmt
    , ifElseBody :: Maybe Stmt
    } |
    Case
    { caseCond :: Expr
    , caseCases :: [([Expr], Stmt)]
    } |
    For
    { forBody :: Stmt
    } |
    NonBlockingAssign
    { nonBlockingAssignLval :: Expr
    , nonBlockingAssignRval :: Expr
    } |
    BlockingAssign
    { blockingAssignLval :: Expr
    , blockingAssignRval :: Expr
    } |
    UnknownStmt
    deriving (Show)

data Expr =
    Var
    { varDefId :: NodeId
    } |
    Index
    { indexBase :: Expr
    , indexIndex :: Expr
    } |
    Const
    { constText :: Text
    } |
    Concat
    { concatExprs :: [Expr]
    } |
    IfExpr
    { ifExprCond :: Expr
    , ifExprThen :: Expr
    , ifExprElse :: Expr
    } |
    UnknownExpr
    deriving (Show)

data ParamDecl = ParamDecl
    { paramDeclId :: NodeId
    }
    deriving (Show)

data PortDecl = PortDecl
    { portDeclId :: NodeId
    , portDeclName :: Text
    , portDeclDir :: Integer
    }
    deriving (Show)
