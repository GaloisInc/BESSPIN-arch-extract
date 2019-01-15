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
    , instanceParamVals :: [Expr]
    , instancePortConns :: [PortConn]
    } |
    VarDecl
    { modItemId :: NodeId
    , varDeclName :: Text
    , varDeclDims :: Maybe Index
    , varDeclDir :: Integer
    , varDeclInit :: Maybe Expr
    } |
    ContAssign
    { modItemId :: NodeId
    , contAssignLval :: Expr
    , contAssignRval :: Expr
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
    { forInits :: [Stmt]
    , forCond :: Expr
    , forSteps :: [Stmt]
    , forBody :: Stmt
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
    , blockingUpdateOper :: Integer
    } |
    NullStmt |
    UnknownStmt
    deriving (Show)

data Expr =
    Var
    { varDefId :: NodeId
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
    { unaryOper :: Integer
    , unaryArg :: Expr
    } |
    Binary
    { binaryOper :: Integer
    , binaryLeft :: Expr
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
    deriving (Show)

data Index = ISingle Expr | IRange Expr Expr
    deriving (Show)

data ParamDecl = ParamDecl
    { paramDeclId :: NodeId
    , paramDeclDims :: Maybe Index
    , paramDeclInit :: Maybe Expr
    }
    deriving (Show)

data PortDecl = PortDecl
    { portDeclId :: NodeId
    , portDeclName :: Text
    , portDeclDims :: Maybe Index
    , portDeclDir :: Integer
    }
    deriving (Show)

data PortConn =
    PCPositional Expr |
    PCNamed Text Expr |
    PCGlob
    deriving (Show)
