module BESSPIN.ArchExtract.Verilog.Raw where

import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word


newtype NodeId = NodeId { unwrapNodeId :: Int }
    deriving (Show, Eq, Ord)

data Node =
    Module
    { moduleName :: Text
    , modulePorts :: [NodeId]
    , moduleParams :: [NodeId]
    , moduleItems :: [NodeId]
    } |
    DataDecl
    { dataDeclIds :: [NodeId]
    } |
    NetDecl
    { netDeclIds :: [NodeId]
    } |
    Variable
    { variableName :: Text
    -- Data type can be null for implicitly declared vars
    , variableDataType :: Maybe NodeId
    , variableDims :: Maybe NodeId
    , variableInit :: Maybe NodeId
    , variableDir :: Maybe PortDir
    } |
    ParamId
    { paramIdName :: Text
    , paramIdDataType :: Maybe NodeId
    , paramIdInit :: Maybe NodeId
    , paramIdDims :: Maybe NodeId
    } |
    DataType
    { dataTypeType :: BaseType
    , dataTypeSigned :: Bool
    , dataTypeDims :: Maybe NodeId
    } |
    TypeRef
    { typeRefDef :: NodeId
    } |
    TypeId
    { typeIdName :: Text
    , typeIdType :: NodeId
    } |
    Enum
    { enumBaseType :: Maybe NodeId
    , enumVariants :: Map Text NodeId
    } |
    ModuleInstantiation
    { moduleInstantiationModule :: NodeId
    , moduleInstantiationParamVals :: [NodeId]
    , moduleInstantiationIds :: [NodeId]
    } |
    InstId
    { instIdInstantiation :: NodeId
    , instIdName :: Text
    , instIdPortConns :: [NodeId]
    } |
    ContinuousAssign
    { continuousAssignIds :: [NodeId]
    } |
    NetRegAssign
    { netRegAssignLval :: NodeId
    , netRegAssignRval :: NodeId
    } |
    AlwaysConstruct
    { alwaysConstructKind :: AlwaysKind
    , alwaysConstructBody :: NodeId
    } |
    InitialConstruct
    { initialConstructBody :: NodeId
    } |
    EventExpression
    { eventExpressionEdge :: Maybe Edge
    , eventExpressionExpr :: NodeId
    } |

    SeqBlock
    { seqBlockDecls :: [NodeId]
    , seqBlockStmts :: [NodeId]
    } |
    EventControlStatement
    { eventControlStatementEvents :: [NodeId]
    , eventControlStatementBody :: NodeId
    } |
    ConditionalStatement
    { conditionalStatementCond :: NodeId
    , conditionalStatementThen :: NodeId
    , conditionalStatementElse :: Maybe NodeId
    } |
    CaseStatement
    { caseStatementCond :: NodeId
    , caseStatementItems :: [NodeId]
    } |
    CaseItem
    { caseItemExprs :: [NodeId]
    , caseItemBody :: NodeId
    } |
    For
    { forInits :: [NodeId]
    , forCond :: NodeId
    , forSteps :: [NodeId]
    , forBody :: NodeId
    } |
    NonBlockingAssign
    { nonBlockingAssignLval :: NodeId
    , nonBlockingAssignRval :: NodeId
    } |
    BlockingAssign
    { blockingAssignLval :: NodeId
    , blockingAssignRval :: NodeId
    } |
    BlockingAssignInPlace
    { blockingAssignLval :: NodeId
    } |
    DelayControlStatement
    { delayControlStatementBody :: NodeId
    } |
    NullStatement
    {} |

    IdRef
    { idRefDef :: NodeId
    } |
    IndexedId
    { indexedIdBase :: NodeId
    , indexedIdIndex :: NodeId
    } |
    IndexedMemoryId
    { indexedMemoryIdBase :: NodeId
    , indexedMemoryIdIndexes :: [NodeId]
    } |
    ConstVal
    { constValText :: Text
    } |
    IntVal
    { intValText :: Text
    } |
    RealVal
    { realValText :: Text
    } |
    Concat
    { concatExprs :: [NodeId]
    } |
    MultiConcat
    { multiConcatRepeat :: NodeId
    , multiConcatExprs :: [NodeId]
    } |
    QuestionColon
    { questionColonCond :: NodeId
    , questionColonThen :: NodeId
    , questionColonElse :: NodeId
    } |
    UnaryOperator
    { unaryOperatorOp :: UnOp
    , unaryOperatorArg :: NodeId
    } |
    BinaryOperator
    { binaryOperatorOp :: BinOp
    , binaryOperatorLeft :: NodeId
    , binaryOperatorRight :: NodeId
    } |
    SelectedName
    { selectedNameBase :: NodeId
    , selectedNameName :: Text
    } |
    MultiAssignmentPattern
    { multiAssignmentPatternRepeat :: NodeId
    , multiAssignmentPatternExprs :: [NodeId]
    } |
    Range
    { rangeLeft :: NodeId
    , rangeRight :: Maybe NodeId
    , rangeNext :: Maybe NodeId
    } |

    Dollar |

    Ignored
    { ignoredClass :: Text
    } |
    Unknown
    { unknownClass :: Text
    }
    deriving (Show)

data PortDir = Input | Output | InOut
    deriving (Show, Eq)

data AlwaysKind = AkPlain | AkComb | AkLatch | AkFf
    deriving (Show, Eq)

data Edge = PosEdge | NegEdge
    deriving (Show, Eq)

data BaseType = TLogic | TWire | TReg | TTri | TInt | TInteger | TString
    deriving (Show, Eq)

data UnOp =
    UNeg |
    UNot |
    ULogNot |
    UReduce BinOp |
    UReduceNot BinOp |
    UOther
    deriving (Show, Eq)

data BinOp =
    BAdd | BSub | BMul | BDiv | BMod |
    BAnd | BOr | BXor |
    BLogAnd | BLogOr |
    BEq | BNe | BLt | BLe | BGt | BGe |
    BShl | BShr |
    BOther
    deriving (Show, Eq)

data Span = Span !Word32 !Word32
    deriving (Show, Eq)

data FileInfo = FileInfo Text Span
    deriving (Show)
