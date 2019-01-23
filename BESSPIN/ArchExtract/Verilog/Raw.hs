module BESSPIN.ArchExtract.Verilog.Raw where

import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T


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
    , variableDims :: Maybe NodeId
    , variableInit :: Maybe NodeId
    , variableDir :: Maybe PortDir
    } |
    ParamId
    { paramIdName :: Text
    , paramIdInit :: Maybe NodeId
    , paramIdDims :: Maybe NodeId
    } |
    DataType
    { dataTypeDims :: Maybe NodeId
    } |
    TypeId
    {} |
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
    { alwaysConstructBody :: NodeId
    } |
    InitialConstruct
    { initialConstructBody :: NodeId
    } |

    SeqBlock
    { seqBlockDecls :: [NodeId]
    , seqBlockStmts :: [NodeId]
    } |
    EventControlStatement
    { eventControlStatementBody :: NodeId
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
    { unaryOperatorArg :: NodeId
    } |
    BinaryOperator
    { binaryOperatorLeft :: NodeId
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
    } |

    Ignored
    { ignoredClass :: Text
    } |
    Unknown
    { unknownClass :: Text
    }
    deriving (Show)

data PortDir = Input | Output | InOut
    deriving (Show, Eq)
