module BESSPIN.ArchExtract.Gen.Clafer where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

import Debug.Trace

import Language.Clafer
import Language.Clafer.Common
import Language.Clafer.Front.AbsClafer hiding (Module)
import qualified Language.Clafer.Front.AbsClafer as C
import Language.Clafer.Front.PrintClafer
import qualified Language.Clafer.ClaferArgs as Args

import BESSPIN.ArchExtract.Verilog hiding (Module)
import qualified BESSPIN.ArchExtract.Verilog as V

convName x = T.unpack x ++ "_"

cfrModule :: Text -> [Element] -> Element
cfrModule name elts = Subclafer noSpan $ Clafer noSpan
    (Abstract noSpan)
    []
    (GCardEmpty noSpan)
    (mkIdent $ convName name)
    (SuperEmpty noSpan)
    (ReferenceEmpty noSpan)
    (CardEmpty noSpan)
    (InitEmpty noSpan)
    (TransitionEmpty noSpan)
    (ElementsList noSpan elts)

cfrModInst :: Text -> Text -> Element
cfrModInst modName instName = Subclafer noSpan $ Clafer noSpan
    (AbstractEmpty noSpan)
    []
    (GCardEmpty noSpan)
    (mkIdent $ convName instName)
    (SuperSome noSpan $ ClaferId noSpan $
        Path noSpan [ModIdIdent noSpan $ mkIdent $ convName modName])
    (ReferenceEmpty noSpan)
    (CardEmpty noSpan)
    (InitEmpty noSpan)
    (TransitionEmpty noSpan)
    (ElementsEmpty noSpan)



genClafer v = C.Module noSpan $ map (ElementDecl noSpan) $ map go v ++ [root]
  where
    modMap = M.fromList [(moduleId m, m) | m <- v]

    go (m@ModuleDecl {}) = cfrModule (moduleName m) (concatMap goItem $ moduleItems m)
    goItem (mi@Instance {}) = case M.lookup (instanceModId mi) modMap of
        Just m -> [cfrModInst (moduleName m) (instanceName mi)]
        Nothing -> trace ("warning: mod ref to unknown ID " ++ show (instanceModId mi)) []

    root = cfrModInst (T.pack "leg") (T.pack "root")

putAst a = do
    env <- getEnv
    putEnv env { cAst = Just a }

compiled ast = runClafer defaultClaferArgs { mode = [Args.Clafer] } $ do
    putAst ast
    iModule <- desugar Nothing
    compile iModule
    generate

