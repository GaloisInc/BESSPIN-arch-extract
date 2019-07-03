{-# LANGUAGE TemplateHaskell #-}
module BESSPIN.FeatureExtract.Verilog.Preprocess.Eval where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Data
import Data.Ix
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable
import Lens.Micro.Platform

import Debug.Trace

import BESSPIN.FeatureExtract.Verilog.Preprocess.Parser (Event(..), Branch(..))
import BESSPIN.ArchExtract.Lens


data S = S
    { sCurDefs :: Set Text
    , sUsed :: Set Text
    , sIncluded :: Set Text
    }

makeLenses' ''S

evalPp :: Set Text -> (FilePath -> [Event]) -> [Event] -> S
evalPp initDefs getInc evts = execState (mapM_ go evts) (S initDefs Set.empty Set.empty)
  where
    go :: Event -> State S ()
    go evt = case evt of
        TickRef d -> _sUsed %= Set.insert d
        Cond bs e -> doCond bs e
        Define d _ -> _sCurDefs %= Set.insert d
        Include p -> do _sIncluded %= Set.insert p; mapM_ go $ getInc $ T.unpack p
        OtherDirective _ -> return ()

    doCond :: [Branch] -> Maybe [Event] -> State S ()
    doCond (Branch d val evts : bs) e = do
        _sUsed %= Set.insert d
        curVal <- use $ _sCurDefs . to (Set.member d)
        if curVal == val then
            mapM_ go evts
        else
            doCond bs e
    doCond [] (Just evts) = mapM_ go evts
    doCond [] Nothing = return ()

evalPpUsed :: Set Text -> (FilePath -> [Event]) -> [Event] -> Set Text
evalPpUsed initDefs getInc evts = sUsed $ evalPp initDefs getInc evts
