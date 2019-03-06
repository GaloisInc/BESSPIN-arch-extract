{-# LANGUAGE TemplateHaskell #-}
module BESSPIN.ArchExtract.Constraints.Flat where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.Generics
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro
import Lens.Micro.Platform

import Debug.Trace

import BESSPIN.ArchExtract.Architecture
import BESSPIN.ArchExtract.Lens


data OverrideOrigin = OoLocal Int Int | OoInst Int Int Int
    deriving (Show, Eq, Ord)

data FlatConstraints = FlatConstraints
    { fcVars :: Seq Text
    , fcOverrides :: Seq (Text, OverrideOrigin)
    , fcConstraints :: Seq Constraint
    }
    deriving (Show)

makeLenses' ''FlatConstraints
