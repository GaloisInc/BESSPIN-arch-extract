module BESSPIN.ArchExtract.Lens where

import Language.Haskell.TH.Syntax

import Lens.Micro.Platform

myLensRules = lensRules
    & lensField .~ (\_ _ n -> [TopName $ mkName $ '_' : nameBase n])

makeLenses' name = makeLensesWith myLensRules name
