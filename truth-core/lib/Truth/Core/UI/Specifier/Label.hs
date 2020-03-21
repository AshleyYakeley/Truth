module Truth.Core.UI.Specifier.Label where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data LabelUISpec where
    MkLabelUISpec :: Subscriber (ROWUpdate Text) -> LabelUISpec

instance Show LabelUISpec where
    show (MkLabelUISpec _) = "label"

instance UIType LabelUISpec where
    uiWitness = $(iowitness [t|LabelUISpec|])

labelUISpec :: Subscriber (ROWUpdate Text) -> CVUISpec
labelUISpec sub = mkCVUISpec $ MkLabelUISpec sub
