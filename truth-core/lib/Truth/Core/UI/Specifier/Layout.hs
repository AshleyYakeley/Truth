module Truth.Core.UI.Specifier.Layout where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data LayoutUISpec where
    HorizontalUISpec :: [(CVUISpec, Bool)] -> LayoutUISpec
    VerticalUISpec :: [(CVUISpec, Bool)] -> LayoutUISpec

instance Show LayoutUISpec where
    show (HorizontalUISpec _) = "horizontal layout"
    show (VerticalUISpec _) = "vertical layout"

instance UIType LayoutUISpec where
    uiWitness = $(iowitness [t|LayoutUISpec|])

horizontalUISpec :: [(CVUISpec, Bool)] -> CVUISpec
horizontalUISpec specs = mkCVUISpec $ HorizontalUISpec specs

verticalUISpec :: [(CVUISpec, Bool)] -> CVUISpec
verticalUISpec specs = mkCVUISpec $ VerticalUISpec specs
