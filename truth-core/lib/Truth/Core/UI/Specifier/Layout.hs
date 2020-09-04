module Truth.Core.UI.Specifier.Layout where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data LayoutUISpec where
    HorizontalUISpec :: [(Bool, CVUISpec)] -> LayoutUISpec
    VerticalUISpec :: [(Bool, CVUISpec)] -> LayoutUISpec

instance Show LayoutUISpec where
    show (HorizontalUISpec _) = "horizontal layout"
    show (VerticalUISpec _) = "vertical layout"

instance UIType LayoutUISpec where
    uiWitness = $(iowitness [t|LayoutUISpec|])

horizontalUISpec :: [(Bool, CVUISpec)] -> CVUISpec
horizontalUISpec specs = mkCVUISpec $ HorizontalUISpec specs

verticalUISpec :: [(Bool, CVUISpec)] -> CVUISpec
verticalUISpec specs = mkCVUISpec $ VerticalUISpec specs
