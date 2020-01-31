module Truth.Core.UI.Specifier.Layout where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data LayoutUISpec sel where
    HorizontalUISpec :: [(LUISpec sel, Bool)] -> LayoutUISpec sel
    VerticalUISpec :: [(LUISpec sel, Bool)] -> LayoutUISpec sel

instance Show (LayoutUISpec sel) where
    show (HorizontalUISpec _) = "horizontal layout"
    show (VerticalUISpec _) = "vertical layout"

instance UIType LayoutUISpec where
    uiWitness = $(iowitness [t|LayoutUISpec|])

horizontalUISpec :: forall sel. [(LUISpec sel, Bool)] -> LUISpec sel
horizontalUISpec specs = mkLUISpec $ HorizontalUISpec specs

verticalUISpec :: forall sel. [(LUISpec sel, Bool)] -> LUISpec sel
verticalUISpec specs = mkLUISpec $ VerticalUISpec specs
