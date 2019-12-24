module Truth.Core.UI.Specifier.Layout where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data LayoutUISpec sel where
    HorizontalUISpec :: [(UISpec sel, Bool)] -> LayoutUISpec sel
    VerticalUISpec :: [(UISpec sel, Bool)] -> LayoutUISpec sel

instance Show (LayoutUISpec sel) where
    show (HorizontalUISpec specs) = "horizontal (" ++ intercalate ", " (fmap (show . fst) specs) ++ ")"
    show (VerticalUISpec specs) = "vertical (" ++ intercalate ", " (fmap (show . fst) specs) ++ ")"

instance UIType LayoutUISpec where
    uiWitness = $(iowitness [t|LayoutUISpec|])

horizontalUISpec :: forall sel. [(UISpec sel, Bool)] -> UISpec sel
horizontalUISpec specs = MkUISpec $ HorizontalUISpec specs

verticalUISpec :: forall sel. [(UISpec sel, Bool)] -> UISpec sel
verticalUISpec specs = MkUISpec $ VerticalUISpec specs
