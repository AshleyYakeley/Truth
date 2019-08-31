module Truth.Core.UI.Specifier.Layout where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data LayoutUISpec sel update where
    HorizontalUISpec :: [(UISpec sel update, Bool)] -> LayoutUISpec sel update
    VerticalUISpec :: [(UISpec sel update, Bool)] -> LayoutUISpec sel update

instance Show (LayoutUISpec sel update) where
    show (HorizontalUISpec specs) = "horizontal (" ++ intercalate ", " (fmap (show . fst) specs) ++ ")"
    show (VerticalUISpec specs) = "vertical (" ++ intercalate ", " (fmap (show . fst) specs) ++ ")"

instance UIType LayoutUISpec where
    uiWitness = $(iowitness [t|LayoutUISpec|])

horizontalUISpec :: forall update sel. [(UISpec sel update, Bool)] -> UISpec sel update
horizontalUISpec specs = MkUISpec $ HorizontalUISpec specs

verticalUISpec :: forall update sel. [(UISpec sel update, Bool)] -> UISpec sel update
verticalUISpec specs = MkUISpec $ VerticalUISpec specs
