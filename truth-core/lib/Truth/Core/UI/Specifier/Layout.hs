module Truth.Core.UI.Specifier.Layout where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data LayoutUISpec sel edit where
    HorizontalUISpec :: [(UISpec sel edit, Bool)] -> LayoutUISpec sel edit
    VerticalUISpec :: [(UISpec sel edit, Bool)] -> LayoutUISpec sel edit

instance Show (LayoutUISpec sel edit) where
    show (HorizontalUISpec specs) = "horizontal (" ++ intercalate ", " (fmap (show . fst) specs) ++ ")"
    show (VerticalUISpec specs) = "vertical (" ++ intercalate ", " (fmap (show . fst) specs) ++ ")"

instance UIType LayoutUISpec where
    uiWitness = $(iowitness [t|LayoutUISpec|])

horizontalUISpec :: forall edit sel. [(UISpec sel edit, Bool)] -> UISpec sel edit
horizontalUISpec specs = MkUISpec $ HorizontalUISpec specs

verticalUISpec :: forall edit sel. [(UISpec sel edit, Bool)] -> UISpec sel edit
verticalUISpec specs = MkUISpec $ VerticalUISpec specs
