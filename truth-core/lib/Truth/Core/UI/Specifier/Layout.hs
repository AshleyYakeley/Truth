module Truth.Core.UI.Specifier.Layout where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data UILayout sel edit where
    MkUIHorizontal :: [(UISpec sel edit, Bool)] -> UILayout sel edit
    MkUIVertical :: [(UISpec sel edit, Bool)] -> UILayout sel edit

instance Show (UILayout sel edit) where
    show (MkUIHorizontal specs) = "horizontal (" ++ intercalate ", " (fmap (show . fst) specs) ++ ")"
    show (MkUIVertical specs) = "vertical (" ++ intercalate ", " (fmap (show . fst) specs) ++ ")"

instance UIType UILayout where
    uiWitness = $(iowitness [t|UILayout|])

uiHorizontal :: forall edit sel. [(UISpec sel edit, Bool)] -> UISpec sel edit
uiHorizontal specs = MkUISpec $ MkUIHorizontal specs

uiVertical :: forall edit sel. [(UISpec sel edit, Bool)] -> UISpec sel edit
uiVertical specs = MkUISpec $ MkUIVertical specs
