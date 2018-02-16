module Truth.Core.UI.Layout where

import Truth.Core.Import
import Truth.Core.UI.Specifier

data UILayout edit where
    MkUIHorizontal :: [(UISpec edit, Bool)] -> UILayout edit
    MkUIVertical :: [(UISpec edit, Bool)] -> UILayout edit

instance Show (UILayout edit) where
    show (MkUIHorizontal specs) = "horizontal (" ++ intercalate ", " (fmap (show . fst) specs) ++ ")"
    show (MkUIVertical specs) = "vertical (" ++ intercalate ", " (fmap (show . fst) specs) ++ ")"

instance UIType UILayout where
    uiWitness = $(iowitness [t|UILayout|])

uiHorizontal :: [(UISpec edit, Bool)] -> UISpec edit
uiHorizontal specs = MkUISpec $ MkUIHorizontal specs

uiVertical :: [(UISpec edit, Bool)] -> UISpec edit
uiVertical specs = MkUISpec $ MkUIVertical specs
