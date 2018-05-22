module Truth.Core.UI.Specifier.Layout where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data UILayout seledit edit where
    MkUIHorizontal :: [(UISpec seledit edit, Bool)] -> UILayout seledit edit
    MkUIVertical :: [(UISpec seledit edit, Bool)] -> UILayout seledit edit

instance Show (UILayout seledit edit) where
    show (MkUIHorizontal specs) = "horizontal (" ++ intercalate ", " (fmap (show . fst) specs) ++ ")"
    show (MkUIVertical specs) = "vertical (" ++ intercalate ", " (fmap (show . fst) specs) ++ ")"

instance UIType UILayout where
    uiWitness = $(iowitness [t|UILayout|])

uiHorizontal :: [(UISpec seledit edit, Bool)] -> UISpec seledit edit
uiHorizontal specs = MkUISpec $ MkUIHorizontal specs

uiVertical :: [(UISpec seledit edit, Bool)] -> UISpec seledit edit
uiVertical specs = MkUISpec $ MkUIVertical specs
