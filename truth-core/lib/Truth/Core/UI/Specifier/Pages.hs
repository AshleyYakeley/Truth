module Truth.Core.UI.Specifier.Pages where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data UIPages edit where
    MkUIPages :: [(UISpec edit, UISpec edit)] -> UIPages edit

instance Show (UIPages edit) where
    show (MkUIPages specs) = "pages (" ++ intercalate ", " (fmap show specs) ++ ")"

instance UIType UIPages where
    uiWitness = $(iowitness [t|UIPages|])

uiPages :: [(UISpec edit, UISpec edit)] -> UISpec edit
uiPages pages = MkUISpec $ MkUIPages pages
