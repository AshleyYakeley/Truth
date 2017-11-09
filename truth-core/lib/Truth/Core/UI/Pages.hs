module Truth.Core.UI.Pages where

import Truth.Core.Import
import Truth.Core.UI.Specifier

data UIPages edit where
    MkUIPages :: [(Text, UISpec edit)] -> UIPages edit

instance Show (UIPages edit) where
    show (MkUIPages specs) = "pages (" ++ intercalate ", " (fmap show specs) ++ ")"

instance UIType UIPages where
    uiWitness = $(iowitness [t|UIPages|])

uiPages :: [(Text, UISpec edit)] -> UISpec edit
uiPages pages = MkUISpec $ MkUIPages pages
