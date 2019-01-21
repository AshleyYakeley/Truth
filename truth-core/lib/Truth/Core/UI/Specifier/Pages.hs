module Truth.Core.UI.Specifier.Pages where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data UIPages sel edit where
    MkUIPages :: [(UISpec sel' edit, UISpec sel edit)] -> UIPages sel edit

instance Show (UIPages sel edit) where
    show (MkUIPages specs) = "pages (" ++ intercalate ", " (fmap show specs) ++ ")"

instance UIType UIPages where
    uiWitness = $(iowitness [t|UIPages|])

uiPages :: forall edit sel sel'. [(UISpec sel' edit, UISpec sel edit)] -> UISpec sel edit
uiPages pages = MkUISpec $ MkUIPages pages
