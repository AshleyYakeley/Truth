module Truth.Core.UI.Specifier.Pages where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data UIPages seledit edit where
    MkUIPages :: [(UISpec anyedit edit, UISpec seledit edit)] -> UIPages seledit edit

instance Show (UIPages seledit edit) where
    show (MkUIPages specs) = "pages (" ++ intercalate ", " (fmap show specs) ++ ")"

instance UIType UIPages where
    uiWitness = $(iowitness [t|UIPages|])

uiPages :: [(UISpec anyedit edit, UISpec seledit edit)] -> UISpec seledit edit
uiPages pages = MkUISpec $ MkUIPages pages
