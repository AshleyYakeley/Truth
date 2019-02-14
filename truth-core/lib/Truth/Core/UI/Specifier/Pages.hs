module Truth.Core.UI.Specifier.Pages where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data PagesUISpec sel edit where
    MkPagesUISpec :: [(UISpec sel' edit, UISpec sel edit)] -> PagesUISpec sel edit

instance Show (PagesUISpec sel edit) where
    show (MkPagesUISpec specs) = "pages (" ++ intercalate ", " (fmap show specs) ++ ")"

instance UIType PagesUISpec where
    uiWitness = $(iowitness [t|PagesUISpec|])

pagesUISpec :: forall edit sel sel'. [(UISpec sel' edit, UISpec sel edit)] -> UISpec sel edit
pagesUISpec pages = MkUISpec $ MkPagesUISpec pages
