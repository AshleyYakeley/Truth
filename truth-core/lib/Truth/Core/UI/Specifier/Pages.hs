module Truth.Core.UI.Specifier.Pages where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data PagesUISpec sel update where
    MkPagesUISpec :: [(UISpec sel' update, UISpec sel update)] -> PagesUISpec sel update

instance Show (PagesUISpec sel update) where
    show (MkPagesUISpec specs) = "pages (" ++ intercalate ", " (fmap show specs) ++ ")"

instance UIType PagesUISpec where
    uiWitness = $(iowitness [t|PagesUISpec|])

pagesUISpec :: forall update sel sel'. [(UISpec sel' update, UISpec sel update)] -> UISpec sel update
pagesUISpec pages = MkUISpec $ MkPagesUISpec pages
