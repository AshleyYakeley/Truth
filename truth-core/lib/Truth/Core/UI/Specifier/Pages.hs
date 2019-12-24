module Truth.Core.UI.Specifier.Pages where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data PagesUISpec sel where
    MkPagesUISpec :: [(UISpec sel', UISpec sel)] -> PagesUISpec sel

instance Show (PagesUISpec sel) where
    show (MkPagesUISpec specs) = "pages (" ++ intercalate ", " (fmap show specs) ++ ")"

instance UIType PagesUISpec where
    uiWitness = $(iowitness [t|PagesUISpec|])

pagesUISpec :: forall sel sel'. [(UISpec sel', UISpec sel)] -> UISpec sel
pagesUISpec pages = MkUISpec $ MkPagesUISpec pages
