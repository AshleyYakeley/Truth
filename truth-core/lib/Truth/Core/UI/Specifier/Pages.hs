module Truth.Core.UI.Specifier.Pages where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data PagesUISpec where
    MkPagesUISpec :: [(CVUISpec, CVUISpec)] -> PagesUISpec

instance Show PagesUISpec where
    show (MkPagesUISpec _) = "pages"

instance UIType PagesUISpec where
    uiWitness = $(iowitness [t|PagesUISpec|])

pagesUISpec :: [(CVUISpec, CVUISpec)] -> CVUISpec
pagesUISpec pages = mkCVUISpec $ MkPagesUISpec pages
