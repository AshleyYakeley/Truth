module Truth.Core.UI.Specifier.Pages where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data PagesUISpec sel where
    MkPagesUISpec :: [(LUISpec sel', LUISpec sel)] -> PagesUISpec sel

instance Show (PagesUISpec sel) where
    show (MkPagesUISpec _) = "pages"

instance UIType PagesUISpec where
    uiWitness = $(iowitness [t|PagesUISpec|])

pagesUISpec :: forall sel sel'. [(LUISpec sel', LUISpec sel)] -> LUISpec sel
pagesUISpec pages = mkLUISpec $ MkPagesUISpec pages
