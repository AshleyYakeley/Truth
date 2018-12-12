module Truth.Core.Types.WholeEditFunction where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types.Whole

newtype WholeEditFunction edit a = MkWholeEditFunction
    { unWholeEditFunction :: EditFunction edit (WholeEdit a)
    }

instance Functor (WholeEditFunction edit) where
    fmap ab (MkWholeEditFunction efa) = MkWholeEditFunction $ funcEditFunction ab . efa

instance Applicative (WholeEditFunction edit) where
    pure a = MkWholeEditFunction $ constEditFunction a
    MkWholeEditFunction fab <*> MkWholeEditFunction fa =
        MkWholeEditFunction $ funcEditFunction (\(ab, a) -> ab a) . pairWholeEditFunction fab fa

instance IsoVariant (WholeEditFunction edit)
