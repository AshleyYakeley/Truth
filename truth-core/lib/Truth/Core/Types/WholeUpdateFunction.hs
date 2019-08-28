module Truth.Core.Types.WholeUpdateFunction where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types.Whole

newtype WholeUpdateFunction edit a = MkWholeUpdateFunction
    { unWholeUpdateFunction :: UpdateFunction edit (WholeEdit a)
    }

instance Functor (WholeUpdateFunction edit) where
    fmap ab (MkWholeUpdateFunction efa) = MkWholeUpdateFunction $ funcUpdateFunction ab . efa

instance Applicative (WholeUpdateFunction edit) where
    pure a = MkWholeUpdateFunction $ constUpdateFunction a
    MkWholeUpdateFunction fab <*> MkWholeUpdateFunction fa =
        MkWholeUpdateFunction $ funcUpdateFunction (\(ab, a) -> ab a) . pairWholeUpdateFunction fab fa

instance IsoVariant (WholeUpdateFunction edit)

wholeUpdateFunctionReadOnlyEditLens :: WholeUpdateFunction edit a -> EditLens edit (WholeEdit a)
wholeUpdateFunctionReadOnlyEditLens (MkWholeUpdateFunction ef) = readOnlyEditLens ef
