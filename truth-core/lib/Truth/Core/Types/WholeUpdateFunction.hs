module Truth.Core.Types.WholeUpdateFunction where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types.Whole

newtype WholeUpdateFunction update a = MkWholeUpdateFunction
    { unWholeUpdateFunction :: UpdateFunction update (WholeUpdate a)
    }

instance Functor (WholeUpdateFunction update) where
    fmap ab (MkWholeUpdateFunction efa) = MkWholeUpdateFunction $ funcUpdateFunction ab . efa

instance Applicative (WholeUpdateFunction update) where
    pure a = MkWholeUpdateFunction $ constUpdateFunction a
    MkWholeUpdateFunction fab <*> MkWholeUpdateFunction fa =
        MkWholeUpdateFunction $ funcUpdateFunction (\(ab, a) -> ab a) . pairWholeUpdateFunction fab fa

instance IsoVariant (WholeUpdateFunction update)

wholeUpdateFunctionReadOnlyEditLens :: WholeUpdateFunction update a -> EditLens update (WholeUpdate a)
wholeUpdateFunctionReadOnlyEditLens (MkWholeUpdateFunction ef) = readOnlyEditLens ef
