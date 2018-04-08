module Truth.Core.UI.Specifier.Option where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UIOption edit where
    MkUIOption
        :: Eq t
        => EditFunction tedit (ListEdit [(t, Text)] (WholeEdit (t, Text)))
        -> EditLens tedit (WholeEdit t)
        -> UIOption tedit

instance Show (UIOption edit) where
    show _ = "option"

instance UIType UIOption where
    uiWitness = $(iowitness [t|UIOption|])

uiOption ::
       forall tedit t. Eq t
    => EditFunction tedit (ListEdit [(t, Text)] (WholeEdit (t, Text)))
    -> EditLens tedit (WholeEdit t)
    -> UISpec tedit
uiOption optlens sellens = MkUISpec $ MkUIOption optlens sellens

uiSimpleOption :: Eq t => [(t, Text)] -> UISpec (WholeEdit t)
uiSimpleOption opts = uiOption (constEditFunction opts) id
