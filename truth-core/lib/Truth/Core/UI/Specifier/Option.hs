module Truth.Core.UI.Specifier.Option where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UIOption sel edit where
    MkUIOption
        :: Eq t
        => EditFunction tedit (ListEdit [(t, Text)] (WholeEdit (t, Text)))
        -> EditLens tedit (WholeEdit t)
        -> UIOption sel tedit

instance Show (UIOption sel edit) where
    show _ = "option"

instance UIType UIOption where
    uiWitness = $(iowitness [t|UIOption|])

uiOption ::
       forall tedit t sel. Eq t
    => EditFunction tedit (ListEdit [(t, Text)] (WholeEdit (t, Text)))
    -> EditLens tedit (WholeEdit t)
    -> UISpec sel tedit
uiOption optlens sellens = MkUISpec $ MkUIOption optlens sellens

uiSimpleOption :: Eq t => [(t, Text)] -> UISpec sel (WholeEdit t)
uiSimpleOption opts = uiOption (constEditFunction opts) id
