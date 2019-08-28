module Truth.Core.UI.Specifier.Option where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data OptionUISpec sel edit where
    MkOptionUISpec
        :: Eq t
        => UpdateFunction tedit (ListEdit [(t, Text)] (WholeEdit (t, Text)))
        -> EditLens tedit (WholeEdit t)
        -> OptionUISpec sel tedit

instance Show (OptionUISpec sel edit) where
    show _ = "option"

instance UIType OptionUISpec where
    uiWitness = $(iowitness [t|OptionUISpec|])

optionUISpec ::
       forall tedit t sel. Eq t
    => UpdateFunction tedit (ListEdit [(t, Text)] (WholeEdit (t, Text)))
    -> EditLens tedit (WholeEdit t)
    -> UISpec sel tedit
optionUISpec optlens sellens = MkUISpec $ MkOptionUISpec optlens sellens

simpleOptionUISpec :: Eq t => [(t, Text)] -> UISpec sel (WholeEdit t)
simpleOptionUISpec opts = optionUISpec (constUpdateFunction opts) id
