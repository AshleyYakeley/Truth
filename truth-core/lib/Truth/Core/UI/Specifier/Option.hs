module Truth.Core.UI.Specifier.Option where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data OptionUISpec sel update where
    MkOptionUISpec
        :: Eq t
        => UpdateFunction updateT (ListUpdate [(t, Text)] (WholeUpdate (t, Text)))
        -> EditLens updateT (WholeUpdate t)
        -> OptionUISpec sel updateT

instance Show (OptionUISpec sel update) where
    show _ = "option"

instance UIType OptionUISpec where
    uiWitness = $(iowitness [t|OptionUISpec|])

optionUISpec ::
       forall updateT t sel. Eq t
    => UpdateFunction updateT (ListUpdate [(t, Text)] (WholeUpdate (t, Text)))
    -> EditLens updateT (WholeUpdate t)
    -> UISpec sel updateT
optionUISpec optlens sellens = MkUISpec $ MkOptionUISpec optlens sellens

simpleOptionUISpec :: Eq t => [(t, Text)] -> UISpec sel (WholeUpdate t)
simpleOptionUISpec opts = optionUISpec (constUpdateFunction opts) id
