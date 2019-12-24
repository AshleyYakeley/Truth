module Truth.Core.UI.Specifier.Option where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.TextStyle

data OptionUICell = MkOptionUICell
    { optionCellText :: Text
    , optionCellStyle :: TextStyle
    } deriving (Eq)

plainOptionUICell :: Text -> OptionUICell
plainOptionUICell optionCellText = let
    optionCellStyle = plainTextStyle
    in MkOptionUICell {..}

data OptionUISpec sel where
    MkOptionUISpec
        :: Eq t
        => ReadOnlySubscriber (ListUpdate [(t, OptionUICell)] (WholeUpdate (t, OptionUICell)))
        -> Subscriber (WholeUpdate t)
        -> OptionUISpec sel

instance Show (OptionUISpec sel) where
    show _ = "option"

instance UIType OptionUISpec where
    uiWitness = $(iowitness [t|OptionUISpec|])

optionUISpec ::
       forall t sel. Eq t
    => ReadOnlySubscriber (ListUpdate [(t, OptionUICell)] (WholeUpdate (t, OptionUICell)))
    -> Subscriber (WholeUpdate t)
    -> UISpec sel
optionUISpec optlens sellens = MkUISpec $ MkOptionUISpec optlens sellens

simpleOptionUISpec :: Eq t => [(t, OptionUICell)] -> Subscriber (WholeUpdate t) -> UISpec sel
simpleOptionUISpec opts sub = optionUISpec (constantSubscriber opts) sub
