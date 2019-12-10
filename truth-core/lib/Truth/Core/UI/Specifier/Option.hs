module Truth.Core.UI.Specifier.Option where

import Truth.Core.Edit
import Truth.Core.Import
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

data OptionUISpec sel update where
    MkOptionUISpec
        :: Eq t
        => UpdateFunction updateT (ListUpdate [(t, OptionUICell)] (WholeUpdate (t, OptionUICell)))
        -> EditLens updateT (WholeUpdate t)
        -> OptionUISpec sel updateT

instance Show (OptionUISpec sel update) where
    show _ = "option"

instance UIType OptionUISpec where
    uiWitness = $(iowitness [t|OptionUISpec|])

optionUISpec ::
       forall updateT t sel. Eq t
    => UpdateFunction updateT (ListUpdate [(t, OptionUICell)] (WholeUpdate (t, OptionUICell)))
    -> EditLens updateT (WholeUpdate t)
    -> UISpec sel updateT
optionUISpec optlens sellens = MkUISpec $ MkOptionUISpec optlens sellens

simpleOptionUISpec :: Eq t => [(t, OptionUICell)] -> UISpec sel (WholeUpdate t)
simpleOptionUISpec opts = optionUISpec (constUpdateFunction opts) id
