module Truth.Core.UI.Specifier.Option where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Read
import Truth.Core.Resource
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
        :: ( Eq t
           , FullSubjectReader (UpdateReader update)
           , ApplicableUpdate update
           , UpdateSubject update ~ (t, OptionUICell)
           )
        => OpenSubscriber (ReadOnlyUpdate (OrderedListUpdate [UpdateSubject update] update))
        -> OpenSubscriber (WholeUpdate t)
        -> OptionUISpec sel

instance Show (OptionUISpec sel) where
    show _ = "option"

instance UIType OptionUISpec where
    uiWitness = $(iowitness [t|OptionUISpec|])

optionUISpec ::
       forall update t sel.
       ( Eq t
       , FullSubjectReader (UpdateReader update)
       , ApplicableUpdate update
       , UpdateSubject update ~ (t, OptionUICell)
       )
    => OpenSubscriber (ReadOnlyUpdate (OrderedListUpdate [UpdateSubject update] update))
    -> OpenSubscriber (WholeUpdate t)
    -> LUISpec sel
optionUISpec optlens sellens = mkLUISpec $ MkOptionUISpec optlens sellens

simpleOptionUISpec ::
       forall t sel. Eq t
    => [(t, OptionUICell)]
    -> OpenSubscriber (WholeUpdate t)
    -> LUISpec sel
simpleOptionUISpec opts sub = optionUISpec @(WholeUpdate (t, OptionUICell)) (openResource $ constantSubscriber opts) sub
