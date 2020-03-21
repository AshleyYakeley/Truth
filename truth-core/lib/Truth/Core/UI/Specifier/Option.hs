module Truth.Core.UI.Specifier.Option where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Read
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

data OptionUISpec where
    MkOptionUISpec
        :: ( Eq t
           , FullSubjectReader (UpdateReader update)
           , ApplicableUpdate update
           , UpdateSubject update ~ (t, OptionUICell)
           )
        => Subscriber (ReadOnlyUpdate (OrderedListUpdate [UpdateSubject update] update))
        -> Subscriber (WholeUpdate t)
        -> OptionUISpec

instance Show OptionUISpec where
    show _ = "option"

instance UIType OptionUISpec where
    uiWitness = $(iowitness [t|OptionUISpec|])

optionUISpec ::
       forall update t.
       ( Eq t
       , FullSubjectReader (UpdateReader update)
       , ApplicableUpdate update
       , UpdateSubject update ~ (t, OptionUICell)
       )
    => Subscriber (ReadOnlyUpdate (OrderedListUpdate [UpdateSubject update] update))
    -> Subscriber (WholeUpdate t)
    -> CVUISpec
optionUISpec optlens sellens = mkCVUISpec $ MkOptionUISpec optlens sellens

simpleOptionUISpec ::
       forall t. Eq t
    => [(t, OptionUICell)]
    -> Subscriber (WholeUpdate t)
    -> CVUISpec
simpleOptionUISpec opts sub = optionUISpec @(WholeUpdate (t, OptionUICell)) (constantSubscriber opts) sub
