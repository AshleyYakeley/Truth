module Truth.Core.UI.Specifier.Checkbox where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data CheckboxUISpec sel update where
    MkCheckboxUISpec
        :: UpdateFunction update (WholeUpdate Text) -> EditLens update (WholeUpdate Bool) -> CheckboxUISpec sel update
    MkMaybeCheckboxUISpec
        :: UpdateFunction update (WholeUpdate Text)
        -> EditLens update (WholeUpdate (Maybe Bool))
        -> CheckboxUISpec sel update

instance Show (CheckboxUISpec sel update) where
    show (MkCheckboxUISpec _ _) = "checkbox"
    show (MkMaybeCheckboxUISpec _ _) = "maybe-checkbox"

instance UIType CheckboxUISpec where
    uiWitness = $(iowitness [t|CheckboxUISpec|])

checkboxUISpec ::
       forall update sel.
       UpdateFunction update (WholeUpdate Text)
    -> EditLens update (WholeUpdate Bool)
    -> UISpec sel update
checkboxUISpec label lens = MkUISpec $ MkCheckboxUISpec label lens

maybeCheckboxUISpec ::
       forall update sel.
       UpdateFunction update (WholeUpdate Text)
    -> EditLens update (WholeUpdate (Maybe Bool))
    -> UISpec sel update
maybeCheckboxUISpec label lens = MkUISpec $ MkMaybeCheckboxUISpec label lens
