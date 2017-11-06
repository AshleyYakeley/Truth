module Truth.Core.UI.Option where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier

data UIOption edit where
    MkUIOption
        :: Eq t
        => GeneralFunction tedit (ListEdit [(t, String)] (WholeEdit (t, String)))
        -> GeneralLens tedit (WholeEdit t)
        -> UIOption tedit

instance Show (UIOption edit) where
    show _ = "option"

instance UIType UIOption where
    uiWitness = $(iowitness [t|UIOption|])

uiOption ::
       forall tedit t. Eq t
    => GeneralFunction tedit (ListEdit [(t, String)] (WholeEdit (t, String)))
    -> GeneralLens tedit (WholeEdit t)
    -> UISpec tedit
uiOption optlens sellens = MkUISpec $ MkUIOption optlens sellens

uiSimpleOption :: Eq t => [(t, String)] -> UISpec (WholeEdit t)
uiSimpleOption opts = uiOption (constGeneralFunction opts) cid
