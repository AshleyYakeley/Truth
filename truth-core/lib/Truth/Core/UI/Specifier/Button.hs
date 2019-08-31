module Truth.Core.UI.Specifier.Button where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data ButtonUISpec sel update where
    MkButtonUISpec
        :: UpdateFunction update (WholeUpdate Text)
        -> UpdateFunction update (WholeUpdate (Maybe (IO ())))
        -> ButtonUISpec sel update

instance Show (ButtonUISpec sel update) where
    show (MkButtonUISpec _ _) = "button"

instance UIType ButtonUISpec where
    uiWitness = $(iowitness [t|ButtonUISpec|])

buttonUISpec ::
       UpdateFunction update (WholeUpdate Text)
    -> UpdateFunction update (WholeUpdate (Maybe (IO ())))
    -> UISpec sel update
buttonUISpec label action = MkUISpec $ MkButtonUISpec label action

simpleButtonUISpec :: UpdateFunction update (WholeUpdate Text) -> IO () -> UISpec sel update
simpleButtonUISpec label action = buttonUISpec label $ constUpdateFunction $ Just action
