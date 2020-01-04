module Truth.Core.UI.Specifier.Button where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Resource
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data ButtonUISpec sel where
    MkButtonUISpec
        :: ReadOnlyOpenSubscriber (WholeUpdate Text)
        -> ReadOnlyOpenSubscriber (WholeUpdate (Maybe (IO ())))
        -> ButtonUISpec sel

instance Show (ButtonUISpec sel) where
    show (MkButtonUISpec _ _) = "button"

instance UIType ButtonUISpec where
    uiWitness = $(iowitness [t|ButtonUISpec|])

buttonUISpec ::
       ReadOnlyOpenSubscriber (WholeUpdate Text) -> ReadOnlyOpenSubscriber (WholeUpdate (Maybe (IO ()))) -> UISpec sel
buttonUISpec label action = MkUISpec $ MkButtonUISpec label action

simpleButtonUISpec :: ReadOnlyOpenSubscriber (WholeUpdate Text) -> IO () -> UISpec sel
simpleButtonUISpec label action = buttonUISpec label $ openResource $ constantSubscriber $ Just action
