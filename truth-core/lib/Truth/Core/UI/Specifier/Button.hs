module Truth.Core.UI.Specifier.Button where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data ButtonUISpec sel where
    MkButtonUISpec
        :: ReadOnlySubscriber (WholeUpdate Text) -> ReadOnlySubscriber (WholeUpdate (Maybe (IO ()))) -> ButtonUISpec sel

instance Show (ButtonUISpec sel) where
    show (MkButtonUISpec _ _) = "button"

instance UIType ButtonUISpec where
    uiWitness = $(iowitness [t|ButtonUISpec|])

buttonUISpec :: ReadOnlySubscriber (WholeUpdate Text) -> ReadOnlySubscriber (WholeUpdate (Maybe (IO ()))) -> UISpec sel
buttonUISpec label action = MkUISpec $ MkButtonUISpec label action

simpleButtonUISpec :: ReadOnlySubscriber (WholeUpdate Text) -> IO () -> UISpec sel
simpleButtonUISpec label action = buttonUISpec label $ constantSubscriber $ Just action
