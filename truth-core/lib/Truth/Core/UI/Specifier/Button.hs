module Truth.Core.UI.Specifier.Button where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Resource
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data ButtonUISpec sel where
    MkButtonUISpec :: OpenSubscriber (ROWUpdate Text) -> OpenSubscriber (ROWUpdate (Maybe (IO ()))) -> ButtonUISpec sel

instance Show (ButtonUISpec sel) where
    show (MkButtonUISpec _ _) = "button"

instance UIType ButtonUISpec where
    uiWitness = $(iowitness [t|ButtonUISpec|])

buttonUISpec :: OpenSubscriber (ROWUpdate Text) -> OpenSubscriber (ROWUpdate (Maybe (IO ()))) -> LUISpec sel
buttonUISpec label action = mkLUISpec $ MkButtonUISpec label action

simpleButtonUISpec :: OpenSubscriber (ROWUpdate Text) -> IO () -> LUISpec sel
simpleButtonUISpec label action = buttonUISpec label $ openResource $ constantSubscriber $ Just action
