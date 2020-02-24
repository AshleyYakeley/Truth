module Truth.Core.UI.Specifier.Button where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.View.View

data ButtonUISpec where
    MkButtonUISpec :: Subscriber (ROWUpdate Text) -> Subscriber (ROWUpdate (Maybe (View ()))) -> ButtonUISpec

instance Show ButtonUISpec where
    show (MkButtonUISpec _ _) = "button"

instance UIType ButtonUISpec where
    uiWitness = $(iowitness [t|ButtonUISpec|])

buttonUISpec :: Subscriber (ROWUpdate Text) -> Subscriber (ROWUpdate (Maybe (View ()))) -> CVUISpec
buttonUISpec label action = mkCVUISpec $ MkButtonUISpec label action

simpleButtonUISpec :: Subscriber (ROWUpdate Text) -> View () -> CVUISpec
simpleButtonUISpec label action = buttonUISpec label $ constantSubscriber $ Just action
