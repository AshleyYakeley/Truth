module Truth.Core.UI.Specifier.Button where

import Truth.Core.Import
import Truth.Core.Reference
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.View.View

data ButtonUISpec where
    MkButtonUISpec :: Model (ROWUpdate Text) -> Model (ROWUpdate (Maybe (View ()))) -> ButtonUISpec

instance Show ButtonUISpec where
    show (MkButtonUISpec _ _) = "button"

instance UIType ButtonUISpec where
    uiWitness = $(iowitness [t|ButtonUISpec|])

buttonUISpec :: Model (ROWUpdate Text) -> Model (ROWUpdate (Maybe (View ()))) -> CVUISpec
buttonUISpec label action = mkCVUISpec $ MkButtonUISpec label action

simpleButtonUISpec :: Model (ROWUpdate Text) -> View () -> CVUISpec
simpleButtonUISpec label action = buttonUISpec label $ constantModel $ Just action
