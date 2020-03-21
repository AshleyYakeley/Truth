{-# OPTIONS -fno-warn-orphans #-}

module Truth.Debug.Edit
    ( module Truth.Debug
    , module Truth.Debug.Edit
    ) where

import Truth.Core.Edit.Edit
import Truth.Core.Edit.Update
import Truth.Core.Import
import Truth.Debug

class TraceArgThing t where
    traceArgThing :: String -> t -> t

type ShowableEdit edit
     = (Show edit, AllWitnessConstraint Show (EditReader edit), WitnessConstraint Show (EditReader edit))

type ShowableUpdate update
     = (Show update, ShowableEdit (UpdateEdit update))
