{-# OPTIONS -fno-warn-orphans #-}

module Changes.Debug.Edit
    ( module Changes.Debug
    , module Changes.Debug.Edit
    ) where

import Changes.Core.Edit.Edit
import Changes.Core.Edit.Update
import Changes.Core.Import
import Changes.Debug

class TraceArgThing t where
    traceArgThing :: String -> t -> t

type ShowableEdit edit
     = (Show edit, AllWitnessConstraint Show (EditReader edit), WitnessConstraint Show (EditReader edit))

type ShowableUpdate update
     = (Show update, ShowableEdit (UpdateEdit update))
