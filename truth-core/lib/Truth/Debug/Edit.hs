{-# OPTIONS -fno-warn-orphans #-}

module Truth.Debug.Edit
    ( module Truth.Debug
    , module Truth.Debug.Edit
    ) where

import Truth.Core.Edit.Edit
import Truth.Core.Edit.Function
import Truth.Core.Edit.Update
import Truth.Core.Import
import Truth.Debug

class TraceArgThing t where
    traceArgThing :: String -> t -> t

type ShowableEdit edit
     = (Show edit, AllWitnessConstraint Show (EditReader edit), WitnessConstraint Show (EditReader edit))

type ShowableUpdate update
     = (Show update, ShowableEdit (UpdateEdit update))

instance TraceThing (UpdateFunction updateA updateB) where
    traceThing prefix (MkUpdateFunction g u) =
        MkUpdateFunction
            (\mr rt -> traceBracket (contextStr prefix "get") $ g mr rt)
            (\ee mr -> traceBracket (contextStr prefix "update") $ u ee mr)

instance (ShowableUpdate updateA, ShowableUpdate updateB) => TraceArgThing (UpdateFunction updateA updateB) where
    traceArgThing prefix (MkUpdateFunction g u) =
        MkUpdateFunction
            (\mr (rt :: UpdateReader updateB r) ->
                 case allWitnessConstraint @_ @_ @Show @(UpdateReader updateB) @r of
                     Dict ->
                         case witnessConstraint @_ @Show rt of
                             Dict ->
                                 traceBracketArgs (contextStr prefix "get") (show rt) show $ g mr rt)
            (\ee mr -> traceBracketArgs (contextStr prefix "update") (show ee) show $ u ee mr)
