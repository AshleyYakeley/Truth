{-# OPTIONS -fno-warn-orphans #-}

module Truth.Debug.Edit
    ( module Truth.Debug
    , module Truth.Debug.Edit
    ) where

import Truth.Core.Edit.Edit
import Truth.Core.Edit.Function
import Truth.Core.Edit.Unlift
import Truth.Core.Import
import Truth.Debug

class TraceArgThing t where
    traceArgThing :: String -> t -> t

type ShowableEdit edit
     = (Show edit, AllWitnessConstraint Show (EditReader edit), WitnessConstraint Show (EditReader edit))

class TraceAThing f where
    traceAThing ::
           forall (t :: (Type -> Type) -> (Type -> Type)) (edita :: Type) (editb :: Type). MonadTransUnlift t
        => String
        -> f t edita editb
        -> f t edita editb
    traceArgAThing ::
           forall (t :: (Type -> Type) -> (Type -> Type)) (edita :: Type) (editb :: Type).
           (MonadTransUnlift t, ShowableEdit edita, ShowableEdit editb)
        => String
        -> f t edita editb
        -> f t edita editb

instance TraceAThing f => TraceThing (CloseUnlift f a b) where
    traceThing prefix (MkCloseUnlift unlift athing) =
        MkCloseUnlift (traceThing prefix unlift) (traceAThing prefix athing)

instance (TraceAThing f, ShowableEdit edita, ShowableEdit editb) => TraceArgThing (CloseUnlift f edita editb) where
    traceArgThing prefix (MkCloseUnlift unlift athing) =
        MkCloseUnlift (traceThing prefix unlift) (traceArgAThing prefix athing)

instance TraceAThing AnEditFunction where
    traceAThing prefix (MkAnEditFunction g u) =
        MkAnEditFunction
            (\mr rt -> withTransConstraintTM @MonadIO $ traceBracket (contextStr prefix "get") $ g mr rt)
            (\ee mr -> withTransConstraintTM @MonadIO $ traceBracket (contextStr prefix "update") $ u ee mr)
    traceArgAThing ::
           forall t edita editb. (MonadTransUnlift t, ShowableEdit edita, ShowableEdit editb)
        => String
        -> AnEditFunction t edita editb
        -> AnEditFunction t edita editb
    traceArgAThing prefix (MkAnEditFunction g u) =
        MkAnEditFunction
            (\mr (rt :: EditReader editb r) ->
                 case allWitnessConstraint @_ @_ @Show @(EditReader editb) @r of
                     Dict ->
                         case witnessConstraint @_ @Show rt of
                             Dict ->
                                 withTransConstraintTM @MonadIO $
                                 traceBracketArgs (contextStr prefix "get") (show rt) show $ g mr rt)
            (\ee mr ->
                 withTransConstraintTM @MonadIO $ traceBracketArgs (contextStr prefix "update") (show ee) show $ u ee mr)
