{-# OPTIONS -fno-warn-orphans #-}

module Truth.Debug.Object
    ( module Truth.Debug
    , module Truth.Debug.Object
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Debug

data EditShower edit = MkEditShower
    { showRead :: forall t. EditReader edit t -> String
    , showReadResult :: forall t. EditReader edit t -> t -> String
    , showEdit :: edit -> String
    }

blankEditShower :: EditShower edit
blankEditShower = MkEditShower {showRead = \_ -> "", showReadResult = \_ _ -> "", showEdit = \_ -> "edit"}

traceObject :: forall edit. String -> EditShower edit -> Object edit -> Object edit
traceObject prefix MkEditShower {..} (MkObject (MkUnliftIO run :: UnliftIO m) r e) = let
    run' :: UnliftIO m
    run' = MkUnliftIO $ \m -> traceBracket (contextStr prefix "object") $ run m
    r' :: MutableRead m (EditReader edit)
    r' rt = traceBracketArgs (contextStr prefix "read") (showRead rt) (showReadResult rt) $ r rt
    e' :: [edit] -> m (Maybe (m ()))
    e' edits =
        traceBracketArgs
            (contextStr prefix "edit.examine")
            ("[" ++ intercalate "," (fmap showEdit edits) ++ "]")
            (\mx ->
                 if isJust mx
                     then "action"
                     else "no action") $
        (fmap $
         fmap $
         traceBracketArgs (contextStr prefix "edit.do") ("[" ++ intercalate "," (fmap showEdit edits) ++ "]") (\_ -> "")) $
        e edits
    in MkObject run' r' e'

type ShowableEdit edit
     = (Show edit, AllWitnessConstraint Show (EditReader edit), WitnessConstraint Show (EditReader edit))

showEditShower ::
       forall edit. ShowableEdit edit
    => EditShower edit
showEditShower = let
    showRead rt = showAllWitness rt
    showReadResult :: forall t. EditReader edit t -> t -> String
    showReadResult rt t =
        case witnessConstraint @_ @Show rt of
            Dict -> show t
    showEdit = show
    in MkEditShower {..}

class TraceArgThing t where
    traceArgThing :: String -> t -> t

instance TraceThing (Object edit) where
    traceThing prefix = traceObject prefix blankEditShower

instance ShowableEdit edit => TraceArgThing (Object edit) where
    traceArgThing prefix = traceObject prefix showEditShower

class TraceAThing f where
    traceAThing ::
           forall (t :: (* -> *) -> (* -> *)) (edita :: *) (editb :: *). MonadTransUnlift t
        => String
        -> f t edita editb
        -> f t edita editb
    traceArgAThing ::
           forall (t :: (* -> *) -> (* -> *)) (edita :: *) (editb :: *).
           (MonadTransUnlift t, ShowableEdit edita, ShowableEdit editb)
        => String
        -> f t edita editb
        -> f t edita editb

instance TraceAThing f => TraceThing (CloseUnlift f a b) where
    traceThing prefix (MkCloseUnlift unlift athing) =
        MkCloseUnlift (traceThing prefix unlift) (traceAThing prefix athing)

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

instance TraceAThing AnEditLens where
    traceAThing prefix (MkAnEditLens ef pe) =
        MkAnEditLens
            (traceAThing prefix ef)
            (\ee mr -> withTransConstraintTM @MonadIO $ traceBracket (contextStr prefix "put") $ pe ee mr)
    traceArgAThing prefix (MkAnEditLens ef pe) =
        MkAnEditLens
            (traceArgAThing prefix ef)
            (\ee mr ->
                 withTransConstraintTM @MonadIO $ traceBracketArgs (contextStr prefix "put") (show ee) show $ pe ee mr)

instance (TraceAThing f, ShowableEdit edita, ShowableEdit editb) => TraceArgThing (CloseUnlift f edita editb) where
    traceArgThing prefix (MkCloseUnlift unlift athing) =
        MkCloseUnlift (traceThing prefix unlift) (traceArgAThing prefix athing)
