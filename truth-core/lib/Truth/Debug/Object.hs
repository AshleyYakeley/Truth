{-# OPTIONS -fno-warn-orphans #-}

module Truth.Debug.Object
    ( module Truth.Debug
    , module Truth.Debug.Edit
    , module Truth.Debug.Object
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Core.Resource
import Truth.Debug
import Truth.Debug.Edit

data EditShower edit = MkEditShower
    { showRead :: forall t. EditReader edit t -> String
    , showReadResult :: forall t. EditReader edit t -> t -> String
    , showEdit :: edit -> String
    }

blankEditShower :: EditShower edit
blankEditShower = MkEditShower {showRead = \_ -> "", showReadResult = \_ _ -> "", showEdit = \_ -> "edit"}

traceAnObject :: forall tt edit. (MonadIO (ApplyStack tt IO)) => String -> EditShower edit -> AnObject edit tt -> AnObject edit tt
traceAnObject prefix MkEditShower {..} (MkAnObject r e) = let
    r' :: MutableRead (ApplyStack tt IO) (EditReader edit)
    r' rt = traceBracketArgs (contextStr prefix "read") (showRead rt) (showReadResult rt) $ r rt
    e' :: NonEmpty edit -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
    e' edits =
        traceBracketArgs
            (contextStr prefix "edit.examine")
            ("[" ++ intercalate "," (toList $ fmap showEdit edits) ++ "]")
            (\mx ->
                 if isJust mx
                     then "action"
                     else "no action") $
        (fmap $
         fmap $
         fmap $
         traceBracketArgs (contextStr prefix "edit.do") ("[" ++ intercalate "," (toList $ fmap showEdit edits) ++ "]") (\_ -> "")) $
        e edits
    in MkAnObject r' e'

traceObject :: forall edit. String -> EditShower edit -> Object edit -> Object edit
traceObject prefix shower (MkResource rr anobj) = runResourceRunnerWith rr $ \_ ->
    MkResource rr $ traceAnObject prefix shower anobj

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

instance TraceThing (EditLens updateA updateB) where
    traceThing prefix (MkEditLens ef pe) =
        MkEditLens
            (traceThing prefix ef)
            (\ee mr -> traceBracket (contextStr prefix "put") $ do
                mee <- pe ee mr
                case mee of
                    Just _ -> traceIOM (contextStr prefix "put: edits")
                    Nothing -> traceIOM (contextStr prefix "put: no edits")
                return mee
                )

instance (ShowableUpdate updateA, ShowableUpdate updateB) => TraceArgThing (EditLens updateA updateB) where
    traceArgThing prefix (MkEditLens ef pe) =
        MkEditLens
            (traceArgThing prefix ef)
            (\ee mr ->
                 traceBracketArgs (contextStr prefix "put") (show ee) show $ do
                mee <- pe ee mr
                case mee of
                    Just _ -> traceIOM (contextStr prefix "put: edits")
                    Nothing -> traceIOM (contextStr prefix "put: no edits")
                return mee
                )

instance TraceThing (Object edit) where
    traceThing prefix = traceObject prefix blankEditShower

instance ShowableEdit edit => TraceArgThing (Object edit) where
    traceArgThing prefix = traceObject prefix showEditShower

instance MonadIO m => TraceThing (LifeCycleT m a) where
    traceThing prefix (MkLifeCycleT oc) =
        MkLifeCycleT $
        traceBracket (contextStr prefix "open") $ do
            (t, closer) <- oc
            return (t, traceBracket (contextStr prefix "close") closer)

slowObject :: Int -> Object edit -> Object edit
slowObject mus (MkResource rr (MkAnObject rd push)) = runResourceRunnerWith rr $ \_ -> let
    push' edits = do
        maction <- push edits
        return $
            case maction of
                Nothing -> Nothing
                Just action ->
                    Just $ \esrc -> do
                        traceBracket "slow: delay" $ liftIO $ threadDelay mus
                        traceBracket "slow: action" $ action esrc
    in MkResource rr $ MkAnObject rd push'
