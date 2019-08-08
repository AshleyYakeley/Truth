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
import Truth.Core.Object.UnliftIO
import Truth.Core.Read
import Truth.Debug
import Truth.Debug.Edit

data EditShower edit = MkEditShower
    { showRead :: forall t. EditReader edit t -> String
    , showReadResult :: forall t. EditReader edit t -> t -> String
    , showEdit :: edit -> String
    }

blankEditShower :: EditShower edit
blankEditShower = MkEditShower {showRead = \_ -> "", showReadResult = \_ _ -> "", showEdit = \_ -> "edit"}

traceAnObject :: forall m edit. MonadIO m => String -> EditShower edit -> AnObject m edit -> AnObject m edit
traceAnObject prefix MkEditShower {..} (MkAnObject r e) = let
    r' :: MutableRead m (EditReader edit)
    r' rt = traceBracketArgs (contextStr prefix "read") (showRead rt) (showReadResult rt) $ r rt
    e' :: [edit] -> m (Maybe (EditSource -> m ()))
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
         fmap $
         traceBracketArgs (contextStr prefix "edit.do") ("[" ++ intercalate "," (fmap showEdit edits) ++ "]") (\_ -> "")) $
        e edits
    in MkAnObject r' e'

traceObject :: forall edit. String -> EditShower edit -> Object edit -> Object edit
traceObject prefix shower (MkCloseUnliftIO (run :: UnliftIO m) anobj) =
    MkCloseUnliftIO (traceThing (contextStr prefix "run") run) $ traceAnObject prefix shower anobj

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

instance TraceAThing AnEditLens where
    traceAThing prefix (MkAnEditLens ef pe) =
        MkAnEditLens
            (traceAThing prefix ef)
            (\ee mr -> withTransConstraintTM @MonadIO $ traceBracket (contextStr prefix "put") $ do
                mee <- pe ee mr
                case mee of
                    Just _ -> traceIOM (contextStr prefix "put: edits")
                    Nothing -> traceIOM (contextStr prefix "put: no edits")
                return mee
                )
    traceArgAThing prefix (MkAnEditLens ef pe) =
        MkAnEditLens
            (traceArgAThing prefix ef)
            (\ee mr ->
                 withTransConstraintTM @MonadIO $ traceBracketArgs (contextStr prefix "put") (show ee) show $ do
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
slowObject mus (MkCloseUnliftIO run (MkAnObject rd push)) = let
    push' edits = do
        maction <- push edits
        return $
            case maction of
                Nothing -> Nothing
                Just action ->
                    Just $ \esrc -> do
                        traceBracket "slow: delay" $ liftIO $ threadDelay mus
                        traceBracket "slow: action" $ action esrc
    in MkCloseUnliftIO run $ MkAnObject rd push'
