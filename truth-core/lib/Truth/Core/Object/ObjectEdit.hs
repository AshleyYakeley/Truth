module Truth.Core.Object.ObjectEdit
    ( ObjectReader(..)
    , ObjectEdit
    , objectEditLens
    , objectLiftEditLens
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.AutoClose
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Core.Types.None

-- | Opens a session on the object. Returns an object that can be used without opening a new session, and a function that closes the session.
openCloseObject :: Object edit -> IO (Object edit, IO ())
openCloseObject (MkObject (MkUnliftIO run) r e) = do
    (run', close) <- withToOpen $ \call -> run $ liftIOWithUnlift $ call
    return (MkObject run' r e, close)

data ObjectReader edit t where
    ReadObject :: ObjectReader edit (Object edit)

instance Show (ObjectReader edit t) where
    show ReadObject = "object"

instance AllWitnessConstraint Show (ObjectReader edit) where
    allWitnessConstraint = Dict

instance c (Object edit) => WitnessConstraint c (ObjectReader edit) where
    witnessConstraint ReadObject = Dict

instance SubjectReader (EditReader edit) => SubjectReader (ObjectReader edit) where
    type ReaderSubject (ObjectReader edit) = EditSubject edit
    subjectToRead subj ReadObject = constantObject subj

instance FullSubjectReader (EditReader edit) => FullSubjectReader (ObjectReader edit) where
    mutableReadToSubject mr = do
        MkObject (MkUnliftIO unlift) mro _ <- mr ReadObject
        liftIO $ unlift $ mutableReadToSubject mro

type ObjectEdit edit = NoEdit (ObjectReader edit)

type ObjectEditT edit = StateT (Maybe (Object edit, IO ()))

unliftObjectEditT :: Unlift (ObjectEditT edit)
unliftObjectEditT =
    MkUnlift $ \smr -> do
        (r, ms) <- runStateT smr Nothing
        case ms of
            Nothing -> return ()
            Just (_, close) -> liftIO close
        return r

openObject ::
       forall edit m. MonadIO m
    => MutableRead m (ObjectReader edit)
    -> ObjectEditT edit m (Object edit)
openObject mr = do
    ms <- get
    case ms of
        Just (obj, _) -> return obj
        Nothing -> do
            mainObj <- lift $ mr ReadObject
            objcl <- liftIO $ openCloseObject mainObj
            put $ Just objcl
            return $ fst objcl

-- | This lens must not be used with 'mapSubscriber' or 'mapViewContextEdit'.
objectEditLens :: forall edit. EditLens (ObjectEdit edit) edit
objectEditLens = let
    efGet :: ReadFunctionT (ObjectEditT edit) (ObjectReader edit) (EditReader edit)
    efGet mr rt = do
        (MkObject (MkUnliftIO run) r _) <- openObject mr
        liftIO $ run $ r rt
    efUpdate ::
           forall m. MonadIO m
        => ObjectEdit edit
        -> MutableRead m (ObjectReader edit)
        -> ObjectEditT edit m [edit]
    efUpdate edit _ = never edit
    elFunction :: AnEditFunction (ObjectEditT edit) (ObjectEdit edit) edit
    elFunction = MkAnEditFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [edit]
        -> MutableRead m (EditReader (ObjectEdit edit))
        -> ObjectEditT edit m (Maybe [ObjectEdit edit])
    elPutEdits edits mr = do
        (MkObject (MkUnliftIO run) _ e) <- openObject mr
        liftIO $
            run $ do
                maction <- e edits
                case maction of
                    Just action -> action
                    Nothing -> liftIO $ fail "objectEditLens: failed"
        return $ Just []
    in MkCloseUnlift unliftObjectEditT $ MkAnEditLens {..}

objectLiftEditLens ::
       forall edita editb. Edit edita
    => EditLens edita editb
    -> EditLens (ObjectEdit edita) (ObjectEdit editb)
objectLiftEditLens lens = let
    efGet :: ReadFunctionT IdentityT (ObjectReader edita) (ObjectReader editb)
    efGet mr ReadObject = do
        object <- lift $ mr ReadObject
        return $ mapObject lens object
    efUpdate ::
           forall m. MonadIO m
        => ObjectEdit edita
        -> MutableRead m (ObjectReader edita)
        -> IdentityT m [ObjectEdit editb]
    efUpdate edit _ = never edit
    elFunction :: AnEditFunction IdentityT (ObjectEdit edita) (ObjectEdit editb)
    elFunction = MkAnEditFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [ObjectEdit editb]
        -> MutableRead m (ObjectReader edita)
        -> IdentityT m (Maybe [ObjectEdit edita])
    elPutEdits [] _ = return $ Just []
    elPutEdits (edit:_) _ = never edit
    in MkCloseUnlift identityUnlift $ MkAnEditLens {..}
