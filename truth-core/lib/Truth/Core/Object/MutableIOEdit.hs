module Truth.Core.Object.MutableIOEdit
    ( MutableIOReader(..)
    , MutableIOEdit
    , mutableIOEditLens
    , mutableIOLiftEditLens
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
    (run', close) <- withToOpen $ \call -> run $ liftIOWithUnlift $ \unlift -> call $ MkUnliftIO unlift
    return (MkObject run' r e, close)

data MutableIOReader edit t where
    ReadMutableIO :: MutableIOReader edit (Object edit)

instance SubjectReader (EditReader edit) => SubjectReader (MutableIOReader edit) where
    type ReaderSubject (MutableIOReader edit) = EditSubject edit
    subjectToRead subj ReadMutableIO = constantObject subj

instance FullSubjectReader (EditReader edit) => FullSubjectReader (MutableIOReader edit) where
    mutableReadToSubject mr = do
        MkObject (MkUnliftIO unlift) mro _ <- mr ReadMutableIO
        liftIO $ unlift $ mutableReadToSubject mro

type MutableIOEdit edit = NoEdit (MutableIOReader edit)

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
    => MutableRead m (MutableIOReader edit)
    -> ObjectEditT edit m (Object edit)
openObject mr = do
    ms <- get
    case ms of
        Just (obj, _) -> return obj
        Nothing -> do
            mainObj <- lift $ mr ReadMutableIO
            objcl <- liftIO $ openCloseObject mainObj
            put $ Just objcl
            return $ fst objcl

mutableIOEditLens :: forall edit. EditLens (MutableIOEdit edit) edit
mutableIOEditLens = let
    efGet :: ReadFunctionT (ObjectEditT edit) (MutableIOReader edit) (EditReader edit)
    efGet mr rt = do
        (MkObject (MkUnliftIO run) r _) <- openObject mr
        liftIO $ run $ r rt
    efUpdate ::
           forall m. MonadIO m
        => MutableIOEdit edit
        -> MutableRead m (MutableIOReader edit)
        -> ObjectEditT edit m [edit]
    efUpdate edit _ = never edit
    elFunction :: AnEditFunction (ObjectEditT edit) (MutableIOEdit edit) edit
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => edit
        -> MutableRead m (EditReader (MutableIOEdit edit))
        -> ObjectEditT edit m (Maybe [MutableIOEdit edit])
    elPutEdit edit mr = do
        (MkObject (MkUnliftIO run) _ e) <- openObject mr
        liftIO $
            run $ do
                maction <- e [edit]
                case maction of
                    Just action -> action
                    Nothing -> liftIO $ fail "mutableIOEditLens: failed"
        return $ Just []
    in MkCloseUnlift unliftObjectEditT $ MkAnEditLens {..}

mutableIOLiftEditLens ::
       forall edita editb. Edit edita
    => EditLens edita editb
    -> EditLens (MutableIOEdit edita) (MutableIOEdit editb)
mutableIOLiftEditLens lens = let
    efGet :: ReadFunctionT IdentityT (MutableIOReader edita) (MutableIOReader editb)
    efGet mr ReadMutableIO = do
        object <- lift $ mr ReadMutableIO
        return $ mapObject lens object
    efUpdate ::
           forall m. MonadIO m
        => MutableIOEdit edita
        -> MutableRead m (MutableIOReader edita)
        -> IdentityT m [MutableIOEdit editb]
    efUpdate edit _ = never edit
    elFunction :: AnEditFunction IdentityT (MutableIOEdit edita) (MutableIOEdit editb)
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => MutableIOEdit editb
        -> MutableRead m (MutableIOReader edita)
        -> IdentityT m (Maybe [MutableIOEdit edita])
    elPutEdit edit _ = never edit
    in MkCloseUnlift identityUnlift $ MkAnEditLens {..}
