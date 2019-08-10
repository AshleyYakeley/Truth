module Truth.Core.Object.UpdatingObject
    ( UpdatingObject
    , updatingObject
    , shareUpdatingObject
    , lensUpdatingObject
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.DeferActionT
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.UnliftIO
import Truth.Core.Read
import Truth.Debug.Object

type UpdatingObject edit a = ([edit] -> EditSource -> IO ()) -> LifeCycleIO (Object edit, a)

updatingObject :: forall edit. Object edit -> UpdatingObject edit ()
updatingObject (MkCloseUnliftIO (run :: UnliftIO m) (MkAnObject r e)) update =
    return $ let
        run' :: UnliftIO (DeferActionT m)
        run' = composeUnliftTransformCommute runDeferActionT run
        r' :: MutableRead (DeferActionT m) (EditReader edit)
        r' = liftMutableRead r
        e' :: [edit] -> DeferActionT m (Maybe (EditSource -> DeferActionT m ()))
        e' edits = do
            maction <- lift $ e edits
            case maction of
                Nothing -> return Nothing
                Just action ->
                    return $
                    Just $ \esrc -> do
                        lift $ traceBracket "updatingObject: push" $ action esrc
                        deferActionT $ traceBracket "updatingObject: reflecting update" $ update edits esrc
        in (traceThing "updatingObject" $ MkCloseUnliftIO run' $ MkAnObject r' e', ())

type UpdateStoreEntry edit = [edit] -> EditSource -> IO ()

type UpdateStore edit = Store (UpdateStoreEntry edit)

newtype EditQueue edit =
    MkEditQueue [(EditSource, [edit])]

collapse1 :: (EditSource, [edit]) -> [(EditSource, [edit])] -> [(EditSource, [edit])]
collapse1 (esa, ea) ((esb, eb):bb)
    | esa == esb = (esb, ea <> eb) : bb
collapse1 a bb = a : bb

collapse :: [(EditSource, [edit])] -> [(EditSource, [edit])] -> [(EditSource, [edit])]
collapse [] bb = bb
collapse [a] bb = collapse1 a bb
collapse (a:aa) bb = a : collapse aa bb

instance Semigroup (EditQueue edit) where
    MkEditQueue aa <> MkEditQueue bb = MkEditQueue $ collapse aa bb

singleEditQueue :: [edit] -> EditSource -> EditQueue edit
singleEditQueue edits esrc = MkEditQueue $ pure (esrc, edits)

getRunner :: Bool -> ([edit] -> EditSource -> IO ()) -> LifeCycleIO ([edit] -> EditSource -> IO ())
getRunner False handler = return handler
getRunner True handler = do
    runAsync <- asyncRunner $ \(MkEditQueue sourcededits) -> for_ sourcededits $ \(esrc, edits) -> handler edits esrc
    return $ \edits esrc -> runAsync $ singleEditQueue edits esrc

shareUpdatingObject :: forall edit a. Bool -> UpdatingObject edit a -> LifeCycleIO (UpdatingObject edit a)
shareUpdatingObject async uobj = do
    var :: MVar (UpdateStore edit) <- liftIO $ newMVar emptyStore
    let
        updateP :: [edit] -> EditSource -> IO ()
        updateP edits ectxt = do
            store <- mvarRun var get
            for_ store $ \entry -> entry edits ectxt
    runAsync <- getRunner async updateP
    (objectC, a) <- uobj runAsync
    let
        child :: UpdatingObject edit a
        child updateC = do
            key <- liftIO $ mvarRun var $ addStoreStateT updateC
            lifeCycleClose $ mvarRun var $ deleteStoreStateT key
            return (objectC, a)
    return child

mapUpdates :: forall edita editb. EditLens edita editb -> Object edita -> [edita] -> IO [editb]
mapUpdates (MkCloseUnlift unlift (MkAnEditLens (MkAnEditFunction _ update) _)) (MkCloseUnliftIO unliftIO (MkAnObject mr _)) eas =
    runTransform unliftIO $
    runUnlift unlift $ withTransConstraintTM @MonadIO $ fmap mconcat $ for eas $ \ea -> update ea mr

lensUpdatingObject :: EditLens edita editb -> UpdatingObject edita a -> UpdatingObject editb a
lensUpdatingObject lens uobja recvb = do
    rec
        let
            recva [] _esrc = return ()
            recva eas esrc = do
                ebs <- mapUpdates lens obja eas
                recvb ebs esrc
        (obja, a) <- uobja recva
    let objb = lensObject True lens obja
    return (objb, a)
