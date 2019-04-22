module Truth.Core.Object.Subscriber
    ( mapUpdates
    , ReceiveUpdatesM
    , ReceiveUpdates
    , ReceiveUpdatesT
    , mapReceiveUpdates
    , mapReceiveUpdatesT
    , Subscriber(..)
    , makeObjectSubscriber
    , liftIO
    , UpdatingObject
    , updatingObject
    , makeSharedSubscriber
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.DeferActionT
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.Update
import Truth.Core.Read

data Subscriber edit = MkSubscriber
    { subObject :: Object edit
    , subscribe :: ([edit] -> EditContext -> IO ()) -> LifeCycleIO ()
    }

type UpdateStoreEntry edit = [edit] -> EditContext -> IO ()

type UpdateStore edit = Store (UpdateStoreEntry edit)

type UpdatingObject edit a = ([edit] -> EditSource -> IO ()) -> LifeCycleIO (Object edit, a)

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

getRunner :: Bool -> ([edit] -> EditContext -> IO ()) -> LifeCycleIO ([edit] -> EditSource -> IO ())
getRunner False handler =
    return $ \edits editContextSource -> handler edits $ MkEditContext {editContextAsync = False, ..}
getRunner True handler = do
    runAsync <-
        asyncRunner $ \(MkEditQueue sourcededits) ->
            for_ sourcededits $ \(editContextSource, edits) -> handler edits MkEditContext {editContextAsync = True, ..}
    return $ \edits esrc -> runAsync $ singleEditQueue edits esrc

makeSharedSubscriber :: forall edit a. Bool -> UpdatingObject edit a -> LifeCycleIO (Subscriber edit, a)
makeSharedSubscriber async uobj = do
    var :: MVar (UpdateStore edit) <- liftIO $ newMVar emptyStore
    let
        updateP :: [edit] -> EditContext -> IO ()
        updateP edits ectxt = do
            store <- mvarRun var get
            for_ store $ \entry -> entry edits ectxt
    runAsync <- getRunner async updateP
    (objectC, a) <- uobj runAsync
    let
        child :: Subscriber edit
        child =
            MkSubscriber objectC $ \updateC ->
                case objectC of
                    MkObject runC _ _ -> do
                        key <- liftIO $ runTransform runC $ mvarRun var $ addStoreStateT updateC
                        lifeCycleClose $ runTransform runC $ mvarRun var $ deleteStoreStateT key
    return (child, a)

updatingObject :: forall edit. Object edit -> UpdatingObject edit ()
updatingObject (MkObject (run :: UnliftIO m) r e) update =
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
                        lift $ action esrc
                        deferActionT $ update edits esrc
        in (MkObject run' r' e', ())

makeObjectSubscriber :: Bool -> Object edit -> LifeCycleIO (Subscriber edit)
makeObjectSubscriber async object = do
    (sub, ()) <- makeSharedSubscriber async $ updatingObject object
    return sub
