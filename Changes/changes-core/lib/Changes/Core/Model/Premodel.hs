module Changes.Core.Model.Premodel
    ( PremodelResult (..)
    , Premodel
    , execPremodel
    , reflectingPremodel
    , notifyingPremodel
    , mapPremodel
    )
where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Model.DeferActionT
import Changes.Core.Model.EditContext
import Changes.Core.Model.Reference
import Changes.Core.Read
import Changes.Core.Resource
import Changes.Core.Types

data PremodelResult edit a = MkPremodelResult
    { pmrReference :: Reference edit
    , pmrUpdatesTask :: Task IO ()
    , pmrValue :: a
    }

instance Functor (PremodelResult edit) where
    fmap ab (MkPremodelResult r u v) = MkPremodelResult r u $ ab v

type Premodel update a =
    Task IO () -> (ResourceContext -> NonEmpty update -> EditContext -> IO ()) -> Lifecycle (PremodelResult (UpdateEdit update) a)

execPremodel :: Lifecycle (Premodel update a) -> Premodel update a
execPremodel mpm utask recv = do
    pm <- mpm
    pm utask recv

reflectingPremodel ::
    forall update.
    IsUpdate update =>
    Reference (UpdateEdit update) ->
    Premodel update ()
reflectingPremodel (MkResource (trun :: ResourceRunner t) (MkAReference r e ctask)) utask recv = do
    deferRR <- deferActionResourceRunner
    let
        r' :: Readable (ReaderT _ IO) (UpdateReader update)
        r' rt = withReaderT fst $ r rt
        e' ::
            NonEmpty (UpdateEdit update) ->
            ReaderT _ IO (Maybe (EditSource -> ReaderT _ IO ()))
        e' edits = do
            maction <- withReaderT fst $ e edits
            return
                $ fmap
                    ( \action esrc -> do
                        withReaderT fst $ action esrc
                        defer <- asks snd
                        liftIO
                            $ deferAction defer
                            $ recv emptyResourceContext (fmap editUpdate edits)
                            $ editSourceContext esrc
                    )
                    maction
        anobj :: AReference (UpdateEdit update) _
        anobj = MkAReference r' e' ctask
        pmrUpdatesTask = utask
        pmrValue = ()
        pmrReference = MkResource (liftA2 (,) trun deferRR) anobj
        in return MkPremodelResult{..}

notifyingPremodel :: forall a. a -> Premodel (ROWUpdate a) ((a -> IO a) -> IO ())
notifyingPremodel initial pmrUpdatesTask recv = do
    wit <- liftIO newIOWitness
    var <- liftIO $ newMVar initial
    let
        pmrValue :: (a -> IO a) -> IO ()
        pmrValue update = do
            val <-
                modifyMVar var $ \oldval -> do
                    newval <- update oldval
                    return (newval, newval)
            recv emptyResourceContext (pure $ MkReadOnlyUpdate $ MkWholeReaderUpdate val) noEditContext
        pmrReference :: Reference (ConstEdit (WholeReader a))
        pmrReference = mapReference (toReadOnlyChangeLens @(WholeUpdate a)) $ mvarReference wit var (\_ -> False)
    return MkPremodelResult{..}

mapPremodel ::
    forall updateA updateB a.
    ResourceContext ->
    FloatingChangeLens updateA updateB ->
    Premodel updateA a ->
    Premodel updateB a
mapPremodel rc (MkFloatingChangeLens finit rlens) premodel utask recvb = do
    rec (result, recva) <- do
            MkPremodelResult (MkResource (rr :: _ tt) anobjA) updTask val <- premodel utask recva
            liftIO $ do
                r <- runResourceRunner rc rr $ runReaderT $ runFloatInit finit $ refRead anobjA
                let
                    lens = rlens r
                    recva' urc eas esrc = do
                        ebs <-
                            runResourceRunner urc rr
                                $ runReaderT
                                $ fmap mconcat
                                $ for (toList eas)
                                $ \ea -> clUpdate lens ea $ refRead anobjA
                        case nonEmpty ebs of
                            Nothing -> return ()
                            Just ebb -> recvb urc ebb esrc
                    objB :: Reference (UpdateEdit updateB)
                    objB = MkResource rr $ mapAReference lens anobjA
                return (MkPremodelResult objB updTask val, recva')
    return result
