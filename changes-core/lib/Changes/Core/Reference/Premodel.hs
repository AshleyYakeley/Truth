module Changes.Core.Reference.Premodel
    ( PremodelResult(..)
    , Premodel
    , reflectingPremodel
    , notifyingPremodel
    , mapPremodel
    ) where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Reference.DeferActionT
import Changes.Core.Reference.EditContext
import Changes.Core.Reference.Reference
import Changes.Core.Resource
import Changes.Core.Types

data PremodelResult edit a = MkPremodelResult
    { pmrReference :: Reference edit
    , pmrUpdatesTask :: Task ()
    , pmrValue :: a
    }

instance Functor (PremodelResult edit) where
    fmap ab (MkPremodelResult r u v) = MkPremodelResult r u $ ab v

type Premodel update a
     = Task () -> (ResourceContext -> NonEmpty update -> EditContext -> IO ()) -> LifeCycleIO (PremodelResult (UpdateEdit update) a)

reflectingPremodel ::
       forall update. IsUpdate update
    => Reference (UpdateEdit update)
    -> Premodel update ()
reflectingPremodel (MkResource (trun :: ResourceRunner tt) (MkAReference r e ctask)) utask recv = do
    Dict <- return $ resourceRunnerUnliftAllDict trun
    Dict <- return $ transStackDict @MonadUnliftIO @tt @(DeferActionT IO)
    Refl <- return $ transStackConcatRefl @tt @'[ DeferActionT] @IO
    Dict <- return $ concatMonadTransStackUnliftAllDict @tt @'[ DeferActionT]
    deferRR <- deferActionResourceRunner
    let trun' = combineIndependentResourceRunners trun deferRR
    Dict <- return $ resourceRunnerUnliftAllDict trun'
    let
        r' :: Readable (ApplyStack tt (DeferActionT IO)) (UpdateReader update)
        r' rt = stackUnderliftIO @tt @(DeferActionT IO) $ r rt
        e' :: NonEmpty (UpdateEdit update)
           -> ApplyStack tt (DeferActionT IO) (Maybe (EditSource -> ApplyStack tt (DeferActionT IO) ()))
        e' edits = do
            maction <- stackUnderliftIO @tt @(DeferActionT IO) $ e edits
            case maction of
                Nothing -> return Nothing
                Just action ->
                    return $
                    Just $ \esrc -> do
                        stackUnderliftIO @tt @(DeferActionT IO) $ action esrc
                        stackLift @tt $
                            deferAction @IO $ recv emptyResourceContext (fmap editUpdate edits) $ editSourceContext esrc
        anobj :: AReference (UpdateEdit update) (Concat tt '[ DeferActionT])
        anobj = MkAReference r' e' ctask
        pmrUpdatesTask :: Task ()
        pmrUpdatesTask = utask
        pmrValue = ()
        pmrReference :: Reference (UpdateEdit update)
        pmrReference = MkResource trun' anobj
    return MkPremodelResult {..}

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
    return MkPremodelResult {..}

mapPremodel ::
       forall updateA updateB a.
       ResourceContext
    -> FloatingChangeLens updateA updateB
    -> Premodel updateA a
    -> Premodel updateB a
mapPremodel rc (MkFloatingChangeLens init rlens) uobja utask recvb = do
    rec
        (result, recva) <- do
            MkPremodelResult (MkResource (rr :: _ tt) anobjA) updTask val <- uobja utask recva
            liftIO $ do
                r <- runResourceRunner rc rr $ runFloatInit init $ refRead anobjA
                let
                    lens = rlens r
                    recva' urc eas esrc = do
                        ebs <-
                            runResourceRunner urc rr $
                            fmap mconcat $ for (toList eas) $ \ea -> clUpdate lens ea $ refRead anobjA
                        case nonEmpty ebs of
                            Nothing -> return ()
                            Just ebb -> recvb urc ebb esrc
                    objB :: Reference (UpdateEdit updateB)
                    objB =
                        case resourceRunnerUnliftAllDict rr of
                            Dict -> MkResource rr $ mapAReference lens anobjA
                return (MkPremodelResult objB updTask val, recva')
    return result
