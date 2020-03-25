module Truth.Core.Object.ObjectMaker
    ( ObjectMakerResult(..)
    , ObjectMaker
    , reflectingObjectMaker
    , mapObjectMaker
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Object.DeferActionT
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Core.Resource

data ObjectMakerResult edit a = MkObjectMakerResult
    { omrObject :: Object edit
    , omrUpdatesTask :: Task ()
    , omrValue :: a
    }

type ObjectMaker update a
     = Task () -> (ResourceContext -> NonEmpty update -> EditContext -> IO ()) -> LifeCycleIO (ObjectMakerResult (UpdateEdit update) a)

reflectingObjectMaker ::
       forall update. IsUpdate update
    => Object (UpdateEdit update)
    -> ObjectMaker update ()
reflectingObjectMaker (MkResource (trun :: ResourceRunner tt) (MkAnObject r e ctask)) utask recv = do
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
        anobj :: AnObject (UpdateEdit update) (Concat tt '[ DeferActionT])
        anobj = MkAnObject r' e' ctask
        omrUpdatesTask :: Task ()
        omrUpdatesTask = utask
        omrValue = ()
        omrObject :: Object (UpdateEdit update)
        omrObject = MkResource trun' anobj
    return MkObjectMakerResult {..}

mapObjectMaker ::
       forall updateA updateB a.
       ResourceContext
    -> FloatingChangeLens updateA updateB
    -> ObjectMaker updateA a
    -> ObjectMaker updateB a
mapObjectMaker rc (MkFloatingChangeLens init rlens) uobja utask recvb = do
    rec
        (result, recva) <- do
            MkObjectMakerResult (MkResource (rr :: _ tt) anobjA) updTask val <- uobja utask recva
            liftIO $ do
                r <- runResourceRunner rc rr $ runFloatInit init $ objRead anobjA
                let
                    lens = rlens r
                    recva' urc eas esrc = do
                        ebs <-
                            runResourceRunner urc rr $
                            fmap mconcat $ for (toList eas) $ \ea -> clUpdate lens ea $ objRead anobjA
                        case nonEmpty ebs of
                            Nothing -> return ()
                            Just ebb -> recvb urc ebb esrc
                    objB :: Object (UpdateEdit updateB)
                    objB =
                        case resourceRunnerUnliftAllDict rr of
                            Dict -> MkResource rr $ mapAnObject lens anobjA
                return (MkObjectMakerResult objB updTask val, recva')
    return result
