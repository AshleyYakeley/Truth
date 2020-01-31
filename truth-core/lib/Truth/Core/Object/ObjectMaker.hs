module Truth.Core.Object.ObjectMaker
    ( ObjectMaker
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

type ObjectMaker update a = (NonEmpty update -> EditContext -> IO ()) -> LifeCycleIO (Object (UpdateEdit update), a)

reflectingObjectMaker ::
       forall update. IsUpdate update
    => Object (UpdateEdit update)
    -> ObjectMaker update ()
reflectingObjectMaker (MkResource (trun :: ResourceRunner tt) (MkAnObject r e)) recv = do
    Dict <- return $ resourceRunnerUnliftAllDict trun
    Dict <- return $ transStackDict @MonadUnliftIO @tt @(DeferActionT IO)
    Refl <- return $ transStackConcatRefl @tt @'[ DeferActionT] @IO
    Dict <- return $ concatMonadTransStackUnliftAllDict @tt @'[ DeferActionT]
    deferRR <- deferActionResourceRunner
    let trun' = combineIndependentResourceRunners trun deferRR
    Dict <- return $ resourceRunnerUnliftAllDict trun'
    let
        r' :: MutableRead (ApplyStack tt (DeferActionT IO)) (UpdateReader update)
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
                        stackLift @tt $ deferAction @IO $ recv (fmap editUpdate edits) $ editSourceContext esrc
        anobj :: AnObject (UpdateEdit update) (Concat tt '[ DeferActionT])
        anobj = MkAnObject r' e'
    return $ (MkResource trun' anobj, ())

mapObjectMaker ::
       forall updateA updateB a. FloatingEditLens updateA updateB -> ObjectMaker updateA a -> ObjectMaker updateB a
mapObjectMaker (MkFloatingEditLens init rlens) uobja recvb = do
    rec
        (result, recva) <- do
            (MkResource (rr :: _ tt) anobjA, a) <- uobja recva
            runResourceRunnerWith rr $ \run -> do
                r <- liftIO $ run $ runFloatInit init $ objRead anobjA
                let
                    lens = rlens r
                    recva' eas esrc = do
                        ebs <- run $ fmap mconcat $ for (toList eas) $ \ea -> elUpdate lens ea $ objRead anobjA
                        case nonEmpty ebs of
                            Nothing -> return ()
                            Just ebb -> recvb ebb esrc
                    objB :: Object (UpdateEdit updateB)
                    objB = MkResource rr $ mapAnObject lens anobjA
                return ((objB, a), recva')
    return result
