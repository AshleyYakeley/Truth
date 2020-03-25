module Truth.Core.Types.One.Result where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Types.One.Edit
import Truth.Core.Types.One.Read

data ResultOneUpdate (f :: Type -> Type) update where
    SuccessResultOneUpdate :: update -> ResultOneUpdate f update
    NewResultOneUpdate :: f () -> ResultOneUpdate f update

type instance UpdateEdit (ResultOneUpdate f update) =
     OneEdit f (UpdateEdit update)

instance IsUpdate update => IsUpdate (ResultOneUpdate f update) where
    editUpdate (MkOneEdit edit) = SuccessResultOneUpdate $ editUpdate edit

liftResultOneFloatingEditLens ::
       forall f updateA updateB. MonadOne f
    => FloatingEditLens updateA updateB
    -> FloatingEditLens (ResultOneUpdate f updateA) (ResultOneUpdate f updateB)
liftResultOneFloatingEditLens (MkFloatingEditLens (init :: FloatInit _ r) rlens) = let
    sInit :: StateLensInit (OneReader f (UpdateReader updateA)) (f r)
    sInit mr = getComposeM $ runFloatInit init $ oneReadFunctionF mr
    sGet :: ReadFunctionT (StateT (f r)) (OneReader f (UpdateReader updateA)) (OneReader f (UpdateReader updateB))
    sGet mr rt = do
        fr <- get
        case retrieveOne fr of
            SuccessResult r -> lift $ liftOneReadFunction (elGet $ rlens r) mr rt
            FailureResult (MkLimit fx) ->
                case rt of
                    ReadHasOne -> return fx
                    ReadOne _ -> return fx
    sUpdate ::
           forall m. MonadIO m
        => ResultOneUpdate f updateA
        -> Readable m (OneReader f (UpdateReader updateA))
        -> StateT (f r) m [ResultOneUpdate f updateB]
    sUpdate (SuccessResultOneUpdate upda) mr = do
        fr <- get
        case retrieveOne fr of
            SuccessResult r ->
                lift $
                fmap (fmap SuccessResultOneUpdate . fromMaybe [] . getMaybeOne) $
                getComposeM $ elUpdate (rlens r) upda $ oneReadFunctionF mr
            FailureResult _ -> return []
    sUpdate (NewResultOneUpdate fu) mr = do
        case retrieveOne fu of
            SuccessResult () -> do
                r <-
                    lift $
                    runFloatInit init $ \rt -> do
                        ft <- mr $ ReadOne rt
                        case retrieveOne ft of
                            SuccessResult t -> return t
                            FailureResult _ -> liftIO $ fail "liftResultOneFloatingEditLens: missing"
                put $ pure r
                return [NewResultOneUpdate $ pure ()]
            FailureResult (MkLimit fx) -> do
                put fx
                return [NewResultOneUpdate fx]
    sPutEdits ::
           forall m. MonadIO m
        => [OneEdit f (UpdateEdit updateB)]
        -> Readable m (OneReader f (UpdateReader updateA))
        -> StateT (f r) m (Maybe [OneEdit f (UpdateEdit updateA)])
    sPutEdits ebs mr = do
        fr <- get
        case retrieveOne fr of
            SuccessResult r ->
                lift $
                fmap (fmap (fmap MkOneEdit . fromMaybe []) . getMaybeOne) $
                getComposeM $ elPutEdits (rlens r) (fmap (\(MkOneEdit eb) -> eb) ebs) $ oneReadFunctionF mr
            FailureResult _ -> return $ Just []
    in makeStateLens MkStateEditLens {..}
