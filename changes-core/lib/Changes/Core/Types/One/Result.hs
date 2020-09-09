module Changes.Core.Types.One.Result where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.One.Edit
import Changes.Core.Types.One.Read

data ResultOneUpdate (f :: Type -> Type) update where
    SuccessResultOneUpdate :: update -> ResultOneUpdate f update
    NewResultOneUpdate :: f () -> ResultOneUpdate f update

type instance UpdateEdit (ResultOneUpdate f update) =
     OneEdit f (UpdateEdit update)

instance IsUpdate update => IsUpdate (ResultOneUpdate f update) where
    editUpdate (MkOneEdit edit) = SuccessResultOneUpdate $ editUpdate edit

liftResultOneFloatingChangeLens ::
       forall f updateA updateB. MonadOne f
    => FloatingChangeLens updateA updateB
    -> FloatingChangeLens (ResultOneUpdate f updateA) (ResultOneUpdate f updateB)
liftResultOneFloatingChangeLens (MkFloatingChangeLens (init :: FloatInit _ r) rlens) = let
    sclInit :: StateLensInit (OneReader f (UpdateReader updateA)) (f r)
    sclInit mr = getComposeM $ runFloatInit init $ oneReadFunctionF mr
    sclRead :: ReadFunctionT (StateT (f r)) (OneReader f (UpdateReader updateA)) (OneReader f (UpdateReader updateB))
    sclRead mr rt = do
        fr <- get
        case retrieveOne fr of
            SuccessResult r -> lift $ liftOneReadFunction (clRead $ rlens r) mr rt
            FailureResult fn ->
                return $
                case rt of
                    ReadHasOne -> fmap never fn
                    ReadOne _ -> fmap never fn
    sclUpdate ::
           forall m. MonadIO m
        => ResultOneUpdate f updateA
        -> Readable m (OneReader f (UpdateReader updateA))
        -> StateT (f r) m [ResultOneUpdate f updateB]
    sclUpdate (SuccessResultOneUpdate upda) mr = do
        fr <- get
        case retrieveOne fr of
            SuccessResult r ->
                lift $
                fmap (fmap SuccessResultOneUpdate . fromMaybe [] . getMaybeOne) $
                getComposeM $ clUpdate (rlens r) upda $ oneReadFunctionF mr
            FailureResult _ -> return []
    sclUpdate (NewResultOneUpdate fu) mr = do
        case retrieveOne fu of
            SuccessResult () -> do
                r <-
                    lift $
                    runFloatInit init $ \rt -> do
                        ft <- mr $ ReadOne rt
                        case retrieveOne ft of
                            SuccessResult t -> return t
                            FailureResult _ -> liftIO $ fail "liftResultOneFloatingChangeLens: missing"
                put $ pure r
                return [NewResultOneUpdate $ pure ()]
            FailureResult fn -> do
                put $ fmap never fn
                return [NewResultOneUpdate $ fmap never fn]
    sclPutEdits ::
           forall m. MonadIO m
        => [OneEdit f (UpdateEdit updateB)]
        -> Readable m (OneReader f (UpdateReader updateA))
        -> StateT (f r) m (Maybe [OneEdit f (UpdateEdit updateA)])
    sclPutEdits ebs mr = do
        fr <- get
        case retrieveOne fr of
            SuccessResult r ->
                lift $
                fmap (fmap (fmap MkOneEdit . fromMaybe []) . getMaybeOne) $
                getComposeM $ clPutEdits (rlens r) (fmap (\(MkOneEdit eb) -> eb) ebs) $ oneReadFunctionF mr
            FailureResult _ -> return $ Just []
    in makeStateLens MkStateChangeLens {..}
