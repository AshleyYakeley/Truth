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

type instance UpdateEdit (ResultOneUpdate f update) = OneEdit f (UpdateEdit update)

instance IsUpdate update => IsUpdate (ResultOneUpdate f update) where
    editUpdate (MkOneEdit edit) = SuccessResultOneUpdate $ editUpdate edit

liftResultOneFloatingChangeLens ::
       forall f updateA updateB. MonadInner f
    => FloatingChangeLens updateA updateB
    -> FloatingChangeLens (ResultOneUpdate f updateA) (ResultOneUpdate f updateB)
liftResultOneFloatingChangeLens (MkFloatingChangeLens (finit :: FloatInit _ r) rlens) = let
    sclInit :: StateLensInit (OneReader f (UpdateReader updateA)) (f r)
    sclInit mr = unComposeInner $ runFloatInit finit $ oneReadFunctionF mr
    sclRead :: ReadFunctionT (StateT (f r)) (OneReader f (UpdateReader updateA)) (OneReader f (UpdateReader updateB))
    sclRead mr rt = do
        fr <- get
        case retrieveInner fr of
            SuccessResult r -> lift $ liftOneReadFunction (clRead $ rlens r) mr rt
            FailureResult e ->
                return $
                case rt of
                    ReadHasOne -> throwExc e
                    ReadOne _ -> throwExc e
    sclUpdate ::
           forall m. MonadIO m
        => ResultOneUpdate f updateA
        -> Readable m (OneReader f (UpdateReader updateA))
        -> StateT (f r) m [ResultOneUpdate f updateB]
    sclUpdate (SuccessResultOneUpdate upda) mr = do
        fr <- get
        case retrieveInner fr of
            SuccessResult r ->
                lift $
                fmap (fmap SuccessResultOneUpdate . fromMaybe [] . mToMaybe) $
                unComposeInner $ clUpdate (rlens r) upda $ oneReadFunctionF mr
            FailureResult _ -> return []
    sclUpdate (NewResultOneUpdate fu) mr = do
        case retrieveInner fu of
            SuccessResult () -> do
                r <-
                    lift $
                    runFloatInit finit $ \rt -> do
                        ft <- mr $ ReadOne rt
                        case retrieveInner ft of
                            SuccessResult t -> return t
                            FailureResult _ -> liftIO $ fail "liftResultOneFloatingChangeLens: missing"
                put $ pure r
                return [NewResultOneUpdate $ pure ()]
            FailureResult e -> do
                put $ throwExc e
                return [NewResultOneUpdate $ throwExc e]
    sclPutEdits ::
           forall m. MonadIO m
        => [OneEdit f (UpdateEdit updateB)]
        -> Readable m (OneReader f (UpdateReader updateA))
        -> StateT (f r) m (Maybe [OneEdit f (UpdateEdit updateA)])
    sclPutEdits ebs mr = do
        fr <- get
        case retrieveInner fr of
            SuccessResult r ->
                lift $
                fmap (fmap (fmap MkOneEdit . fromMaybe []) . mToMaybe) $
                unComposeInner $ clPutEdits (rlens r) (fmap (\(MkOneEdit eb) -> eb) ebs) $ oneReadFunctionF mr
            FailureResult _ -> return $ Just []
    in makeStateLens @'NonLinear MkStateChangeLens {..}
