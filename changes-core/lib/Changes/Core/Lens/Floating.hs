module Changes.Core.Lens.Floating
    ( FloatInit(..)
    , runFloatInit
    , mapFloatInit
    , mapFFloatInit
    , FloatingChangeLens(..)
    , runFloatingChangeLens
    , changeLensToFloating
    , floatingToMaybeChangeLens
    , floatingToDiscardingChangeLens
    , floatLift
    ) where

import Changes.Core.Edit.Update
import Changes.Core.Import
import Changes.Core.Lens.Lens
import Changes.Core.Read

data FloatInit reader r where
    ReadFloatInit :: (forall m. MonadIO m => Readable m reader -> m r) -> FloatInit reader r
    NoFloatInit :: FloatInit reader ()

runFloatInit :: FloatInit reader r -> forall m. MonadIO m => Readable m reader -> m r
runFloatInit (ReadFloatInit init) = init
runFloatInit NoFloatInit = \_ -> return ()

mapFloatInit :: ReadFunction readerB readerA -> FloatInit readerA r -> FloatInit readerB r
mapFloatInit _ NoFloatInit = NoFloatInit
mapFloatInit rf (ReadFloatInit i) = ReadFloatInit $ \mr -> i $ rf mr

mapFFloatInit :: MonadOne f => ReadFunctionF f readerB readerA -> FloatInit readerA r -> FloatInit readerB (f r)
mapFFloatInit rf init = ReadFloatInit $ \mr -> getComposeM $ runFloatInit init $ rf mr

data FloatingChangeLens updateA updateB = forall r. MkFloatingChangeLens
    { fclInit :: FloatInit (UpdateReader updateA) r
    , fclLens :: r -> ChangeLens updateA updateB
    }

changeLensToFloating :: ChangeLens updateA updateB -> FloatingChangeLens updateA updateB
changeLensToFloating lens = MkFloatingChangeLens NoFloatInit $ \_ -> lens

floatingToMaybeChangeLens :: FloatingChangeLens updateA updateB -> Maybe (ChangeLens updateA updateB)
floatingToMaybeChangeLens (MkFloatingChangeLens NoFloatInit f) = Just $ f ()
floatingToMaybeChangeLens _ = Nothing

floatingToDiscardingChangeLens ::
       forall updateA updateB. FloatingChangeLens updateA updateB -> ChangeLens updateA updateB
floatingToDiscardingChangeLens (MkFloatingChangeLens NoFloatInit rlens) = rlens ()
floatingToDiscardingChangeLens (MkFloatingChangeLens (ReadFloatInit init) rlens) = let
    g :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    g mr rt = do
        r <- init mr
        clRead (rlens r) mr rt
    u :: forall m. MonadIO m
      => updateA
      -> Readable m (UpdateReader updateA)
      -> m [updateB]
    u upd mr = do
        r <- init mr
        clUpdate (rlens r) upd mr
    pe :: forall m. MonadIO m
       => [UpdateEdit updateB]
       -> Readable m (UpdateReader updateA)
       -> m (Maybe [UpdateEdit updateA])
    pe edits mr = do
        r <- init mr
        clPutEdits (rlens r) edits mr
    in MkChangeLens g u pe

runFloatingChangeLens ::
       forall updateA updateB. IO (FloatingChangeLens updateA updateB) -> FloatingChangeLens updateA updateB
runFloatingChangeLens iol = let
    fclInit :: FloatInit (UpdateReader updateA) (ChangeLens updateA updateB)
    fclInit =
        ReadFloatInit $ \mr -> do
            MkFloatingChangeLens init rlens <- liftIO iol
            r <- runFloatInit init mr
            return $ rlens r
    in MkFloatingChangeLens fclInit id

floatLift ::
       ReadFunction (UpdateReader updateC) (UpdateReader updateA)
    -> (ChangeLens updateA updateB -> ChangeLens updateC updateD)
    -> FloatingChangeLens updateA updateB
    -> FloatingChangeLens updateC updateD
floatLift rf mappl (MkFloatingChangeLens init rlens) =
    MkFloatingChangeLens (mapFloatInit rf init) $ \r -> mappl $ rlens r

instance Category FloatingChangeLens where
    id :: forall update. FloatingChangeLens update update
    id = changeLensToFloating id
    (.) :: forall updateA updateB updateC.
           FloatingChangeLens updateB updateC
        -> FloatingChangeLens updateA updateB
        -> FloatingChangeLens updateA updateC
    lensBC . lensAB
        | Just plensBC <- floatingToMaybeChangeLens lensBC
        , Just plensAB <- floatingToMaybeChangeLens lensAB = changeLensToFloating $ plensBC . plensAB
    MkFloatingChangeLens initBC rlensBC . MkFloatingChangeLens initAB rlensAB = let
        initAC =
            ReadFloatInit $ \mr -> do
                r1 <- runFloatInit initAB mr
                r2 <- runFloatInit initBC $ clRead (rlensAB r1) mr
                return (r1, r2)
        rlensAC (r1, r2) = rlensBC r2 . rlensAB r1
        in MkFloatingChangeLens initAC rlensAC
