module Truth.Core.Lens.Floating
    ( FloatInit(..)
    , runFloatInit
    , mapFloatInit
    , mapFFloatInit
    , FloatingEditLens(..)
    , runFloatingEditLens
    , editLensToFloating
    , floatingToMaybeEditLens
    , floatingToDiscardingEditLens
    , floatLift
    ) where

import Truth.Core.Edit.Update
import Truth.Core.Import
import Truth.Core.Lens.Lens
import Truth.Core.Read

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

data FloatingEditLens updateA updateB = forall r. MkFloatingEditLens
    { felInit :: FloatInit (UpdateReader updateA) r
    , felLens :: r -> EditLens updateA updateB
    }

editLensToFloating :: EditLens updateA updateB -> FloatingEditLens updateA updateB
editLensToFloating lens = MkFloatingEditLens NoFloatInit $ \_ -> lens

floatingToMaybeEditLens :: FloatingEditLens updateA updateB -> Maybe (EditLens updateA updateB)
floatingToMaybeEditLens (MkFloatingEditLens NoFloatInit f) = Just $ f ()
floatingToMaybeEditLens _ = Nothing

floatingToDiscardingEditLens :: forall updateA updateB. FloatingEditLens updateA updateB -> EditLens updateA updateB
floatingToDiscardingEditLens (MkFloatingEditLens NoFloatInit rlens) = rlens ()
floatingToDiscardingEditLens (MkFloatingEditLens (ReadFloatInit init) rlens) = let
    g :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    g mr rt = do
        r <- init mr
        elGet (rlens r) mr rt
    u :: forall m. MonadIO m
      => updateA
      -> Readable m (UpdateReader updateA)
      -> m [updateB]
    u upd mr = do
        r <- init mr
        elUpdate (rlens r) upd mr
    pe :: forall m. MonadIO m
       => [UpdateEdit updateB]
       -> Readable m (UpdateReader updateA)
       -> m (Maybe [UpdateEdit updateA])
    pe edits mr = do
        r <- init mr
        elPutEdits (rlens r) edits mr
    in MkEditLens g u pe

runFloatingEditLens :: forall updateA updateB. IO (FloatingEditLens updateA updateB) -> FloatingEditLens updateA updateB
runFloatingEditLens iol = let
    felInit :: FloatInit (UpdateReader updateA) (EditLens updateA updateB)
    felInit =
        ReadFloatInit $ \mr -> do
            MkFloatingEditLens init rlens <- liftIO iol
            r <- runFloatInit init mr
            return $ rlens r
    in MkFloatingEditLens felInit id

floatLift ::
       ReadFunction (UpdateReader updateC) (UpdateReader updateA)
    -> (EditLens updateA updateB -> EditLens updateC updateD)
    -> FloatingEditLens updateA updateB
    -> FloatingEditLens updateC updateD
floatLift rf mappl (MkFloatingEditLens init rlens) = MkFloatingEditLens (mapFloatInit rf init) $ \r -> mappl $ rlens r

instance Category FloatingEditLens where
    id :: forall update. FloatingEditLens update update
    id = editLensToFloating id
    (.) :: forall updateA updateB updateC.
           FloatingEditLens updateB updateC
        -> FloatingEditLens updateA updateB
        -> FloatingEditLens updateA updateC
    lensBC . lensAB
        | Just plensBC <- floatingToMaybeEditLens lensBC
        , Just plensAB <- floatingToMaybeEditLens lensAB = editLensToFloating $ plensBC . plensAB
    MkFloatingEditLens initBC rlensBC . MkFloatingEditLens initAB rlensAB = let
        initAC =
            ReadFloatInit $ \mr -> do
                r1 <- runFloatInit initAB mr
                r2 <- runFloatInit initBC $ elGet (rlensAB r1) mr
                return (r1, r2)
        rlensAC (r1, r2) = rlensBC r2 . rlensAB r1
        in MkFloatingEditLens initAC rlensAC
