module Truth.Core.Lens.State
    ( StateChangeLens(..)
    , StateLensInit
    , makeStateLens
    ) where

import Truth.Core.Edit.Update
import Truth.Core.Import
import Truth.Core.Lens.Floating
import Truth.Core.Lens.Lens
import Truth.Core.Read

data StateChangeLens updateA updateB = forall s. MkStateChangeLens
    { sclInit :: forall m. MonadIO m => Readable m (UpdateReader updateA) -> m s
    , sclRead :: ReadFunctionT (StateT s) (UpdateReader updateA) (UpdateReader updateB)
    , sclUpdate :: forall m. MonadIO m => updateA -> Readable m (UpdateReader updateA) -> StateT s m [updateB]
    , sclPutEdits :: forall m.
                         MonadIO m =>
                                 [UpdateEdit updateB] -> Readable m (UpdateReader updateA) -> StateT s m (Maybe [UpdateEdit updateA])
    }

type StateLensInit reader s = forall m. MonadIO m => Readable m reader -> m s

makeStateLens :: forall updateA updateB. StateChangeLens updateA updateB -> FloatingChangeLens updateA updateB
makeStateLens MkStateChangeLens {sclInit = sclInit :: StateLensInit _ s, ..} = let
    tempLens :: Lens' Identity (s, s) s
    tempLens = let
        lensGet (_, s) = s
        lensPutback snew (sold, _) = Identity (sold, snew)
        in MkLens {..}
    permLens :: Lens' Identity (s, s) s
    permLens = let
        lensGet (s, _) = s
        lensPutback s _ = Identity (s, s)
        in MkLens {..}
    fclInit :: FloatInit (UpdateReader updateA) (MVar (s, s))
    fclInit =
        ReadFloatInit $ \mr -> do
            initial <- sclInit mr
            liftIO $ newMVar (initial, initial)
    fclLens :: MVar (s, s) -> ChangeLens updateA updateB
    fclLens var = let
        clRead :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
        clRead mr rt = dangerousMVarRun var $ lensStateT tempLens $ sclRead mr rt
        clUpdate ::
               forall m. MonadIO m
            => updateA
            -> Readable m (UpdateReader updateA)
            -> m [updateB]
        clUpdate update mr = dangerousMVarRun var $ lensStateT permLens $ sclUpdate update mr
        clPutEdits ::
               forall m. MonadIO m
            => [UpdateEdit updateB]
            -> Readable m (UpdateReader updateA)
            -> m (Maybe [UpdateEdit updateA])
        clPutEdits edits mr = dangerousMVarRun var $ lensStateT tempLens $ sclPutEdits edits mr
        in MkChangeLens {..}
    in MkFloatingChangeLens {..}
