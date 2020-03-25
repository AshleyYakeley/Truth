module Truth.Core.Lens.State
    ( StateEditLens(..)
    , StateLensInit
    , makeStateLens
    ) where

import Truth.Core.Edit.Update
import Truth.Core.Import
import Truth.Core.Lens.Floating
import Truth.Core.Lens.Lens
import Truth.Core.Read

data StateEditLens updateA updateB = forall s. MkStateEditLens
    { sInit :: forall m. MonadIO m => Readable m (UpdateReader updateA) -> m s
    , sGet :: ReadFunctionT (StateT s) (UpdateReader updateA) (UpdateReader updateB)
    , sUpdate :: forall m. MonadIO m => updateA -> Readable m (UpdateReader updateA) -> StateT s m [updateB]
    , sPutEdits :: forall m.
                       MonadIO m =>
                               [UpdateEdit updateB] -> Readable m (UpdateReader updateA) -> StateT s m (Maybe [UpdateEdit updateA])
    }

type StateLensInit reader s = forall m. MonadIO m => Readable m reader -> m s

makeStateLens :: forall updateA updateB. StateEditLens updateA updateB -> FloatingEditLens updateA updateB
makeStateLens MkStateEditLens {sInit = sInit :: StateLensInit _ s, ..} = let
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
    felInit :: FloatInit (UpdateReader updateA) (MVar (s, s))
    felInit =
        ReadFloatInit $ \mr -> do
            initial <- sInit mr
            liftIO $ newMVar (initial, initial)
    felLens :: MVar (s, s) -> EditLens updateA updateB
    felLens var = let
        elGet :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
        elGet mr rt = dangerousMVarRun var $ lensStateT tempLens $ sGet mr rt
        elUpdate ::
               forall m. MonadIO m
            => updateA
            -> Readable m (UpdateReader updateA)
            -> m [updateB]
        elUpdate update mr = dangerousMVarRun var $ lensStateT permLens $ sUpdate update mr
        elPutEdits ::
               forall m. MonadIO m
            => [UpdateEdit updateB]
            -> Readable m (UpdateReader updateA)
            -> m (Maybe [UpdateEdit updateA])
        elPutEdits edits mr = dangerousMVarRun var $ lensStateT tempLens $ sPutEdits edits mr
        in MkEditLens {..}
    in MkFloatingEditLens {..}
