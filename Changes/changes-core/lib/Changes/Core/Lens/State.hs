module Changes.Core.Lens.State
    ( StateChangeLens(..)
    , StateLensInit
    , StateLensVar
    , makeStateExpLens
    , makeStateLens
    ) where

import Changes.Core.Edit.Update
import Changes.Core.Import
import Changes.Core.Lens.Floating
import Changes.Core.Lens.Lens
import Changes.Core.Read

type StateLensInit reader s = forall m. MonadIO m => Readable m reader -> m s

type StateChangeLens :: Linearity -> Type -> Type -> Type -> Type
data StateChangeLens lin s updateA updateB = MkStateChangeLens
    { sclInit :: StateLensInit (UpdateReader updateA) s
    , sclRead :: ReadFunctionT (StateT s) (UpdateReader updateA) (UpdateReader updateB)
    , sclUpdate :: forall m. MonadIO m => updateA -> Readable m (UpdateReader updateA) -> StateT s m [updateB]
    , sclPutEdits :: forall m.
                         MonadIO m =>
                                 [UpdateEdit updateB] -> Readable m (NL lin (UpdateReader updateA)) -> StateT s m (Maybe [UpdateEdit updateA])
    }

type StateLensVar s = MVar (s, s)

makeStateExpLens ::
       forall lin s updateA updateB.
       StateChangeLens lin s updateA updateB
    -> ExpFloatingChangeLens lin (StateLensVar s) updateA updateB
makeStateExpLens MkStateChangeLens {..} = let
    tempLens :: PureLens (s, s) s
    tempLens = let
        lensGet (_, s) = s
        lensPutback snew (sold, _) = Identity (sold, snew)
        in MkLens {..}
    permLens :: PureLens (s, s) s
    permLens = let
        lensGet (s, _) = s
        lensPutback s _ = Identity (s, s)
        in MkLens {..}
    finit :: FloatInit (UpdateReader updateA) (StateLensVar s)
    finit =
        ReadFloatInit $ \mr -> do
            initial <- sclInit mr
            liftIO $ newMVar (initial, initial)
    rlens :: StateLensVar s -> GenChangeLens lin updateA updateB
    rlens var = let
        clRead :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
        clRead mr rt = dangerousMVarRunStateT var $ lensStateT tempLens $ sclRead mr rt
        clUpdate ::
               forall m. MonadIO m
            => updateA
            -> Readable m (UpdateReader updateA)
            -> m [updateB]
        clUpdate update mr = dangerousMVarRunStateT var $ lensStateT permLens $ sclUpdate update mr
        clPutEdits ::
               forall m. MonadIO m
            => [UpdateEdit updateB]
            -> Readable m (NL lin (UpdateReader updateA))
            -> m (Maybe [UpdateEdit updateA])
        clPutEdits edits mr = dangerousMVarRunStateT var $ lensStateT tempLens $ sclPutEdits edits mr
        in MkChangeLens {..}
    in MkExpFloatingChangeLens finit rlens

makeStateLens ::
       forall lin s updateA updateB. IsLinearity lin
    => StateChangeLens lin s updateA updateB
    -> FloatingChangeLens updateA updateB
makeStateLens slens = expToFloatingChangeLens $ makeStateExpLens slens
