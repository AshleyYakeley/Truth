module Truth.Core.Edit.StateEditLens where

import Truth.Core.Edit.Function
import Truth.Core.Edit.Lens
import Truth.Core.Edit.Update
import Truth.Core.Import
import Truth.Core.Read

data StateEditLens updateA updateB = forall s. MkStateEditLens
    { sInit :: forall m. MonadIO m => MutableRead m (UpdateReader updateA) -> m s
    , sGet :: ReadFunctionT (StateT s) (UpdateReader updateA) (UpdateReader updateB)
    , sUpdate :: forall m. MonadIO m => updateA -> MutableRead m (UpdateReader updateA) -> StateT s m [updateB]
    , sPutEdits :: forall m.
                       MonadIO m =>
                               [UpdateEdit updateB] -> MutableRead m (UpdateReader updateA) -> StateT s m (Maybe [UpdateEdit updateA])
    }

makeStateLens ::
       forall m' updateA updateB. MonadIO m'
    => StateEditLens updateA updateB
    -> MFunction m' IO
    -> MutableRead m' (UpdateReader updateA)
    -> LifeCycleIO (EditLens updateA updateB)
makeStateLens MkStateEditLens {..} run initmr = do
    let
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
    initial <- liftIO $ run $ sInit initmr
    var <- liftIO $ newMVar (initial, initial)
    let
        ufGet :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
        ufGet mr rt = dangerousMVarRun var $ lensStateT tempLens $ sGet mr rt
        ufUpdate ::
               forall m. MonadIO m
            => updateA
            -> MutableRead m (UpdateReader updateA)
            -> m [updateB]
        ufUpdate update mr = dangerousMVarRun var $ lensStateT permLens $ sUpdate update mr
        elFunction :: UpdateFunction updateA updateB
        elFunction = MkUpdateFunction {..}
        elPutEdits ::
               forall m. MonadIO m
            => [UpdateEdit updateB]
            -> MutableRead m (UpdateReader updateA)
            -> m (Maybe [UpdateEdit updateA])
        elPutEdits edits mr = dangerousMVarRun var $ lensStateT tempLens $ sPutEdits edits mr
    return MkEditLens {..}

discardingStateLens :: forall updateA updateB. StateEditLens updateA updateB -> EditLens updateA updateB
discardingStateLens MkStateEditLens {..} = let
    ufGet :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    ufGet mr rt = do
        s <- sInit mr
        stateDiscardingUntrans s $ sGet mr rt
    ufUpdate ::
           forall m. MonadIO m
        => updateA
        -> MutableRead m (UpdateReader updateA)
        -> m [updateB]
    ufUpdate update mr = do
        s <- sInit mr
        stateDiscardingUntrans s $ sUpdate update mr
    elFunction :: UpdateFunction updateA updateB
    elFunction = MkUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit updateB]
        -> MutableRead m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    elPutEdits edits mr = do
        s <- sInit mr
        stateDiscardingUntrans s $ sPutEdits edits mr
    in MkEditLens {..}
