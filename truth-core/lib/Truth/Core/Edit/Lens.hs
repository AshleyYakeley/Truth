module Truth.Core.Edit.Lens where

import Truth.Core.Edit.Edit
import Truth.Core.Edit.FullEdit
import Truth.Core.Edit.Function
import Truth.Core.Edit.Update
import Truth.Core.Import
import Truth.Core.Read

data EditLens updateA updateB = MkEditLens
    { elFunction :: UpdateFunction updateA updateB
    , elPutEdits :: forall m.
                        MonadIO m =>
                                [UpdateEdit updateB] -> MutableRead m (UpdateReader updateA) -> m (Maybe [UpdateEdit updateA])
    }

instance Category EditLens where
    id :: forall update. EditLens update update
    id = let
        pe :: forall m. MonadIO m
           => [UpdateEdit update]
           -> MutableRead m (UpdateReader update)
           -> m (Maybe [UpdateEdit update])
        pe edits _ = return $ Just edits
        in MkEditLens id pe
    (.) :: forall updateA updateB updateC.
           EditLens updateB updateC
        -> EditLens updateA updateB
        -> EditLens updateA updateC
    MkEditLens efBC peBC . lensAB@(MkEditLens efAB _) = let
        peAC ::
               forall m. MonadIO m
            => [UpdateEdit updateC]
            -> MutableRead m (UpdateReader updateA)
            -> m (Maybe [UpdateEdit updateA])
        peAC ec mra =
            getComposeM $ do
                ebs <- MkComposeM $ peBC ec $ ufGet efAB mra
                MkComposeM $ elPutEdits lensAB ebs mra
        efAC = efBC . efAB
        in MkEditLens efAC peAC

elPutEditsFromPutEdit ::
       forall edita editb m m'. (Monad m', MonadIO m, ApplicableEdit edita)
    => (editb -> MutableRead m (EditReader edita) -> m' (Maybe [edita]))
    -> [editb]
    -> MutableRead m (EditReader edita)
    -> m' (Maybe [edita])
elPutEditsFromPutEdit _ [] _ = getComposeM $ return []
elPutEditsFromPutEdit elPutEdit (e:ee) mr =
    getComposeM $ do
        ea <- MkComposeM $ elPutEdit e mr
        eea <- MkComposeM $ elPutEditsFromPutEdit elPutEdit ee $ applyEdits ea mr
        return $ ea ++ eea

elPutEditsFromSimplePutEdit ::
       forall editA editB m. MonadIO m
    => (editB -> m (Maybe [editA]))
    -> [editB]
    -> MutableRead m (EditReader editA)
    -> m (Maybe [editA])
elPutEditsFromSimplePutEdit putEdit editBs _ =
    getComposeM $ do
        editAss <- for editBs $ \update -> MkComposeM $ putEdit update
        return $ mconcat editAss

editLensFunction :: EditLens updateA updateB -> UpdateFunction updateA updateB
editLensFunction (MkEditLens func _) = func

convertAnUpdateFunction ::
       forall updateA updateB.
       ( IsUpdate updateB
       , UpdateSubject updateA ~ UpdateSubject updateB
       , FullSubjectReader (UpdateReader updateA)
       , ApplicableEdit (UpdateEdit updateA)
       , FullEdit (UpdateEdit updateB)
       )
    => UpdateFunction updateA updateB
convertAnUpdateFunction = let
    ufGet :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    ufGet mr = mSubjectToMutableRead $ mutableReadToSubject mr
    ufUpdate ::
           forall m. MonadIO m
        => updateA
        -> MutableRead m (UpdateReader updateA)
        -> m [updateB]
    ufUpdate _updateA mr = do
        newa <- mutableReadToSubject mr
        edits <- getReplaceEditsFromSubject newa
        return $ fmap editUpdate edits
    in MkUpdateFunction {..}

convertUpdateFunction ::
       forall updateA updateB.
       ( IsUpdate updateB
       , UpdateSubject updateA ~ UpdateSubject updateB
       , FullSubjectReader (UpdateReader updateA)
       , ApplicableEdit (UpdateEdit updateA)
       , FullEdit (UpdateEdit updateB)
       )
    => UpdateFunction updateA updateB
convertUpdateFunction = convertAnUpdateFunction

convertEditLens ::
       forall updateA updateB.
       ( IsUpdate updateB
       , UpdateSubject updateA ~ UpdateSubject updateB
       , FullEdit (UpdateEdit updateA)
       , FullEdit (UpdateEdit updateB)
       )
    => EditLens updateA updateB
convertEditLens = let
    elFunction :: UpdateFunction updateA updateB
    elFunction = convertAnUpdateFunction
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit updateB]
        -> MutableRead m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    elPutEdits editbs mr = do
        newsubject <- mutableReadToSubject $ applyEdits editbs $ mSubjectToMutableRead $ mutableReadToSubject mr
        editas <- getReplaceEditsFromSubject newsubject
        return $ Just editas
    in MkEditLens {..}

class IsEditLens lens where
    type LensDomain lens :: Type
    type LensRange lens :: Type
    toEditLens :: lens -> EditLens (LensDomain lens) (LensRange lens)

instance IsEditLens (EditLens updateA updateB) where
    type LensDomain (EditLens updateA updateB) = updateA
    type LensRange (EditLens updateA updateB) = updateB
    toEditLens = id

data StateEditLens s updateA updateB = MkStateEditLens
    { sGet :: ReadFunctionT (StateT s) (UpdateReader updateA) (UpdateReader updateB)
    , sUpdate :: forall m. MonadIO m => updateA -> MutableRead m (UpdateReader updateA) -> StateT s m [updateB]
    , sPutEdits :: forall m.
                       MonadIO m =>
                               [UpdateEdit updateB] -> MutableRead m (UpdateReader updateA) -> StateT s m (Maybe [UpdateEdit updateA])
    }

makeStateLens ::
       forall s updateA updateB. StateEditLens s updateA updateB -> s -> LifeCycleIO (EditLens updateA updateB)
makeStateLens MkStateEditLens {..} initial = do
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

discardingStateLens :: forall s updateA updateB. StateEditLens s updateA updateB -> s -> EditLens updateA updateB
discardingStateLens MkStateEditLens {..} s = let
    ufGet :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    ufGet mr rt = stateDiscardingUntrans s $ sGet mr rt
    ufUpdate ::
           forall m. MonadIO m
        => updateA
        -> MutableRead m (UpdateReader updateA)
        -> m [updateB]
    ufUpdate update mr = stateDiscardingUntrans s $ sUpdate update mr
    elFunction :: UpdateFunction updateA updateB
    elFunction = MkUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit updateB]
        -> MutableRead m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    elPutEdits edits mr = stateDiscardingUntrans s $ sPutEdits edits mr
    in MkEditLens {..}
