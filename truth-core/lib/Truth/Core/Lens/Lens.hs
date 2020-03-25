module Truth.Core.Lens.Lens where

import Truth.Core.Edit.Edit
import Truth.Core.Edit.FullEdit
import Truth.Core.Edit.Update
import Truth.Core.Import
import Truth.Core.Read

data ChangeLens updateA updateB = MkChangeLens
    { clRead :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    , clUpdate :: forall m. MonadIO m => updateA -> Readable m (UpdateReader updateA) -> m [updateB]
    -- ^ the Readable argument will reflect the new value
    , clPutEdits :: forall m.
                        MonadIO m =>
                                [UpdateEdit updateB] -> Readable m (UpdateReader updateA) -> m (Maybe [UpdateEdit updateA])
    }

instance Category ChangeLens where
    id :: forall update. ChangeLens update update
    id = let
        clRead :: ReadFunction (UpdateReader update) (UpdateReader update)
        clRead mr = mr
        clUpdate ::
               forall m. MonadIO m
            => update
            -> Readable m (UpdateReader update)
            -> m [update]
        clUpdate update _ = return [update]
        clPutEdits ::
               forall m. MonadIO m
            => [UpdateEdit update]
            -> Readable m (UpdateReader update)
            -> m (Maybe [UpdateEdit update])
        clPutEdits edits _ = return $ Just edits
        in MkChangeLens {..}
    (.) :: forall updateA updateB updateC.
           ChangeLens updateB updateC
        -> ChangeLens updateA updateB
        -> ChangeLens updateA updateC
    (MkChangeLens gBC uBC peBC) . (MkChangeLens gAB uAB peAB) = let
        gAC :: forall m. MonadIO m
            => Readable m (UpdateReader updateA)
            -> Readable m (UpdateReader updateC)
        gAC mra = gBC $ gAB mra
        uAC :: forall m. MonadIO m
            => updateA
            -> Readable m (UpdateReader updateA)
            -> m [updateC]
        uAC updA mrA = do
            updBs <- uAB updA mrA
            updCss <- for updBs $ \updB -> uBC updB $ \rt -> id $ gAB mrA rt
            return $ mconcat updCss
        peAC ::
               forall m. MonadIO m
            => [UpdateEdit updateC]
            -> Readable m (UpdateReader updateA)
            -> m (Maybe [UpdateEdit updateA])
        peAC ec mra =
            getComposeM $ do
                ebs <- MkComposeM $ peBC ec $ gAB mra
                MkComposeM $ peAB ebs mra
        in MkChangeLens gAC uAC peAC

clPutEditsFromPutEdit ::
       forall edita editb m m'. (Monad m', MonadIO m, ApplicableEdit edita)
    => (editb -> Readable m (EditReader edita) -> m' (Maybe [edita]))
    -> [editb]
    -> Readable m (EditReader edita)
    -> m' (Maybe [edita])
clPutEditsFromPutEdit _ [] _ = getComposeM $ return []
clPutEditsFromPutEdit elPutEdit (e:ee) mr =
    getComposeM $ do
        ea <- MkComposeM $ elPutEdit e mr
        eea <- MkComposeM $ clPutEditsFromPutEdit elPutEdit ee $ applyEdits ea mr
        return $ ea ++ eea

clPutEditsFromSimplePutEdit ::
       forall editA editB m. MonadIO m
    => (editB -> m (Maybe [editA]))
    -> [editB]
    -> Readable m (EditReader editA)
    -> m (Maybe [editA])
clPutEditsFromSimplePutEdit putEdit editBs _ =
    getComposeM $ do
        editAss <- for editBs $ \update -> MkComposeM $ putEdit update
        return $ mconcat editAss

convertChangeLens ::
       forall updateA updateB.
       ( IsUpdate updateB
       , UpdateSubject updateA ~ UpdateSubject updateB
       , FullEdit (UpdateEdit updateA)
       , FullEdit (UpdateEdit updateB)
       )
    => ChangeLens updateA updateB
convertChangeLens = let
    clRead :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    clRead mr = mSubjectToReadable $ readableToSubject mr
    clUpdate ::
           forall m. MonadIO m
        => updateA
        -> Readable m (UpdateReader updateA)
        -> m [updateB]
    clUpdate _updateA mr = do
        newa <- readableToSubject mr
        edits <- getReplaceEditsFromSubject newa
        return $ fmap editUpdate edits
    clPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit updateB]
        -> Readable m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    clPutEdits editbs mr = do
        newsubject <- readableToSubject $ applyEdits editbs $ mSubjectToReadable $ readableToSubject mr
        editas <- getReplaceEditsFromSubject newsubject
        return $ Just editas
    in MkChangeLens {..}

class IsChangeLens lens where
    type LensDomain lens :: Type
    type LensRange lens :: Type
    toChangeLens :: lens -> ChangeLens (LensDomain lens) (LensRange lens)

instance IsChangeLens (ChangeLens updateA updateB) where
    type LensDomain (ChangeLens updateA updateB) = updateA
    type LensRange (ChangeLens updateA updateB) = updateB
    toChangeLens = id
