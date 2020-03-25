module Truth.Core.Lens.Lens where

import Truth.Core.Edit.Edit
import Truth.Core.Edit.FullEdit
import Truth.Core.Edit.Update
import Truth.Core.Import
import Truth.Core.Read

data EditLens updateA updateB = MkEditLens
    { elGet :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    , elUpdate :: forall m. MonadIO m => updateA -> Readable m (UpdateReader updateA) -> m [updateB]
    -- ^ the Readable argument will reflect the new value
    , elPutEdits :: forall m.
                        MonadIO m =>
                                [UpdateEdit updateB] -> Readable m (UpdateReader updateA) -> m (Maybe [UpdateEdit updateA])
    }

instance Category EditLens where
    id :: forall update. EditLens update update
    id = let
        elGet :: ReadFunction (UpdateReader update) (UpdateReader update)
        elGet mr = mr
        elUpdate ::
               forall m. MonadIO m
            => update
            -> Readable m (UpdateReader update)
            -> m [update]
        elUpdate update _ = return [update]
        elPutEdits ::
               forall m. MonadIO m
            => [UpdateEdit update]
            -> Readable m (UpdateReader update)
            -> m (Maybe [UpdateEdit update])
        elPutEdits edits _ = return $ Just edits
        in MkEditLens {..}
    (.) :: forall updateA updateB updateC.
           EditLens updateB updateC
        -> EditLens updateA updateB
        -> EditLens updateA updateC
    (MkEditLens gBC uBC peBC) . (MkEditLens gAB uAB peAB) = let
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
        in MkEditLens gAC uAC peAC

elPutEditsFromPutEdit ::
       forall edita editb m m'. (Monad m', MonadIO m, ApplicableEdit edita)
    => (editb -> Readable m (EditReader edita) -> m' (Maybe [edita]))
    -> [editb]
    -> Readable m (EditReader edita)
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
    -> Readable m (EditReader editA)
    -> m (Maybe [editA])
elPutEditsFromSimplePutEdit putEdit editBs _ =
    getComposeM $ do
        editAss <- for editBs $ \update -> MkComposeM $ putEdit update
        return $ mconcat editAss

convertEditLens ::
       forall updateA updateB.
       ( IsUpdate updateB
       , UpdateSubject updateA ~ UpdateSubject updateB
       , FullEdit (UpdateEdit updateA)
       , FullEdit (UpdateEdit updateB)
       )
    => EditLens updateA updateB
convertEditLens = let
    elGet :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    elGet mr = mSubjectToReadable $ readableToSubject mr
    elUpdate ::
           forall m. MonadIO m
        => updateA
        -> Readable m (UpdateReader updateA)
        -> m [updateB]
    elUpdate _updateA mr = do
        newa <- readableToSubject mr
        edits <- getReplaceEditsFromSubject newa
        return $ fmap editUpdate edits
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit updateB]
        -> Readable m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    elPutEdits editbs mr = do
        newsubject <- readableToSubject $ applyEdits editbs $ mSubjectToReadable $ readableToSubject mr
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
