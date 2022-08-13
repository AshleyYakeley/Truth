module Changes.Core.Types.Bi.Bi where

import Changes.Core.Edit
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.None
import Changes.Core.Types.ReadOnly
import Shapes

type BiReader (preader :: Type -> Type) (qreader :: Type -> Type) = qreader

newtype BiEdit (pedit :: Type) (qedit :: Type) = MkBiEdit
    { unBiEdit :: pedit
    }

type instance EditReader (BiEdit pedit qedit) = BiReader (EditReader pedit) (EditReader qedit)

newtype BiUpdate (pupdate :: Type) (qupdate :: Type) =
    MkBiUpdate qupdate

type instance UpdateEdit (BiUpdate pupdate qupdate) = BiEdit (UpdateEdit pupdate) (UpdateEdit qupdate)

biSingleChangeLens :: forall update. ChangeLens (BiUpdate update update) update
biSingleChangeLens = let
    clRead :: ReadFunction (UpdateReader update) (UpdateReader update)
    clRead rd = rd
    clUpdate ::
           forall m. MonadIO m
        => BiUpdate update update
        -> Readable m (UpdateReader update)
        -> m [update]
    clUpdate (MkBiUpdate update) _ = return [update]
    clPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> Readable m (UpdateReader update)
        -> m (Maybe [BiEdit (UpdateEdit update) (UpdateEdit update)])
    clPutEdits = clPutEditsFromSimplePutEdit $ \edit -> return $ Just [MkBiEdit edit]
    in MkChangeLens {..}

singleBiChangeLens :: forall update. ChangeLens update (BiUpdate update update)
singleBiChangeLens = let
    clRead :: ReadFunction (UpdateReader update) (UpdateReader update)
    clRead rd = rd
    clUpdate ::
           forall m. MonadIO m
        => update
        -> Readable m (UpdateReader update)
        -> m [BiUpdate update update]
    clUpdate update _ = return [MkBiUpdate update]
    clPutEdits ::
           forall m. MonadIO m
        => [BiEdit (UpdateEdit update) (UpdateEdit update)]
        -> Readable m (UpdateReader update)
        -> m (Maybe [UpdateEdit update])
    clPutEdits = clPutEditsFromSimplePutEdit $ \(MkBiEdit edit) -> return $ Just [edit]
    in MkChangeLens {..}

biReadOnlyChangeLens :: forall pupdate qupdate. ChangeLens (BiUpdate pupdate qupdate) (ReadOnlyUpdate qupdate)
biReadOnlyChangeLens = let
    clRead :: ReadFunction (UpdateReader qupdate) (UpdateReader qupdate)
    clRead rd = rd
    clUpdate ::
           forall m. MonadIO m
        => BiUpdate pupdate qupdate
        -> Readable m (UpdateReader qupdate)
        -> m [ReadOnlyUpdate qupdate]
    clUpdate (MkBiUpdate qupdate) _ = return [MkReadOnlyUpdate qupdate]
    in MkChangeLens {clPutEdits = clPutEditsNone, ..}

readOnlyBiChangeLens ::
       forall pupdate qupdate. ChangeLens (ReadOnlyUpdate qupdate) (ReadOnlyUpdate (BiUpdate pupdate qupdate))
readOnlyBiChangeLens = let
    clRead :: ReadFunction (UpdateReader qupdate) (UpdateReader qupdate)
    clRead rd = rd
    clUpdate ::
           forall m. MonadIO m
        => ReadOnlyUpdate qupdate
        -> Readable m (UpdateReader qupdate)
        -> m [ReadOnlyUpdate (BiUpdate pupdate qupdate)]
    clUpdate (MkReadOnlyUpdate qupdate) _ = return [MkReadOnlyUpdate $ MkBiUpdate qupdate]
    in MkChangeLens {clPutEdits = clPutEditsNone, ..}

convertBiChangeLens ::
       forall updateAP updateAQ updateBP updateBQ.
       ( UpdateSubject updateAP ~ UpdateSubject updateBP
       , UpdateSubject updateAQ ~ UpdateSubject updateBQ
       , FullSubjectReader (UpdateReader updateAQ)
       , FullSubjectReader (UpdateReader updateBP)
       , SubjectReader (UpdateReader updateBQ)
       , FullUpdate updateBQ
       , FullEdit (UpdateEdit updateAP)
       , ApplicableEdit (UpdateEdit updateBP)
       )
    => (UpdateSubject updateAQ -> UpdateSubject updateAP)
    -> ChangeLens (BiUpdate updateAP updateAQ) (BiUpdate updateBP updateBQ)
convertBiChangeLens qp = let
    clRead :: ReadFunction (UpdateReader updateAQ) (UpdateReader updateBQ)
    clRead mr = mSubjectToReadable $ readableToSubject mr
    clUpdate ::
           forall m. MonadIO m
        => BiUpdate updateAP updateAQ
        -> Readable m (UpdateReader updateAQ)
        -> m [BiUpdate updateBP updateBQ]
    clUpdate _updateA mr = do
        newa <- readableToSubject mr
        updates <- getReplaceUpdatesFromSubject newa
        return $ fmap MkBiUpdate updates
    clPutEdits ::
           forall m. MonadIO m
        => [BiEdit (UpdateEdit updateBP) (UpdateEdit updateBQ)]
        -> Readable m (UpdateReader updateAQ)
        -> m (Maybe [BiEdit (UpdateEdit updateAP) (UpdateEdit updateAQ)])
    clPutEdits editbs mr = do
        newsubject <-
            readableToSubject $ applyEdits (fmap unBiEdit editbs) $ mSubjectToReadable $ fmap qp $ readableToSubject mr
        editas <- getReplaceEditsFromSubject newsubject
        return $ Just $ fmap MkBiEdit editas
    in MkChangeLens {..}
