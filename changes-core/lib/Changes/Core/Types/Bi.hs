module Truth.Core.Types.Bi where

import Shapes
import Truth.Core.Edit
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Types.None
import Truth.Core.Types.ReadOnly

type BiReader (preader :: Type -> Type) (qreader :: Type -> Type) = qreader

newtype BiEdit (pedit :: Type) (qedit :: Type) =
    MkBiEdit pedit

type instance EditReader (BiEdit pedit qedit) =
     BiReader (EditReader pedit) (EditReader qedit)

newtype BiUpdate (pupdate :: Type) (qupdate :: Type) =
    MkBiUpdate qupdate

type instance UpdateEdit (BiUpdate pupdate qupdate) =
     BiEdit (UpdateEdit pupdate) (UpdateEdit qupdate)

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
    clPutEdits ::
           forall m. MonadIO m
        => [_]
        -> Readable m (UpdateReader qupdate)
        -> m (Maybe [_])
    clPutEdits = clPutEditsNone
    in MkChangeLens {..}

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
    clPutEdits ::
           forall m. MonadIO m
        => [_]
        -> Readable m (UpdateReader qupdate)
        -> m (Maybe [_])
    clPutEdits = clPutEditsNone
    in MkChangeLens {..}
