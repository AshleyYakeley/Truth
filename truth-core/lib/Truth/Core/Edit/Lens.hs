module Truth.Core.Edit.Lens where

import Truth.Core.Edit.Edit
import Truth.Core.Edit.FullEdit
import Truth.Core.Edit.Function
import Truth.Core.Edit.Run
import Truth.Core.Edit.Update
import Truth.Core.Import
import Truth.Core.Read

data AnEditLens t updateA updateB = MkAnEditLens
    { elFunction :: AnUpdateFunction t updateA updateB
    , elPutEdits :: forall m.
                        MonadIO m =>
                                [UpdateEdit updateB] -> MutableRead m (UpdateReader updateA) -> t m (Maybe [UpdateEdit updateA])
    }

type EditLens = RunnableT2 AnEditLens

instance Unliftable AnEditLens where
    fmapUnliftable t1t2 (MkAnEditLens f pe) = MkAnEditLens (fmapUnliftable t1t2 f) (\eb mr -> t1t2 $ pe eb mr)

instance RunnableCategory AnEditLens where
    ucId :: forall update. AnEditLens IdentityT update update
    ucId = let
        pe :: forall m. MonadIO m
           => [UpdateEdit update]
           -> MutableRead m (UpdateReader update)
           -> IdentityT m (Maybe [UpdateEdit update])
        pe edits _ = return $ Just edits
        in MkAnEditLens ucId pe
    ucCompose ::
           forall tab tbc updateA updateB editc. (MonadTransUntrans tab, MonadTransUntrans tbc)
        => AnEditLens tbc updateB editc
        -> AnEditLens tab updateA updateB
        -> AnEditLens (ComposeT tbc tab) updateA editc
    ucCompose (MkAnEditLens efBC peBC) lensAB@(MkAnEditLens efAB _) = let
        peAC ::
               forall m. MonadIO m
            => [UpdateEdit editc]
            -> MutableRead m (UpdateReader updateA)
            -> ComposeT tbc tab m (Maybe [UpdateEdit updateA])
        peAC ec mra =
            case hasTransConstraint @MonadIO @tab @m of
                Dict ->
                    case hasTransConstraint @MonadIO @tbc @(tab m) of
                        Dict ->
                            getComposeM $ do
                                ebs <- MkComposeM $ MkComposeT $ peBC ec $ ufGet efAB mra
                                MkComposeM $ lift2ComposeT $ elPutEdits lensAB ebs mra
        efAC = ucCompose efBC efAB
        in MkAnEditLens efAC peAC

elPutEditsFromPutEdit ::
       forall t edita editb m. (MonadTransConstraint MonadIO t, MonadIO m, ApplicableEdit edita)
    => (editb -> MutableRead m (EditReader edita) -> t m (Maybe [edita]))
    -> [editb]
    -> MutableRead m (EditReader edita)
    -> t m (Maybe [edita])
elPutEditsFromPutEdit _ [] _ = withTransConstraintTM @MonadIO $ getComposeM $ return []
elPutEditsFromPutEdit elPutEdit (e:ee) mr =
    withTransConstraintTM @MonadIO $
    getComposeM $ do
        ea <- MkComposeM $ elPutEdit e mr
        eea <- MkComposeM $ elPutEditsFromPutEdit elPutEdit ee $ applyEdits ea mr
        return $ ea ++ eea

elPutEditsFromSimplePutEdit ::
       (MonadTransConstraint MonadIO t, MonadIO m)
    => (editB -> t m (Maybe [editA]))
    -> [editB]
    -> MutableRead m (EditReader editA)
    -> t m (Maybe [editA])
elPutEditsFromSimplePutEdit putEdit editBs _ =
    withTransConstraintTM @MonadIO $
    getComposeM $ do
        editAss <- for editBs $ \update -> MkComposeM $ putEdit update
        return $ mconcat editAss

editLensFunction :: EditLens updateA updateB -> UpdateFunction updateA updateB
editLensFunction (MkRunnableT2 unlift (MkAnEditLens func _)) = MkRunnableT2 unlift func

readOnlyEditLens :: UpdateFunction updateA updateB -> EditLens updateA updateB
readOnlyEditLens (MkRunnableT2 unlift elFunction) = let
    elPutEdits [] _ = withTransConstraintTM @MonadIO $ return $ Just []
    -- must allow empty update-lists so that composition works correctly
    elPutEdits (_:_) _ = withTransConstraintTM @MonadIO $ return Nothing
    in MkRunnableT2 unlift $ MkAnEditLens {..}

funcEditLens ::
       forall updateA updateB.
       ( IsEditUpdate updateA
       , IsUpdate updateB
       , FullSubjectReader (UpdateReader updateA)
       , ApplicableEdit (UpdateEdit updateA)
       , FullEdit (UpdateEdit updateB)
       )
    => (UpdateSubject updateA -> UpdateSubject updateB)
    -> EditLens updateA updateB
funcEditLens f = readOnlyEditLens $ funcUpdateFunction f

constEditLens ::
       forall updateA updateB. SubjectReader (UpdateReader updateB)
    => UpdateSubject updateB
    -> EditLens updateA updateB
constEditLens b = readOnlyEditLens $ constUpdateFunction b

convertAnUpdateFunction ::
       forall updateA updateB.
       ( IsUpdate updateB
       , UpdateSubject updateA ~ UpdateSubject updateB
       , FullSubjectReader (UpdateReader updateA)
       , ApplicableEdit (UpdateEdit updateA)
       , FullEdit (UpdateEdit updateB)
       )
    => AnUpdateFunction IdentityT updateA updateB
convertAnUpdateFunction = let
    ufGet :: ReadFunctionT IdentityT (UpdateReader updateA) (UpdateReader updateB)
    ufGet mr = mSubjectToMutableRead $ lift $ mutableReadToSubject mr
    ufUpdate ::
           forall m. MonadIO m
        => updateA
        -> MutableRead m (UpdateReader updateA)
        -> IdentityT m [updateB]
    ufUpdate _updateA mr = do
        newa <- lift $ mutableReadToSubject mr
        edits <- getReplaceEditsFromSubject newa
        return $ fmap editUpdate edits
    in MkAnUpdateFunction {..}

convertUpdateFunction ::
       forall updateA updateB.
       ( IsUpdate updateB
       , UpdateSubject updateA ~ UpdateSubject updateB
       , FullSubjectReader (UpdateReader updateA)
       , ApplicableEdit (UpdateEdit updateA)
       , FullEdit (UpdateEdit updateB)
       )
    => UpdateFunction updateA updateB
convertUpdateFunction = MkRunnableT2 wUnIdentityT convertAnUpdateFunction

convertEditLens ::
       forall updateA updateB.
       ( IsUpdate updateB
       , UpdateSubject updateA ~ UpdateSubject updateB
       , FullEdit (UpdateEdit updateA)
       , FullEdit (UpdateEdit updateB)
       )
    => EditLens updateA updateB
convertEditLens = let
    elFunction :: AnUpdateFunction IdentityT updateA updateB
    elFunction = convertAnUpdateFunction
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit updateB]
        -> MutableRead m (UpdateReader updateA)
        -> IdentityT m (Maybe [UpdateEdit updateA])
    elPutEdits editbs mr = do
        newsubject <- lift $ mutableReadToSubject $ applyEdits editbs $ mSubjectToMutableRead $ mutableReadToSubject mr
        editas <- getReplaceEditsFromSubject newsubject
        return $ Just editas
    in MkRunnableT2 wUnIdentityT MkAnEditLens {..}

class IsEditLens lens where
    type LensDomain lens :: Type
    type LensRange lens :: Type
    toEditLens :: lens -> EditLens (LensDomain lens) (LensRange lens)

instance IsEditLens (EditLens updateA updateB) where
    type LensDomain (EditLens updateA updateB) = updateA
    type LensRange (EditLens updateA updateB) = updateB
    toEditLens = id
