module Truth.Core.Edit.Lens where

import Truth.Core.Edit.Edit
import Truth.Core.Edit.FullEdit
import Truth.Core.Edit.Function
import Truth.Core.Edit.Unlift
import Truth.Core.Import
import Truth.Core.Read

data AnEditLens t edita editb = MkAnEditLens
    { elFunction :: AnEditFunction t edita editb
    , elPutEdits :: forall m. MonadIO m =>
                                  [editb] -> MutableRead m (EditReader edita) -> t m (Maybe [edita])
    }

type EditLens = CloseUnlift AnEditLens

instance Unliftable AnEditLens where
    fmapUnliftable t1t2 (MkAnEditLens f pe) = MkAnEditLens (fmapUnliftable t1t2 f) (\eb mr -> t1t2 $ pe eb mr)

instance UnliftCategory AnEditLens where
    type UnliftCategoryConstraint AnEditLens edit = ()
    ucId = let
        pe :: forall m edit. MonadIO m
           => [edit]
           -> MutableRead m (EditReader edit)
           -> IdentityT m (Maybe [edit])
        pe edits _ = return $ Just edits
        in MkAnEditLens ucId pe
    ucCompose ::
           forall tab tbc edita editb editc. (MonadTransUnlift tab, MonadTransUnlift tbc)
        => AnEditLens tbc editb editc
        -> AnEditLens tab edita editb
        -> AnEditLens (ComposeT tbc tab) edita editc
    ucCompose (MkAnEditLens efBC peBC) lensAB@(MkAnEditLens efAB _) = let
        peAC ::
               forall m. MonadIO m
            => [editc]
            -> MutableRead m (EditReader edita)
            -> ComposeT tbc tab m (Maybe [edita])
        peAC ec mra =
            case hasTransConstraint @MonadIO @tab @m of
                Dict ->
                    case hasTransConstraint @MonadIO @tbc @(tab m) of
                        Dict ->
                            getCompose $ do
                                ebs <- Compose $ MkComposeT $ peBC ec $ efGet efAB mra
                                Compose $ lift2ComposeT $ elPutEdits lensAB ebs mra
        efAC = ucCompose efBC efAB
        in MkAnEditLens efAC peAC

elPutEditsFromPutEdit ::
       (MonadTransConstraint MonadIO t, MonadIO m, Edit edita)
    => (editb -> MutableRead m (EditReader edita) -> t m (Maybe [edita]))
    -> [editb]
    -> MutableRead m (EditReader edita)
    -> t m (Maybe [edita])
elPutEditsFromPutEdit _ [] _ = withTransConstraintTM @MonadIO $ getCompose $ return []
elPutEditsFromPutEdit elPutEdit (e:ee) mr =
    withTransConstraintTM @MonadIO $
    getCompose $ do
        ea <- Compose $ elPutEdit e mr
        eea <- Compose $ elPutEditsFromPutEdit elPutEdit ee $ applyEdits ea mr
        return $ ea ++ eea

elPutEditsFromSimplePutEdit ::
       (MonadTransConstraint MonadIO t, MonadIO m)
    => (editb -> t m (Maybe [edita]))
    -> [editb]
    -> MutableRead m (EditReader edita)
    -> t m (Maybe [edita])
elPutEditsFromSimplePutEdit putEdit editBs _ =
    withTransConstraintTM @MonadIO $
    getCompose $ do
        editAss <- for editBs $ \edit -> Compose $ putEdit edit
        return $ mconcat editAss

editLensFunction :: EditLens edita editb -> EditFunction edita editb
editLensFunction (MkCloseUnlift unlift (MkAnEditLens func _)) = MkCloseUnlift unlift func

readOnlyEditLens :: EditFunction edita editb -> EditLens edita editb
readOnlyEditLens (MkCloseUnlift unlift elFunction) = let
    elPutEdits [] _ = withTransConstraintTM @MonadIO $ return $ Just []
    -- must allow empty edit-lists so that composition works correctly
    elPutEdits (_:_) _ = withTransConstraintTM @MonadIO $ return Nothing
    in MkCloseUnlift unlift $ MkAnEditLens {..}

constEditLens ::
       forall edita editb. SubjectReader (EditReader editb)
    => EditSubject editb
    -> EditLens edita editb
constEditLens b = readOnlyEditLens $ constEditFunction b

convertAnEditFunction ::
       forall edita editb.
       (EditSubject edita ~ EditSubject editb, FullSubjectReader (EditReader edita), Edit edita, FullEdit editb)
    => AnEditFunction IdentityT edita editb
convertAnEditFunction = let
    efGet :: ReadFunctionT IdentityT (EditReader edita) (EditReader editb)
    efGet mr = mSubjectToMutableRead $ lift $ mutableReadToSubject mr
    efUpdate ::
           forall m. MonadIO m
        => edita
        -> MutableRead m (EditReader edita)
        -> IdentityT m [editb]
    efUpdate edita mr = do
        newa <- lift $ mutableReadToSubject $ applyEdit edita mr
        getReplaceEditsFromSubject newa
    in MkAnEditFunction {..}

convertEditFunction ::
       forall edita editb.
       (EditSubject edita ~ EditSubject editb, FullSubjectReader (EditReader edita), Edit edita, FullEdit editb)
    => EditFunction edita editb
convertEditFunction = MkCloseUnlift identityUnlift convertAnEditFunction

convertEditLens ::
       forall edita editb. (EditSubject edita ~ EditSubject editb, FullEdit edita, FullEdit editb)
    => EditLens edita editb
convertEditLens = let
    elFunction :: AnEditFunction IdentityT edita editb
    elFunction = convertAnEditFunction
    elPutEdits ::
           forall m. MonadIO m
        => [editb]
        -> MutableRead m (EditReader edita)
        -> IdentityT m (Maybe [edita])
    elPutEdits editbs mr = do
        newsubject <- lift $ mutableReadToSubject $ applyEdits editbs $ mSubjectToMutableRead $ mutableReadToSubject mr
        editas <- getReplaceEditsFromSubject newsubject
        return $ Just editas
    in MkCloseUnlift identityUnlift MkAnEditLens {..}

class IsEditLens lens where
    type LensDomain lens :: *
    type LensRange lens :: *
    toEditLens :: lens -> EditLens (LensDomain lens) (LensRange lens)

instance IsEditLens (EditLens edita editb) where
    type LensDomain (EditLens edita editb) = edita
    type LensRange (EditLens edita editb) = editb
    toEditLens = id
