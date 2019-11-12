module Truth.Core.Edit.Lens where

import Truth.Core.Edit.Edit
import Truth.Core.Edit.FullEdit
import Truth.Core.Edit.Function
import Truth.Core.Edit.Run
import Truth.Core.Edit.Update
import Truth.Core.Import
import Truth.Core.Read

data AnEditLens tt updateA updateB = MkAnEditLens
    { elFunction :: AnUpdateFunction tt updateA updateB
    , elPutEdits :: forall m.
                        MonadIO m =>
                                [UpdateEdit updateB] -> MutableRead m (UpdateReader updateA) -> ApplyStack tt m (Maybe [UpdateEdit updateA])
    }

type EditLens = Runnable2 AnEditLens

instance RunnableMap AnEditLens where
    mapRunnable t1t2 =
        MkNestedMorphism $
        MkNestedMorphism $ \(MkAnEditLens f pe) ->
            MkAnEditLens
                ((unNestedMorphism $ unNestedMorphism $ mapRunnable t1t2) f)
                (\eb (mr :: MutableRead m _) -> tlfFunction t1t2 (Proxy @m) $ pe eb mr)

instance RunnableCategory AnEditLens where
    ucId :: forall update. AnEditLens '[] update update
    ucId = let
        pe :: forall m. MonadIO m
           => [UpdateEdit update]
           -> MutableRead m (UpdateReader update)
           -> m (Maybe [UpdateEdit update])
        pe edits _ = return $ Just edits
        in MkAnEditLens ucId pe
    ucCompose ::
           forall ttab ttbc updateA updateB editc. (MonadTransStackUnliftAll ttab, MonadTransStackUnliftAll ttbc)
        => AnEditLens ttbc updateB editc
        -> AnEditLens ttab updateA updateB
        -> AnEditLens (Concat ttbc ttab) updateA editc
    ucCompose (MkAnEditLens efBC peBC) lensAB@(MkAnEditLens efAB _) = let
        peAC ::
               forall m. MonadIO m
            => [UpdateEdit editc]
            -> MutableRead m (UpdateReader updateA)
            -> ApplyStack (Concat ttbc ttab) m (Maybe [UpdateEdit updateA])
        peAC ec mra =
            case transStackConcatRefl @ttbc @ttab @m of
                Refl ->
                    case concatMonadTransStackUnliftAllDict @ttbc @ttab of
                        Dict ->
                            case transStackUnliftMonad @(Concat ttbc ttab) @m of
                                Dict ->
                                    case transStackUnliftMonadIO @ttab @m of
                                        Dict ->
                                            getComposeM $ do
                                                ebs <- MkComposeM $ peBC ec $ ufGet efAB mra
                                                MkComposeM $
                                                    tlfFunction (sndTransListFunction @ttbc @ttab) (Proxy @m) $
                                                    elPutEdits lensAB ebs mra
        efAC = ucCompose efBC efAB
        in MkAnEditLens efAC peAC

elPutEditsFromPutEdit ::
       forall tt edita editb m. (MonadTransStackUnliftAll tt, MonadIO m, ApplicableEdit edita)
    => (editb -> MutableRead m (EditReader edita) -> ApplyStack tt m (Maybe [edita]))
    -> [editb]
    -> MutableRead m (EditReader edita)
    -> ApplyStack tt m (Maybe [edita])
elPutEditsFromPutEdit _ [] _ =
    case transStackUnliftMonadIO @tt @m of
        Dict -> getComposeM $ return []
elPutEditsFromPutEdit elPutEdit (e:ee) mr =
    case transStackUnliftMonadIO @tt @m of
        Dict ->
            getComposeM $ do
                ea <- MkComposeM $ elPutEdit e mr
                eea <- MkComposeM $ elPutEditsFromPutEdit @tt elPutEdit ee $ applyEdits ea mr
                return $ ea ++ eea

elPutEditsFromSimplePutEdit ::
       forall tt editA editB m. (MonadTransStackUnliftAll tt, MonadIO m)
    => (editB -> ApplyStack tt m (Maybe [editA]))
    -> [editB]
    -> MutableRead m (EditReader editA)
    -> ApplyStack tt m (Maybe [editA])
elPutEditsFromSimplePutEdit putEdit editBs _ =
    case transStackUnliftMonadIO @tt @m of
        Dict ->
            getComposeM $ do
                editAss <- for editBs $ \update -> MkComposeM $ putEdit update
                return $ mconcat editAss

editLensFunction :: EditLens updateA updateB -> UpdateFunction updateA updateB
editLensFunction (MkRunnable2 unlift (MkAnEditLens func _)) = MkRunnable2 unlift func

readOnlyEditLens :: forall updateA updateB. UpdateFunction updateA updateB -> EditLens updateA updateB
readOnlyEditLens (MkRunnable2 (unlift@(MkTransStackRunner _) :: TransStackRunner tt) elFunction) = let
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit updateB]
        -> MutableRead m (UpdateReader updateA)
        -> ApplyStack tt m (Maybe [UpdateEdit updateA])
    elPutEdits edits _ =
        case transStackUnliftMonadIO @tt @m of
            Dict ->
                return $
                case edits of
                    [] -> Just [] -- must allow empty update-lists so that composition works correctly
                    (_:_) -> Nothing
    in MkRunnable2 unlift $ MkAnEditLens {..}

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
    => AnUpdateFunction '[] updateA updateB
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
convertUpdateFunction = MkRunnable2 cmEmpty convertAnUpdateFunction

convertEditLens ::
       forall updateA updateB.
       ( IsUpdate updateB
       , UpdateSubject updateA ~ UpdateSubject updateB
       , FullEdit (UpdateEdit updateA)
       , FullEdit (UpdateEdit updateB)
       )
    => EditLens updateA updateB
convertEditLens = let
    elFunction :: AnUpdateFunction '[] updateA updateB
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
    in MkRunnable2 cmEmpty MkAnEditLens {..}

class IsEditLens lens where
    type LensDomain lens :: Type
    type LensRange lens :: Type
    toEditLens :: lens -> EditLens (LensDomain lens) (LensRange lens)

instance IsEditLens (EditLens updateA updateB) where
    type LensDomain (EditLens updateA updateB) = updateA
    type LensRange (EditLens updateA updateB) = updateB
    toEditLens = id
