module Truth.Core.Types.Comonad where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read

newtype ComonadReader (w :: Type -> Type) (reader :: Type -> Type) (t :: Type) where
    ReadExtract :: forall w reader t. reader t -> ComonadReader w reader t

instance (Comonad w, SubjectReader reader) => SubjectReader (ComonadReader w reader) where
    type ReaderSubject (ComonadReader w reader) = w (ReaderSubject reader)
    subjectToRead wsubj (ReadExtract reader) = subjectToRead (extract wsubj) reader

comonadReadFunction :: ReadFunction (ComonadReader w reader) reader
comonadReadFunction mr rt = mr $ ReadExtract rt

newtype ComonadEdit (w :: Type -> Type) (edit :: Type) =
    MkComonadEdit edit

instance Floating edit edit => Floating (ComonadEdit w edit) (ComonadEdit w edit) where
    floatingUpdate (MkComonadEdit e1) (MkComonadEdit e2) = MkComonadEdit $ floatingUpdate e1 e2

type instance EditReader (ComonadEdit w edit) =
     ComonadReader w (EditReader edit)

instance ApplicableEdit edit => ApplicableEdit (ComonadEdit w edit) where
    applyEdit (MkComonadEdit edit) = comonadLiftReadFunction $ applyEdit edit

instance InvertibleEdit edit => InvertibleEdit (ComonadEdit w edit) where
    invertEdits edits mr =
        fmap (fmap MkComonadEdit) $ invertEdits (fmap (\(MkComonadEdit edit) -> edit) edits) $ comonadReadFunction mr

newtype ComonadUpdate (w :: Type -> Type) (update :: Type) =
    MkComonadUpdate update

instance IsUpdate update => IsUpdate (ComonadUpdate w update) where
    type UpdateEdit (ComonadUpdate w update) = ComonadEdit w (UpdateEdit update)
    editUpdate (MkComonadEdit edit) = MkComonadUpdate $ editUpdate edit

instance IsEditUpdate update => IsEditUpdate (ComonadUpdate w update) where
    updateEdit (MkComonadUpdate update) = MkComonadEdit $ updateEdit update

comonadEditLens :: forall w update. EditLens (ComonadUpdate w update) update
comonadEditLens =
    MkCloseUnlift identityUnlift $ let
        ufGet ::
               forall m. MonadIO m
            => MutableRead m (ComonadReader w (UpdateReader update))
            -> MutableRead (IdentityT m) (UpdateReader update)
        ufGet mr = remonadMutableRead IdentityT $ comonadReadFunction mr
        ufUpdate ::
               forall m. MonadIO m
            => ComonadUpdate w update
            -> MutableRead m (ComonadReader w (UpdateReader update))
            -> IdentityT m [update]
        ufUpdate (MkComonadUpdate update) _ = return [update]
        elFunction = MkAnUpdateFunction {..}
        elPutEdits ::
               forall m. MonadIO m
            => [UpdateEdit update]
            -> MutableRead m (ComonadReader w (UpdateReader update))
            -> IdentityT m (Maybe [ComonadEdit w (UpdateEdit update)])
        elPutEdits edits _ = return $ Just $ fmap MkComonadEdit edits
        in MkAnEditLens {..}

comonadLiftReadFunction :: ReadFunction ra rb -> ReadFunction (ComonadReader w ra) (ComonadReader w rb)
comonadLiftReadFunction rf mr (ReadExtract rbt) = rf (comonadReadFunction mr) rbt

comonadLiftEditLens ::
       forall w updateA updateB.
       EditLens updateA updateB
    -> EditLens (ComonadUpdate w updateA) (ComonadUpdate w updateB)
comonadLiftEditLens (MkCloseUnlift (unlift :: Unlift t) (MkAnEditLens (MkAnUpdateFunction g u) pe)) = let
    g' :: ReadFunctionT t (ComonadReader w (UpdateReader updateA)) (ComonadReader w (UpdateReader updateB))
    g' mr (ReadExtract rt) = g (comonadReadFunction mr) rt
    u' :: forall m. MonadIO m
       => ComonadUpdate w updateA
       -> MutableRead m (ComonadReader w (UpdateReader updateA))
       -> t m [ComonadUpdate w updateB]
    u' (MkComonadUpdate edita) mr =
        case hasTransConstraint @MonadIO @t @m of
            Dict -> fmap (fmap MkComonadUpdate) $ u edita $ comonadReadFunction mr
    pe' :: forall m. MonadIO m
        => [ComonadEdit w (UpdateEdit updateB)]
        -> MutableRead m (ComonadReader w (UpdateReader updateA))
        -> t m (Maybe [ComonadEdit w (UpdateEdit updateA)])
    pe' editbs mr =
        case hasTransConstraint @MonadIO @t @m of
            Dict ->
                fmap (fmap $ fmap MkComonadEdit) $
                pe (fmap (\(MkComonadEdit editb) -> editb) editbs) $ comonadReadFunction mr
    in MkCloseUnlift unlift $ MkAnEditLens (MkAnUpdateFunction g' u') pe'
