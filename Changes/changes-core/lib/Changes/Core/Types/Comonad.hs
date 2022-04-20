module Changes.Core.Types.Comonad where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read

newtype ComonadReader (w :: Type -> Type) (reader :: Type -> Type) (t :: Type) where
    ReadExtract :: forall w reader t. reader t -> ComonadReader w reader t

instance (Comonad w, SubjectReader reader) => SubjectReader (ComonadReader w reader) where
    type ReaderSubject (ComonadReader w reader) = w (ReaderSubject reader)
    subjectToRead wsubj (ReadExtract rd) = subjectToRead (extract wsubj) rd

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

type instance UpdateEdit (ComonadUpdate w update) =
     ComonadEdit w (UpdateEdit update)

instance IsUpdate update => IsUpdate (ComonadUpdate w update) where
    editUpdate (MkComonadEdit edit) = MkComonadUpdate $ editUpdate edit

instance IsEditUpdate update => IsEditUpdate (ComonadUpdate w update) where
    updateEdit (MkComonadUpdate update) = MkComonadEdit $ updateEdit update

comonadChangeLens :: forall w update. ChangeLens (ComonadUpdate w update) update
comonadChangeLens = let
    clRead ::
           forall m. MonadIO m
        => Readable m (ComonadReader w (UpdateReader update))
        -> Readable m (UpdateReader update)
    clRead mr = comonadReadFunction mr
    clUpdate ::
           forall m. MonadIO m
        => ComonadUpdate w update
        -> Readable m (ComonadReader w (UpdateReader update))
        -> m [update]
    clUpdate (MkComonadUpdate update) _ = return [update]
    clPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> Readable m (ComonadReader w (UpdateReader update))
        -> m (Maybe [ComonadEdit w (UpdateEdit update)])
    clPutEdits edits _ = return $ Just $ fmap MkComonadEdit edits
    in MkChangeLens {..}

comonadLiftReadFunction :: ReadFunction ra rb -> ReadFunction (ComonadReader w ra) (ComonadReader w rb)
comonadLiftReadFunction rf mr (ReadExtract rbt) = rf (comonadReadFunction mr) rbt

comonadLiftChangeLens ::
       forall w updateA updateB.
       ChangeLens updateA updateB
    -> ChangeLens (ComonadUpdate w updateA) (ComonadUpdate w updateB)
comonadLiftChangeLens (MkChangeLens g u pe) = let
    g' :: ReadFunction (ComonadReader w (UpdateReader updateA)) (ComonadReader w (UpdateReader updateB))
    g' mr (ReadExtract rt) = g (comonadReadFunction mr) rt
    u' :: forall m. MonadIO m
       => ComonadUpdate w updateA
       -> Readable m (ComonadReader w (UpdateReader updateA))
       -> m [ComonadUpdate w updateB]
    u' (MkComonadUpdate edita) mr = fmap (fmap MkComonadUpdate) $ u edita $ comonadReadFunction mr
    pe' :: forall m. MonadIO m
        => [ComonadEdit w (UpdateEdit updateB)]
        -> Readable m (ComonadReader w (UpdateReader updateA))
        -> m (Maybe [ComonadEdit w (UpdateEdit updateA)])
    pe' editbs mr =
        fmap (fmap $ fmap MkComonadEdit) $ pe (fmap (\(MkComonadEdit editb) -> editb) editbs) $ comonadReadFunction mr
    in MkChangeLens g' u' pe'

comonadLiftFloatingChangeLens ::
       forall w updateA updateB.
       FloatingChangeLens updateA updateB
    -> FloatingChangeLens (ComonadUpdate w updateA) (ComonadUpdate w updateB)
comonadLiftFloatingChangeLens = floatLift comonadReadFunction comonadLiftChangeLens
