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

comonadEditLens :: forall w edit. EditLens (ComonadEdit w edit) edit
comonadEditLens =
    MkCloseUnlift identityUnlift $ let
        ufGet ::
               forall m. MonadIO m
            => MutableRead m (ComonadReader w (EditReader edit))
            -> MutableRead (IdentityT m) (EditReader edit)
        ufGet mr = remonadMutableRead IdentityT $ comonadReadFunction mr
        ufUpdate ::
               forall m. MonadIO m
            => ComonadEdit w edit
            -> MutableRead m (EditReader (ComonadEdit w edit))
            -> IdentityT m [edit]
        ufUpdate (MkComonadEdit edit) _ = return [edit]
        elFunction = MkAnUpdateFunction {..}
        elPutEdits ::
               forall m. MonadIO m
            => [edit]
            -> MutableRead m (EditReader (ComonadEdit w edit))
            -> IdentityT m (Maybe [ComonadEdit w edit])
        elPutEdits edits _ = return $ Just $ fmap MkComonadEdit edits
        in MkAnEditLens {..}

comonadLiftReadFunction :: ReadFunction ra rb -> ReadFunction (ComonadReader w ra) (ComonadReader w rb)
comonadLiftReadFunction rf mr (ReadExtract rbt) = rf (comonadReadFunction mr) rbt

comonadLiftEditLens ::
       forall w edita editb. EditLens edita editb -> EditLens (ComonadEdit w edita) (ComonadEdit w editb)
comonadLiftEditLens (MkCloseUnlift (unlift :: Unlift t) (MkAnEditLens (MkAnUpdateFunction g u) pe)) = let
    g' :: ReadFunctionT t (ComonadReader w (EditReader edita)) (ComonadReader w (EditReader editb))
    g' mr (ReadExtract rt) = g (comonadReadFunction mr) rt
    u' :: forall m. MonadIO m
       => ComonadEdit w edita
       -> MutableRead m (EditReader (ComonadEdit w edita))
       -> t m [ComonadEdit w editb]
    u' (MkComonadEdit edita) mr =
        case hasTransConstraint @MonadIO @t @m of
            Dict -> fmap (fmap MkComonadEdit) $ u edita $ comonadReadFunction mr
    pe' :: forall m. MonadIO m
        => [ComonadEdit w editb]
        -> MutableRead m (EditReader (ComonadEdit w edita))
        -> t m (Maybe [ComonadEdit w edita])
    pe' editbs mr =
        case hasTransConstraint @MonadIO @t @m of
            Dict ->
                fmap (fmap $ fmap MkComonadEdit) $
                pe (fmap (\(MkComonadEdit editb) -> editb) editbs) $ comonadReadFunction mr
    in MkCloseUnlift unlift $ MkAnEditLens (MkAnUpdateFunction g' u') pe'
