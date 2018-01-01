{-# OPTIONS -fno-warn-redundant-constraints #-}

module Truth.Core.Types.Comonad where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read

newtype ComonadReader (w :: * -> *) (reader :: * -> *) (t :: *) where
    ReadExtract :: forall w reader t. reader t -> ComonadReader w reader t

instance (Comonad w, SubjectReader reader) => SubjectReader (ComonadReader w reader) where
    type ReaderSubject (ComonadReader w reader) = w (ReaderSubject reader)
    subjectToRead wsubj (ReadExtract reader) = subjectToRead (extract wsubj) reader

comonadReadFunction :: ReadFunction (ComonadReader w reader) reader
comonadReadFunction mr rt = mr $ ReadExtract rt

newtype ComonadEdit (w :: * -> *) (edit :: *) =
    MkComonadEdit edit

instance Floating edit edit => Floating (ComonadEdit w edit) (ComonadEdit w edit) where
    floatingUpdate (MkComonadEdit e1) (MkComonadEdit e2) = MkComonadEdit $ floatingUpdate e1 e2

instance Edit edit => Edit (ComonadEdit w edit) where
    type EditReader (ComonadEdit w edit) = ComonadReader w (EditReader edit)
    applyEdit (MkComonadEdit edit) = comonadLiftReadFunction $ applyEdit edit

instance InvertibleEdit edit => InvertibleEdit (ComonadEdit w edit) where
    invertEdit (MkComonadEdit edit) mr = fmap (fmap MkComonadEdit) $ invertEdit edit $ comonadReadFunction mr

comonadEditLens :: forall w edit. EditLens' (ComonadEdit w edit) edit
comonadEditLens =
    MkCloseUnlift identityUnlift $ let
        efGet ::
               forall m. MonadIO m
            => MutableRead m (ComonadReader w (EditReader edit))
            -> MutableRead (IdentityT m) (EditReader edit)
        efGet mr = remonadMutableRead IdentityT $ comonadReadFunction mr
        efUpdate ::
               forall m. MonadIO m
            => ComonadEdit w edit
            -> MutableRead m (EditReader (ComonadEdit w edit))
            -> IdentityT m [edit]
        efUpdate (MkComonadEdit edit) _ = return [edit]
        elFunction = MkAnEditFunction {..}
        elPutEdit ::
               forall m. MonadIO m
            => edit
            -> MutableRead m (EditReader (ComonadEdit w edit))
            -> IdentityT m (Maybe [ComonadEdit w edit])
        elPutEdit edit _ = return $ Just [MkComonadEdit edit]
        in MkAnEditLens {..}

comonadLiftReadFunction :: ReadFunction ra rb -> ReadFunction (ComonadReader w ra) (ComonadReader w rb)
comonadLiftReadFunction rf mr (ReadExtract rbt) = rf (comonadReadFunction mr) rbt

comonadLiftEditLens ::
       forall w edita editb. EditLens' edita editb -> EditLens' (ComonadEdit w edita) (ComonadEdit w editb)
comonadLiftEditLens (MkCloseUnlift (unlift :: Unlift t) (MkAnEditLens (MkAnEditFunction g u) pe)) = let
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
        => ComonadEdit w editb
        -> MutableRead m (EditReader (ComonadEdit w edita))
        -> t m (Maybe [ComonadEdit w edita])
    pe' (MkComonadEdit editb) mr =
        case hasTransConstraint @MonadIO @t @m of
            Dict -> fmap (fmap $ fmap MkComonadEdit) $ pe editb $ comonadReadFunction mr
    in MkCloseUnlift unlift $ MkAnEditLens (MkAnEditFunction g' u') pe'
