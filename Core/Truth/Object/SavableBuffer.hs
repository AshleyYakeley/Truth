module Truth.Object.SavableBuffer where
{
    --import Truth.Object.Object;
    import Truth.Edit;
    import Truth.Edit.Import;

    data SavableVersion = SavableOriginal | SavableCurrent deriving Eq;

    instance Countable SavableVersion where
    {
        countPrevious = finiteCountPrevious;
        countMaybeNext = finiteCountMaybeNext;
    };

    instance Searchable SavableVersion where
    {
        search = finiteSearch;
    };

    instance Finite SavableVersion where
    {
        allValues = [SavableOriginal,SavableCurrent];
    };

    type Savable a = SavableVersion -> a;

    mkSavable :: a -> a -> Savable a;
    mkSavable a _ SavableOriginal = a;
    mkSavable _ a SavableCurrent = a;

    savableVersionLens :: SavableVersion -> Lens' Identity (Savable a) a;
    savableVersionLens = pickLens;

    data SavableEdit edit = SEEdit edit | SESave | SEUnsave (Subject edit);

    instance (Eq (Subject edit), Edit edit) => Edit (SavableEdit edit) where
    {
        type Subject (SavableEdit edit) = Savable (Subject edit);
        applyEdit (SEEdit edit) = arr (\sav -> mkSavable (sav SavableOriginal) (applyConstFunction (applyEdit edit) (sav SavableCurrent)));
        applyEdit SESave = arr (\sav -> mkSavable (sav SavableCurrent) (sav SavableCurrent));
        applyEdit (SEUnsave a) = arr (\sav -> mkSavable a (sav SavableCurrent));

        invertEdit (SEEdit edit) sav = fmap SEEdit (invertEdit edit (sav SavableCurrent));
        invertEdit SESave sav = Just (SEUnsave (sav SavableOriginal));
        invertEdit (SEUnsave _) sav = Just
         (if (sav SavableOriginal) == (sav SavableCurrent) then SESave else (SEUnsave (sav SavableOriginal)));
    };

    savableLens :: (Applicative m) => Lens' m a b -> Lens' m (Savable a) (Savable b);
    savableLens = cfmap;

    --objSubscribe :: forall edit. ((edit -> IO ()) -> IO (Object edit)) -> Subscribe edit;

    --savable :: (FullEdit edit) => Subscribe edit -> Subscribe (SavableEdit edit);
    --savable sub = objSubscribe foo

    --    foo :: (SavableEdit edit -> IO ()) -> IO (Object (SavableEdit edit))

{-
    data FloatingEditLens' m state edita editb = MkFloatingEditLens
    {
        floatingEditLensSimple :: FloatingLens' m state (Subject edita) (Subject editb),
        floatingEditLensUpdate :: edita -> state -> ConstFunction (Subject edita) (state,Maybe editb),
        floatingEditLensPutEdit :: state -> editb -> ConstFunction (Subject edita) (m (state,edita))    -- m failure means impossible
    };

    savableLens :: FloatingEditLens' Identity (Maybe (Subject edit)) edit (SavableEdit edit);
    savableLens = MkFloatingEditLens
    {

    };
-}
}
