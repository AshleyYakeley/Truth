module Truth.Object.SavableBuffer where
{
    --import Truth.Object.Object;
    import Truth.Edit;
    import Truth.Edit.Import;

    data Savable a = MkSavable
    {
        savableOriginal :: a,
        savableCurrent :: a
    };

    currentLens :: Lens' Identity (Savable a) a;
    currentLens = MkLens
    {
        lensGet = \(MkSavable _ a) -> a,
        lensPutback = \a -> arr (\(MkSavable original _) -> Identity (MkSavable original a))
    };

    data SavableEdit edit = SEEdit edit | SESave;

    instance (Edit edit) => Edit (SavableEdit edit) where
    {
        type Subject (SavableEdit edit) = Savable (Subject edit);
        applyEdit (SEEdit edit) = arr (\(MkSavable original a) -> MkSavable original (applyConstFunction (applyEdit edit) a));
        applyEdit SESave = arr (\(MkSavable _ a) -> MkSavable a a);

        invertEdit = undefined; -- BUG
    };

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
