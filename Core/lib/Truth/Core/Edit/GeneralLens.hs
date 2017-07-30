module Truth.Core.Edit.GeneralLens where
{
    import Truth.Core.Import;
    import Truth.Core.Edit.EditLens;
    import Truth.Core.Edit.FloatingEditFunction;
    import Truth.Core.Edit.FloatingEditLens;


    type GeneralLens' m = CloseFloat (IOFloatingEditLens' m);

    type GeneralLens = GeneralLens' Maybe;

    generalLens :: MonadOne m => GeneralLens' m edita editb -> GeneralLens edita editb;
    generalLens (MkCloseFloat (MkFloatingEditLens ff putedit)) = MkCloseFloat $ MkFloatingEditLens ff $ \s e -> fmap getMaybeOne $ putedit s e;


    class IsGeneralLens lens where
    {
        type LensMonad lens :: * -> *;
        type LensDomain lens :: *;
        type LensRange lens :: *;

        toGeneralLens' :: lens -> GeneralLens' (LensMonad lens) (LensDomain lens) (LensRange lens);
    };

    toGeneralLens :: (IsGeneralLens lens,MonadOne (LensMonad lens)) => lens -> GeneralLens (LensDomain lens) (LensRange lens);
    toGeneralLens = generalLens . toGeneralLens';

    instance IsGeneralLens (GeneralLens' m edita editb) where
    {
        type LensMonad (GeneralLens' m edita editb) = m;
        type LensDomain (GeneralLens' m edita editb) = edita;
        type LensRange (GeneralLens' m edita editb) = editb;

        toGeneralLens' = id;
    };

    instance Eq state => IsGeneralLens (IOFloatingEditLens' m state edita editb) where
    {
        type LensMonad (IOFloatingEditLens' m state edita editb) = m;
        type LensDomain (IOFloatingEditLens' m state edita editb) = edita;
        type LensRange (IOFloatingEditLens' m state edita editb) = editb;

        toGeneralLens' = MkCloseFloat;
    };

    instance Eq state => IsGeneralLens (FloatingEditLens' m state edita editb) where
    {
        type LensMonad (FloatingEditLens' m state edita editb) = m;
        type LensDomain (FloatingEditLens' m state edita editb) = edita;
        type LensRange (FloatingEditLens' m state edita editb) = editb;

        toGeneralLens' lens = toGeneralLens' $ (floatingEditLensToGen lens :: GenFloatingEditLens' MonadIO m state edita editb);
    };

    instance Functor m => IsGeneralLens (EditLens' m edita editb) where
    {
        type LensMonad (EditLens' m edita editb) = m;
        type LensDomain (EditLens' m edita editb) = edita;
        type LensRange (EditLens' m edita editb) = editb;

        toGeneralLens' = toGeneralLens' . fixedFloatingEditLens;
    };
}
