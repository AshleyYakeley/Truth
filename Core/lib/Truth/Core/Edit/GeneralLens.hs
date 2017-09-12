module Truth.Core.Edit.GeneralLens where
{
    import Truth.Core.Import;
    import Truth.Core.Edit.EditFunction;
    import Truth.Core.Edit.EditLens;


    type GeneralLens' m = CloseState (EditLens' m);

    type GeneralLens = GeneralLens' Maybe;

    generalLens :: MonadOne m => GeneralLens' m edita editb -> GeneralLens edita editb;
    generalLens (MkCloseState (MkEditLens ff putedit)) = MkCloseState $ MkEditLens ff $ \s e -> fmap getMaybeOne $ putedit s e;


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

    instance IsGeneralLens (EditLens' m state edita editb) where
    {
        type LensMonad (EditLens' m state edita editb) = m;
        type LensDomain (EditLens' m state edita editb) = edita;
        type LensRange (EditLens' m state edita editb) = editb;

        toGeneralLens' = MkCloseState;
    };
}
