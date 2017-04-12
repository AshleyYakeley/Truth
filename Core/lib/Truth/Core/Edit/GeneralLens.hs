module Truth.Core.Edit.GeneralLens where
{
    import Truth.Core.Import;
    import Truth.Core.Edit.FloatingEditFunction;
    import Truth.Core.Edit.FloatingEditLens;


    type GeneralLens' m = CloseFloat (FloatingEditLens' m);

    type GeneralLens = GeneralLens' Maybe;

    generalLens :: MonadOne m => GeneralLens' m edita editb -> GeneralLens edita editb;
    generalLens (MkCloseFloat (MkFloatingEditLens ff putedit)) = MkCloseFloat $ MkFloatingEditLens ff $ \s e -> fmap getMaybeOne $ putedit s e;
}
