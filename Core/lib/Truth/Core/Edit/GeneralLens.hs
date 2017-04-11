module Truth.Core.Edit.GeneralLens where
{
    import Truth.Core.Import;
    import Truth.Core.Edit.EditLens;
    import Truth.Core.Edit.FloatingEditFunction;
    import Truth.Core.Edit.FloatingEditLens;


    type GeneralLens' m = CloseFloat (FloatingEditLens' m);

    type GeneralLens = GeneralLens' Maybe;

    fixedGeneralLens :: Functor m => EditLens' m edita editb -> GeneralLens' m edita editb;
    fixedGeneralLens lens = MkCloseFloat $ fixedFloatingEditLens lens;

    generalLens :: MonadOne m => GeneralLens' m edita editb -> GeneralLens edita editb;
    generalLens (MkCloseFloat (MkFloatingEditLens ff putedit)) = MkCloseFloat $ MkFloatingEditLens ff $ \s e -> fmap getMaybeOne $ putedit s e;
}
