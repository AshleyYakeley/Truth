module Truth.Core.UI.Lens where
{
    import Truth.Core.Import;
    import Truth.Core.Edit;
    import Truth.Core.UI.Specifier;


    data UILens edit where
    {
        MkUILens :: forall edita editb. Edit editb => UISpec editb -> GeneralLens edita editb -> UILens edita;
    };

    instance Show (UILens edit) where
    {
        show (MkUILens uispec _) = "lens " ++ show uispec;
    };

    instance UIType UILens where
    {
        uiWitness = $(iowitness [t|UILens|]);
    };
}
