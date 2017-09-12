module Truth.Core.UI.WindowButton where
{
    import Truth.Core.Import;
    import Truth.Core.UI.Specifier;


    data UIWindowButton edit where
    {
        MkUIWindowButton :: UISpec edit -> UIWindowButton edit;
    };

    instance Show (UIWindowButton edit) where
    {
        show (MkUIWindowButton uispec) = "window " ++ show uispec;
    };

    instance UIType UIWindowButton where
    {
        uiWitness = $(iowitness [t|UIWindowButton|]);
    };
}
