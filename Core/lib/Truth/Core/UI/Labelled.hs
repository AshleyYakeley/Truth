module Truth.Core.UI.Labelled where
{
    import Truth.Core.Import;
    import Truth.Core.UI.Specifier;


    data UILabelled edit where
    {
        MkUILabelled :: String -> UISpec edit -> UILabelled edit;
    };

    instance Show (UILabelled edit) where
    {
        show (MkUILabelled l s) = "labelled " ++ show l ++ " " ++ show s;
    };

    instance UIType UILabelled where
    {
        uiWitness = $(iowitness [t|UILabelled|]);
    };

    mkUILabelled :: String -> UISpec edit -> UISpec edit;
    mkUILabelled l s = MkUISpec $ MkUILabelled l s;
}
