module Truth.Core.UI.Layout where
{
    import Truth.Core.Import;
    import Truth.Core.UI.Specifier;


    data UIVertical edit where
    {
        MkUIVertical :: [Aspect edit] -> UIVertical edit;
    };

    instance Show (UIVertical edit) where
    {
        show (MkUIVertical aspects) = "vertical (" ++ intercalate ", " (fmap show aspects) ++ ")";
    };

    instance UIType UIVertical where
    {
        uiWitness = $(iowitness [t|UIVertical|]);
    };

    uiVertical :: [Aspect edit] -> UISpec edit;
    uiVertical aspects = MkUISpec $ MkUIVertical aspects;
}
