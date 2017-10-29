module Truth.Core.UI.TextEditor where
{
    import Truth.Core.Import;
    import Truth.Core.Types;
    import Truth.Core.UI.Specifier;


    data UIText edit where
    {
        MkStringUIText :: UIText (StringEdit String);
        MkTextUIText :: UIText (StringEdit Text);
    };

    instance Show (UIText edit) where
    {
        show MkStringUIText = "string";
        show MkTextUIText = "text";
    };

    instance UIType UIText where
    {
        uiWitness = $(iowitness [t|UIText|]);
    };

    uiStringText :: UISpec (StringEdit String);
    uiStringText = MkUISpec MkStringUIText;

    uiTextText :: UISpec (StringEdit Text);
    uiTextText = MkUISpec MkTextUIText;
}
