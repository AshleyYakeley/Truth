module Truth.Core.UI.TextEntry where
{
    import Truth.Core.Import;
    import Truth.Core.Types;
    import Truth.Core.UI.Specifier;


    data UITextEntry edit where
    {
        MkUITextEntry :: UITextEntry (WholeEdit String);
    };

    instance Show (UITextEntry edit) where
    {
        show MkUITextEntry = "text entry";
    };

    instance UIType UITextEntry where
    {
        uiWitness = $(iowitness [t|UITextEntry|]);
    };

    uiTextEntry :: UISpec (WholeEdit String);
    uiTextEntry = MkUISpec MkUITextEntry;
}
