module Truth.Edit.OneWholeEdit where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;
    import Truth.Edit.Edit;
    import Truth.Edit.Either;
    import Truth.Edit.WholeEdit;
    import Truth.Edit.OneEdit;


    type EitherWholeEdit edit = EitherEdit (WholeEdit (EditReader edit)) edit;

    type OneWholeEdit (f :: * -> *) edit = EitherWholeEdit (OneEdit f edit);

    extractOneWholeEdit :: forall f edit. (MonadOne f,FullEdit edit) => OneWholeEdit f edit -> [edit];
    extractOneWholeEdit (RightEdit (MkOneEdit edit)) = return edit;
    extractOneWholeEdit (LeftEdit (MkWholeEdit fa)) = case retrieveOne fa of
    {
        SuccessResult a -> fromReadable replaceEdit a;
        _ -> [];
    };
}
