module Truth.Core.Edit.FullEdit where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit.Edit;


    class (Edit edit,FullReader (EditReader edit)) => FullEdit edit where
    {
        replaceEdit :: WriterReadable edit (EditReader edit) ();
    };

    getReplaceEditsM :: forall m edit. (FullEdit edit,MonadIO m) => EditSubject edit -> m [edit];
    getReplaceEditsM = fromReadable (writerToReadable replaceEdit :: Readable (EditReader edit) [edit]);
}
