module Truth.Core.Edit.FullEdit where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit.Edit;


    class (Edit edit,FullReader c (EditReader edit)) => FullEdit c edit where
    {
        replaceEdit :: WriterReadable c edit (EditReader edit) ();
    };

    type IOFullEdit = FullEdit MonadIO;
    type PureFullEdit = FullEdit Monad;

    ioReplaceEdit :: IOFullEdit edit => IOWriterReadable edit (EditReader edit) ();
    ioReplaceEdit = replaceEdit;

    pureReplaceEdit :: PureFullEdit edit => PureWriterReadable edit (EditReader edit) ();
    pureReplaceEdit = replaceEdit;

    getReplaceEditsM :: forall c m edit. (ReadableConstraint c,FullEdit c edit,Monad m,c m) => EditSubject edit -> m [edit];
    getReplaceEditsM = fromReadable (writerToReadable replaceEdit :: Readable c (EditReader edit) [edit]);

    getReplaceEdits :: forall edit. PureFullEdit edit => EditSubject edit -> [edit];
    getReplaceEdits = fromPureReadable (writerToReadable pureReplaceEdit :: PureReadable (EditReader edit) [edit]);

    instance HasTypeInfo FullEdit where
    {
        typeWitness = $(generateWitness [t|FullEdit|]);
        typeName _ = "FullEdit";
    };
}
