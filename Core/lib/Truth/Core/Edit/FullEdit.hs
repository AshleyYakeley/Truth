module Truth.Core.Edit.FullEdit where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit.Edit;


    class (Edit edit,GenFullReader c (EditReader edit)) => GenFullEdit c edit where
    {
        genReplaceEdit :: GenWriterReadable c edit (EditReader edit) ();

        default genReplaceEdit :: FullEdit edit => IOWriterReadable edit (EditReader edit) ();
        genReplaceEdit = writerReadableToGen genReplaceEdit;
    };

    type IOFullEdit = GenFullEdit MonadIO;
    type FullEdit = GenFullEdit Monad;

    ioReplaceEdit :: IOFullEdit edit => IOWriterReadable edit (EditReader edit) ();
    ioReplaceEdit = genReplaceEdit;

    replaceEdit :: FullEdit edit => WriterReadable edit (EditReader edit) ();
    replaceEdit = genReplaceEdit;

    getReplaceEditsM :: forall c m edit. (ReadableConstraint c,GenFullEdit c edit,Monad m,c m) => EditSubject edit -> m [edit];
    getReplaceEditsM = fromGenReadable (writerToReadable genReplaceEdit :: GenReadable c (EditReader edit) [edit]);

    getReplaceEdits :: forall edit. FullEdit edit => EditSubject edit -> [edit];
    getReplaceEdits = fromReadable (writerToReadable replaceEdit :: Readable (EditReader edit) [edit]);

    instance HasTypeInfo GenFullEdit where
    {
        typeWitness = $(generateWitness [t|GenFullEdit|]);
        typeName _ = "GenFullEdit";
    };
}
