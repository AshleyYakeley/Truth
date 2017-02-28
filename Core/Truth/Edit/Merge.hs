module Truth.Edit.Merge where
{
    import Truth.Edit.Edit;
--    import Truth.Edit.Import;

    data EditStep edit = MkEditStep edit [edit];

    data Merge edit = MkMerge (EditSubject edit) [EditStep edit];

    data MergeEdit edit = AddOverEdit edit | RemoveOverEdit | UnderEdit edit;
{-
    instance (Edit edit) => Edit (MergeEdit edit) where
    {
        type ReaderSubject (MergeEdit edit) = Merge edit;

        applyEdit (AddOverEdit edit)
    };

    mergeSubjectLens :: EditLens' Identity (MergeEdit edit) edit;
    mergeSubjectLens = MkEditLens
    {
        editLensSimple :: Lens' m (ReaderSubject edita) (ReaderSubject editb),
        editLensUpdate :: edita -> ConstFunction (ReaderSubject edita) (Maybe editb),
        editLensPutEdit :: editb -> ConstFunction (ReaderSubject edita) (m edita)
    };
-}
}
