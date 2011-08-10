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
        type Subject (MergeEdit edit) = Merge edit;

        applyEdit (AddOverEdit edit)
    };

    mergeSubjectLens :: EditLens' Identity (MergeEdit edit) edit;
    mergeSubjectLens = MkEditLens
    {
        editLensSimple :: Lens' m (Subject edita) (Subject editb),
        editLensUpdate :: edita -> ConstFunction (Subject edita) (Maybe editb),
        editLensPutEdit :: editb -> ConstFunction (Subject edita) (m edita)
    };
-}
}
