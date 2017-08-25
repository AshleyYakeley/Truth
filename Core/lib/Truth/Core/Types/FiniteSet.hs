module Truth.Core.Types.FiniteSet where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Whole;
    import Truth.Core.Types.None;
    import Truth.Core.Types.Key;


    type FiniteSetReader subj = KeyReader (FiniteSet subj) (WholeReader subj);
    type FiniteSetEdit subj = KeyEdit (FiniteSet subj) (NoEdit (WholeReader subj));

    finiteSetLens :: forall subj. Eq subj => subj -> PureEditLens' Identity () (FiniteSetEdit subj) (WholeEdit Bool);
    finiteSetLens subj = let
    {
        editInitial = ();
        editGet :: () -> PureReadFunction (FiniteSetReader subj) (WholeReader Bool);
        editGet () = undefined;

        editUpdate :: FiniteSetEdit subj -> () -> PureReadable (FiniteSetReader subj) ((), [WholeEdit Bool]);
        editUpdate (KeyEditItem _ edit) () = never edit;
        editUpdate (KeyDeleteItem key) () | key == subj = return $ pure [MkWholeEdit False];
        editUpdate (KeyDeleteItem _) () = return $ pure [];
        editUpdate (KeyInsertReplaceItem key) () | key == subj = return $ pure [MkWholeEdit True];
        editUpdate (KeyInsertReplaceItem _) () = return $ pure [];
        editUpdate KeyClear () = return $ pure [MkWholeEdit False];

        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> WholeEdit Bool -> PureReadable (FiniteSetReader subj) (Identity ((), [FiniteSetEdit subj]));
        editLensPutEdit () (MkWholeEdit False) = return $ pure $ pure $ [KeyDeleteItem subj];
        editLensPutEdit () (MkWholeEdit True) = return $ pure $ pure $ [KeyInsertReplaceItem subj];
    }
    in MkEditLens{..};
}
