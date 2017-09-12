module Truth.Core.Types.Unit where
{
    import Truth.Core.Import;
    import Truth.Core.Edit;
    import Truth.Core.Types.None;
    import Truth.Core.Types.Whole;


    type UnitEdit = NoEdit (WholeReader ());

    unitEditFunction :: EditFunction c () edit UnitEdit;
    unitEditFunction = constEditFunction ();

    unitLens :: EditLens' c Maybe () edit UnitEdit;
    unitLens = readOnlyEditLens unitEditFunction;
}
