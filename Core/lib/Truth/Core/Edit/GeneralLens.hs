module Truth.Core.Edit.GeneralLens where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit.Edit;
    import Truth.Core.Edit.FullEdit;
    import Truth.Core.Edit.EditFunction;
    import Truth.Core.Edit.EditLens;


    type GeneralLens = CloseState EditLens;

    class IsGeneralLens lens where
    {
        type LensDomain lens :: *;
        type LensRange lens :: *;

        toGeneralLens :: lens -> GeneralLens (LensDomain lens) (LensRange lens);
    };

    instance IsGeneralLens (GeneralLens edita editb) where
    {
        type LensDomain (GeneralLens edita editb) = edita;
        type LensRange (GeneralLens edita editb) = editb;

        toGeneralLens = id;
    };

    instance IsGeneralLens (EditLens state edita editb) where
    {
        type LensDomain (EditLens state edita editb) = edita;
        type LensRange (EditLens state edita editb) = editb;

        toGeneralLens = MkCloseState;
    };

    readOnlyGeneralLens :: forall state edita editb. EditFunction state edita editb -> GeneralLens edita editb;
    readOnlyGeneralLens = MkCloseState . readOnlyEditLens;

    funcROGeneralLens :: forall edita editb. (Edit edita,FullSubjectReader (EditReader edita),FullEdit editb) =>
        (EditSubject edita -> EditSubject editb) -> GeneralLens edita editb;
    funcROGeneralLens = readOnlyGeneralLens . funcEditFunction;

    constGeneralLens :: forall edita editb. (SubjectReader (EditReader editb)) =>
        EditSubject editb -> GeneralLens edita editb;
    constGeneralLens = readOnlyGeneralLens . constEditFunction;

    convertGeneralLens :: forall edita editb. (EditSubject edita ~ EditSubject editb,FullEdit edita,FullEdit editb) =>
        GeneralLens edita editb;
    convertGeneralLens = MkCloseState convertEditLens;
}
