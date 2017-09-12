module Truth.Core.Edit.GeneralLens where
{
    import Truth.Core.Import;
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
}
