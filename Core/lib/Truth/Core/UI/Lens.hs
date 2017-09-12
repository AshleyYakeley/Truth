module Truth.Core.UI.Lens where
{
    import Truth.Core.Import;
    import Truth.Core.Edit;
    import Truth.Core.Types;
    import Truth.Core.UI.Specifier;


    data UILens edit where
    {
        MkUILens :: forall edita editb. Edit editb => GeneralLens edita editb -> UISpec editb -> UILens edita;
    };

    instance Show (UILens edit) where
    {
        show (MkUILens _ uispec) = "lens " ++ show uispec;
    };

    instance UIType UILens where
    {
        uiWitness = $(iowitness [t|UILens|]);
    };

    mapAspectSpec :: (UISpec edita -> UISpec editb) -> Aspect edita -> Aspect editb;
    mapAspectSpec ff (MkAspect name uispec) = MkAspect name (ff uispec);

    mapAspect :: Edit editb => GeneralLens edita editb -> Aspect editb -> Aspect edita;
    mapAspect lens = mapAspectSpec $ MkUISpec . MkUILens lens;

    tupleEditUISpecs :: (TupleWitness FullEdit sel,FiniteTupleSelector sel) =>
        (forall edit. FullEdit edit => sel edit -> UISpec edit) -> [UISpec (TupleEdit sel)];
    tupleEditUISpecs getSpec = fmap (\(MkAnyWitness seledit) -> case tupleWitness (Proxy::Proxy FullEdit) seledit of
    {
        MkConstraintWitness -> case getSpec seledit of
        {
            spec -> MkUISpec $ MkUILens (tupleEditLens seledit) spec;
        };
    }) tupleAllSelectors;
}
