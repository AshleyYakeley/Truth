module Truth.Core.UI.Specifier where
{
    import Truth.Core.Import;
    import Truth.Core.Edit;
    import Truth.Core.Types;


    data Aspect edit where
    {
        MkAspect :: forall edita editb. Edit editb => String -> UISpec editb -> GeneralLens edita editb -> Aspect edita;
    };

    instance Show (Aspect edit) where
    {
        show (MkAspect name _ _) = name;
    };

    mapAspect :: (Edit edita,Edit editb) => GeneralLens edita editb -> Aspect editb -> Aspect edita;
    mapAspect lens (MkAspect name spec lens') = MkAspect name spec $ lens' <.> lens;

    data UISpec (edit :: *) where
    {
        MkUISpec :: forall (t :: * -> *) (edit :: *). (Show (t edit),UIType t) => t edit -> UISpec edit;
    };

    instance Show (UISpec edit) where
    {
        show (MkUISpec tedit) = show tedit;
    };

    class UIType (t :: * -> *) where
    {
        uiWitness :: IOWitness t;
    };

    isUISpec :: forall t edit. UIType t => UISpec edit -> Maybe (t edit);
    isUISpec (MkUISpec (tedit :: t' edit)) = do
    {
        Refl <- testEquality (uiWitness @t) (uiWitness @t');
        return tedit;
    };


    tupleEditAspects :: (TupleWitness FullEdit sel,FiniteTupleSelector sel) =>
        (forall edit. FullEdit edit => sel edit -> (String,UISpec edit)) -> [Aspect (TupleEdit sel)];
    tupleEditAspects getSpec = fmap (\(MkAnyWitness seledit) -> case tupleWitness (Proxy::Proxy FullEdit) seledit of
    {
        MkConstraintWitness -> case getSpec seledit of
        {
            (name,spec) -> MkAspect name spec $ tupleEditLens seledit;
        };
    }) tupleAllSelectors;
}
