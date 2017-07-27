module Truth.Core.Object.HasView where
{
    import Truth.Core.Import;
    import Truth.Core.Object.View;


    class DependentHasView widget edit where
    {
        dependsView :: TypeInfo edit -> KnowM (View edit widget);

        default dependsView :: HasView widget edit => TypeInfo edit -> KnowM (View edit widget);
        dependsView _ = return theView;
    };

    class DependentHasView widget edit => HasView widget edit where
    {
        theView :: View edit widget;
    };

    $(return []);
    instance HasTypeInfo DependentHasView where
    {
        typeWitness = $(generateWitness [t|DependentHasView|]);
        typeName _ = "DependentHasView";
    };

    findView :: forall widget edit. HasTypeInfo widget => TypeInfo edit -> KnowM (View edit widget);
    findView i = do
    {
        ConstraintFact <- askTypeInfo $ applyTypeInfo (applyTypeInfo (typeInfo @DependentHasView) (typeInfo @widget)) i;
        dependsView i;
    };

    namedResult :: MonadOne m => String -> m a -> KnowM a;
    namedResult s ma = case getMaybeOne ma of
    {
        Just a -> pure a;
        Nothing -> kmError s;
    };
}