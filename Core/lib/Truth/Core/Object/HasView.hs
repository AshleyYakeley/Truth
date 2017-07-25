module Truth.Core.Object.HasView where
{
    import Truth.Core.Import;
    import Truth.Core.Edit;
    import Truth.Core.Object.View;


    class DependentHasView widget edit where
    {
        dependsView :: TypeKnowledge -> TypeInfo edit -> KnowM (View edit widget);

        default dependsView :: HasView widget edit => TypeKnowledge -> TypeInfo edit -> KnowM (View edit widget);
        dependsView _ _ = return theView;
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

    findView :: forall widget edit. HasTypeInfo widget => TypeKnowledge -> TypeInfo edit -> KnowM (View edit widget);
    findView k i = let
    {
        k' = mconcat [baseTypeKnowledge,k,typeInfoKnowledge i];
    } in do
    {
        ConstraintFact <- askTypeInfo k' $ applyTypeInfo (applyTypeInfo (typeInfo @DependentHasView) (typeInfo @widget)) i;
        dependsView k' i;
    };

    type GetView widget = forall edit. (Edit edit) => TypeInfo edit -> View edit widget;

    finalGetView :: HasTypeInfo widget => TypeKnowledge -> (FailureReason -> GetView widget) -> GetView widget;
    finalGetView k gv i = case findView k i of
    {
        SuccessResult view -> view;
        FailureResult frs -> gv (MkFailureReason "No Editor" frs) i;
    };

    namedResult :: MonadOne m => String -> m a -> KnowM a;
    namedResult s ma = case getMaybeOne ma of
    {
        Just a -> pure a;
        Nothing -> kmError s;
    };
}