module Truth.Core.Object.HasView where
{
    import Truth.Core.Import;
    import Truth.Core.Edit;
    import Truth.Core.Object.View;


    class DependentHasView widget edit where
    {
        dependsView :: TypeKnowledge -> Info edit -> KnowM (View edit widget);

        default dependsView :: HasView widget edit => TypeKnowledge -> Info edit -> KnowM (View edit widget);
        dependsView _ _ = return theView;
    };

    class DependentHasView widget edit => HasView widget edit where
    {
        theView :: View edit widget;
    };

    $(return []);
    instance HasInfo DependentHasView where
    {
        typeWitness = $(generateWitness [t|DependentHasView|]);
        typeName _ = "DependentHasView";
    };

    findView :: forall widget edit. HasInfo widget => TypeKnowledge -> Info edit -> KnowM (View edit widget);
    findView k i = let
    {
        k' = mappend k (infoKnowledge i);
    } in do
    {
        ConstraintFact <- askInfo k' $ applyInfo (applyInfo (info @DependentHasView) (info @widget)) i;
        dependsView k' i;
    };

    type GetView widget = forall edit. (Edit edit) => Info edit -> View edit widget;

    finalGetView :: HasInfo widget => TypeKnowledge -> (FailureReason -> GetView widget) -> GetView widget;
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