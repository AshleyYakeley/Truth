module Truth.UI.GTK.GView where
{
    import Graphics.UI.Gtk;
    import Data.Result;
    import Data.MonadOne;
    import Data.Reity;
    import Truth.Core;

    makeButton :: String -> IO () -> IO Button;
    makeButton name action = do
    {
        button <- buttonNew;
        set button [buttonLabel := name];
        _ <- onClicked button action;
        return button;
    };

    type GView edit = View edit Widget;
    type GViewResult edit updatestate selstate = ViewResult edit selstate Widget;

    class DependentHasGView edit where
    {
        dependsGView :: TypeKnowledge -> Info edit -> KnowM (GView edit);

        default dependsGView :: HasGView edit => TypeKnowledge -> Info edit -> KnowM (GView edit);
        dependsGView _ _ = return gview;
    };

    class DependentHasGView edit => HasGView edit where
    {
        gview :: GView edit;
    };

    $(return []);
    instance HasInfo DependentHasGView where
    {
        info = mkSimpleInfo $(ionamedwitness[t|DependentHasGView|]) [];
    };

    getGView :: TypeKnowledge -> Info edit -> KnowM (GView edit);
    getGView k i = let
    {
        k' = mappend k (infoKnowledge i);
    } in do
    {
        ConstraintFact <- askInfo k' $ applyInfo (info @DependentHasGView) i;
        dependsGView k' i;
    };

    type GetView = forall edit. (Edit edit) => Info edit -> GView edit;

    finalGetView :: TypeKnowledge -> (FailureReason -> GetView) -> GetView;
    finalGetView k gv i = case getGView k i of
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
