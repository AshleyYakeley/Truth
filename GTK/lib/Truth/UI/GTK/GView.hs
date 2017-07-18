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
    newtype MatchView = MkMatchView (forall edit. (Edit edit) => Info edit -> Result [String] (GView edit));
    type GetView = forall edit. (Edit edit) => Info edit -> GView edit;

    instance Monoid MatchView where
    {
        mempty = MkMatchView $ \_ -> FailureResult [];
        mappend (MkMatchView v1) (MkMatchView v2) = MkMatchView $ \i -> case v1 i of
        {
            SuccessResult view -> SuccessResult view;
            FailureResult msgs1 -> case v2 i of
            {
                SuccessResult view -> SuccessResult view;
                FailureResult msgs2 -> FailureResult $ msgs1 ++ msgs2;
            };
        };
    };

    namedMatchView :: String -> (forall edit. (Edit edit) => Info edit -> KnowM (GView edit)) -> MatchView;
    namedMatchView name ff = MkMatchView $ \i -> case ff i of
    {
        SuccessResult view -> SuccessResult view;
        FailureResult msg -> FailureResult [name ++ ": " ++ show msg];
    };

    finalGetView :: MatchView -> ([String] -> GetView) -> GetView;
    finalGetView (MkMatchView mv) gv i = case mv i of
    {
        SuccessResult view -> view;
        FailureResult msgs -> gv msgs i;
    };

    namedResult :: MonadOne m => String -> m a -> KnowM a;
    namedResult s ma = case getMaybeOne ma of
    {
        Just a -> pure a;
        Nothing -> kmError s;
    };
}
