module Truth.UI.GTK.GView where
{
    import Control.Monad;
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
    newtype MatchView = MkMatchView (forall edit. (Edit edit) => Info edit -> KnowM (GView edit));
    type GetView = forall edit. (Edit edit) => Info edit -> GView edit;

    instance Monoid MatchView where
    {
        mempty = MkMatchView $ \_ -> mzero;
        mappend (MkMatchView v1) (MkMatchView v2) = MkMatchView $ \i -> mplus (v1 i) (v2 i);
    };

    namedMatchView :: String -> (forall edit. (Edit edit) => Info edit -> KnowM (GView edit)) -> MatchView;
    namedMatchView name ff = MkMatchView $ \i -> kmContext name $ ff i;

    finalGetView :: MatchView -> (FailureReason -> GetView) -> GetView;
    finalGetView (MkMatchView mv) gv i = case mv i of
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
