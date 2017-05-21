module Truth.UI.GTK.GView where
{
    import Graphics.UI.Gtk;
    import Data.Reity;
    import Truth.Core;

    type GView edit = View edit Widget;
    type GViewResult edit updatestate selstate = ViewResult edit updatestate selstate Widget;
    newtype MatchView = MkMatchView (forall edit. (Edit edit) => Info edit -> Maybe (GView edit));
    type GetView = forall edit. (Edit edit) => Info edit -> GView edit;

    instance Monoid MatchView where
    {
        mempty = MkMatchView $ \_ -> Nothing;
        mappend (MkMatchView v1) (MkMatchView v2) = MkMatchView $ \i -> case v1 i of
        {
            Just view -> Just view;
            Nothing -> v2 i;
        };
    };

    finalGetView :: MatchView -> GetView -> GetView;
    finalGetView (MkMatchView mv) gv i = case mv i of
    {
        Just view -> view;
        Nothing -> gv i;
    };
}
