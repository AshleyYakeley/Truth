module Truth.UI.GTK.GView where
{
    import Graphics.UI.Gtk;
    import Data.Reity;
    import Truth.Core;

    type GView edit = View edit Widget;
    type GViewResult edit updatestate selstate = ViewResult edit updatestate selstate Widget;
    type MatchView = forall edit. (Edit edit) => Info edit -> Maybe (GView edit);
    type GetView = forall edit. (Edit edit) => Info edit -> GView edit;
}
