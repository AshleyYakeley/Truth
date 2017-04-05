module Truth.UI.GTK.GView where
{
    import Graphics.UI.Gtk;
    import Data.Reity;
    import Truth.Core;

    type GView = View Widget;
    type GViewResult = ViewResult Widget;
    type MatchView = forall edit. (Edit edit) => Info edit -> Maybe (GView edit);
    type GetView = forall edit. (Edit edit) => Info edit -> GView edit;
}
