module UI.Truth.GTK.GView where
{
    import Graphics.UI.Gtk;
    import Data.Reity;
    import Truth.Object;
    import Truth.Edit;

    type GView edit = View Widget edit;
    type GViewResult edit = ViewResult Widget edit;
    type MatchView = forall edit. (Edit edit) => Info edit -> Maybe (GView edit);
    type GetView = forall edit. (Edit edit) => Info edit -> GView edit;
}
