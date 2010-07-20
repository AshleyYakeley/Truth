module UI.Truth.GTK.GView where
{
    import Graphics.UI.Gtk;
    import Truth.Object;
    import Truth.Edit;
    import Truth.TypeKT;

    type GView edit = View Widget edit;
    type GViewResult edit = ViewResult Widget edit;
    type MatchView = forall edit. (Edit edit) => Info (Type_T edit) -> Maybe (GView edit);
    type GetView = forall edit. (Edit edit) => Info (Type_T edit) -> GView edit;
}
