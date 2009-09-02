module UI.Truth.GTK.GView where
{
    import Graphics.UI.Gtk;
    import Data.Changes;
--    import Data.OpenWitness.OpenRep;
--    import Data.Witness;
{-
    data Thing where
    {
        MkThing :: forall a. OpenRep a -> Subscribe a -> Thing;
    };
-}
    data Selection a edita where
    {
        MkSelection :: 
         forall b editb state. (Eq state,HasTypeRep b,HasTypeRep editb,HasNewValue b,CompleteEditScheme b editb) =>
          FloatingLens state a edita b editb -> state -> Selection a edita;
    };

    data WidgetStuff a edita = MkWidgetStuff
    {
        wsWidget :: Widget,
        wsGetSelection :: IO (Maybe (Selection a edita))
    };

    type GView a edita = View (WidgetStuff a edita) a edita;
    type GViewResult a edita = ViewResult (WidgetStuff a edita) edita;

{-
    class (Data.Changes.Editable a) => HasGView a where
    {
        gView :: GView a;
    };

    class (Data.Changes.Editable a) => HasNamedGView a where
    {
        gNamedView :: String -> GView a;
    };
-}
}
