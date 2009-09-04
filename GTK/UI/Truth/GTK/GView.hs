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
    data Selection edit where
    {
        MkSelection :: 
         forall editb state. (Eq state,HasTypeRepT editb,HasNewValue (Subject editb),Edit editb) =>
          FloatingLens state edita editb -> state -> Selection edita;
    };

    data WidgetStuff edit = MkWidgetStuff
    {
        wsWidget :: Widget,
        wsGetSelection :: IO (Maybe (Selection edit))
    };

    type GView edit = View (WidgetStuff edit) edit;
    type GViewResult edit = ViewResult (WidgetStuff edit) edit;

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
