module UI.Truth.GTK.GView where
{
	import Graphics.UI.Gtk;
	import Data.Changes;
	import Data.OpenWitness.OpenRep;

	data Thing where
	{
		MkThing :: forall a. OpenRep a -> Subscribe a -> Thing;
	};

	data WidgetStuff = MkWidgetStuff
	{
		wsWidget :: Widget,
		wsGetSelection :: IO (Maybe Thing)
	};

	type GView a = View WidgetStuff a;
	type GViewResult = ViewResult WidgetStuff;

	class (Data.Changes.Editable a) => HasGView a where
	{
		gView :: GView a;
	};

	class (Data.Changes.Editable a) => HasNamedGView a where
	{
		gNamedView :: String -> GView a;
	};
}
