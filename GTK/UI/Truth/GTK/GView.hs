module UI.Truth.GTK.GView where
{
	import Graphics.UI.Gtk;
	import Data.Changes;
	import Data.Result;
	
	class HasNewValue a where
	{
		newValue :: a;
	};

	instance HasNewValue [a] where
	{
		newValue = [];
	};

	instance HasNewValue (Maybe a) where
	{
		newValue = Nothing;
	};

	instance (HasNewValue a) => HasNewValue (Result err a) where
	{
		newValue = SuccessResult newValue;
	};

	type GView a = View Widget a;
	type GViewResult = ViewResult Widget;

	class (Data.Changes.Editable a) => HasGView a where
	{
		gView :: GView a;
	};

	class (Data.Changes.Editable a) => HasNamedGView a where
	{
		gNamedView :: String -> GView a;
	};
}
