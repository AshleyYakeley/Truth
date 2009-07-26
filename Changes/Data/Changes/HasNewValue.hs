module Data.Changes.HasNewValue where
{
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
}
