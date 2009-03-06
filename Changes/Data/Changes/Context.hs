module Data.Changes.Context where
{
	import Data.Changes.Tuple;
	import Data.Changes.Edit;
	import Data.FunctorOne;
	import Data.Traversable;
	import Data.Foldable;

	data WithContext context content = MkWithContext context content;

	instance Functor (WithContext context) where
	{
		fmap ab (MkWithContext context a) = MkWithContext context (ab a);
	};

	instance Foldable (WithContext context) where
	{
		foldMap am (MkWithContext _ a) = am a;
	};

	instance Traversable (WithContext context) where
	{
		traverse afb (MkWithContext context a) = fmap (MkWithContext context) (afb a);
		sequenceA (MkWithContext context fa) = fmap (MkWithContext context) fa;
	};

	instance FunctorOne (WithContext context) where
	{
		retrieveOne (MkWithContext _ a) = Right a;
	};
	
	instance IsTuple (WithContext context content) where
	{
		type Tuple (WithContext context content) = (content,(context,()));
		fromListTuple (content,(context,())) = MkWithContext context content;
		toListTuple (MkWithContext context content) = (content,(context,()));
	};

	contentFixedLens :: FixedLens (WithContext context content) content;
	contentFixedLens = firstTupleFixedLens;

	contextFixedLens :: FixedLens (WithContext context content) context;
	contextFixedLens = secondTupleFixedLens;
}

