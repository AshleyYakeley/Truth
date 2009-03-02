module Data.Changes.Context where
{
	import Data.Changes.Tuple;
	import Data.Changes.Edit;

	data WithContext context content = MkWithContext context content;
	
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

