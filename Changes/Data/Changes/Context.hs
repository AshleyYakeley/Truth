module Data.Changes.Context where
{
	import Data.Result;
	import Data.Changes.Tuple;
	import Data.Changes.FixedLens;
	import Data.Changes.Edit;
	import Data.FunctorOne;
	import Data.Traversable;
	import Data.Foldable;
	import Control.Arrow;
	import Control.Monad.Identity;

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
		retrieveOne (MkWithContext _ a) = SuccessResult a;
		getPureOne = arr (\(MkWithContext context _) content -> (MkWithContext context content));
	};
	
	instance (Editable context,Editable content) => Editable (WithContext context content) where
	{
		type PartEdit (WithContext context content) = TListPartEdit (content,(context,()));
	};
	
	instance (Editable context,Editable content) => IsTuple (WithContext context content) where
	{
		type TList (WithContext context content) = (content,(context,()));
		fromListTuple (content,(context,())) = MkWithContext context content;
		toListTuple (MkWithContext context content) = (content,(context,()));
	};

--	tupleElementCleanLens :: (IsTuple t,Editable a,PartEdit t ~ TListPartEdit (TList t)) => TListElement (TList t) a -> CleanLens' Identity t a;


	contentCleanLens :: (Editable context,Editable content) => CleanLens' Identity (WithContext context content) content;
	contentCleanLens = tupleElementCleanLens HeadTListElement;

	contextCleanLens :: (Editable context,Editable content) => CleanLens' Identity (WithContext context content) context;
	contextCleanLens = tupleElementCleanLens (TailTListElement HeadTListElement);
}

