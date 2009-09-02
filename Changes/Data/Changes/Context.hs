module Data.Changes.Context where
{
	import Data.Result;
--	import Data.Changes.Tuple;
	import Data.Changes.FixedLens;
	import Data.Changes.EditScheme;
	import Data.FunctorOne;
--	import Data.Witness;
	import Data.ConstFunction;
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
{-
	instance IsTuple (WithContext context content) where
	{
		type TList (WithContext context content) = (content,(context,()));
		fromListTuple (content,(context,())) = MkWithContext context content;
		toListTuple (MkWithContext context content) = (content,(context,()));
	};

	contentCleanLens :: (CompleteEditScheme content editn) =>
	 CleanLens' Identity (WithContext context content) (TListEdit (content,(context,())) (editn,(editx,()))) content editn;
	contentCleanLens = tupleElementCleanLens HeadDoubleListElementType;

	contextCleanLens :: (CompleteEditScheme context editx) =>
	 CleanLens' Identity (WithContext context content) (TListEdit (content,(context,())) (editn,(editx,()))) context editx;
	contextCleanLens = tupleElementCleanLens (TailDoubleListElementType HeadDoubleListElementType);
-}

    data ContextContentEdit editx editn = ContextEdit editx | ContentEdit editn;

	instance (EditScheme context editx,EditScheme content editn) =>
	 EditScheme (WithContext context content) (ContextContentEdit editx editn) where
	{
        applyEdit (ContextEdit edit) = arr (\(MkWithContext x n) -> MkWithContext (applyConstFunction (applyEdit edit) x) n);
        applyEdit (ContentEdit edit) = arr (\(MkWithContext x n) -> MkWithContext x (applyConstFunction (applyEdit edit) n));
    	invertEdit (ContextEdit edit) (MkWithContext a _) = fmap ContextEdit (invertEdit edit a);
    	invertEdit (ContentEdit edit) (MkWithContext _ a) = fmap ContentEdit (invertEdit edit a);
	};

	contextCleanLens :: CleanLens' Identity (WithContext context content) (ContextContentEdit editx editn) context editx;
	contextCleanLens = MkCleanLens
	{
	    cleanLensUpdate = \ccedit -> case ccedit of
	    {
	        ContextEdit edit -> Just edit;
	        _ -> Nothing;
	    },
	    cleanLensGet = \(MkWithContext a _) -> a,
	    cleanLensPutEdit = Identity . ContextEdit
	};

	contentCleanLens :: CleanLens' Identity (WithContext context content) (ContextContentEdit editx editn) content editn;
	contentCleanLens = MkCleanLens
	{
	    cleanLensUpdate = \ccedit -> case ccedit of
	    {
	        ContentEdit edit -> Just edit;
	        _ -> Nothing;
	    },
	    cleanLensGet = \(MkWithContext _ a) -> a,
	    cleanLensPutEdit = Identity . ContentEdit
	};
}

