{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Changes.Tuple where
{
	import Data.Changes.FixedLens;
	import Data.Changes.Edit;
	import Data.Witness;
	import Data.ConstFunction;
	import Control.Monad.Identity;
	import Control.Category;
	import Prelude hiding (id,(.));

	class (Is (ListType Type) (TList a) {-, Editable (TList a)-}) => IsTuple a where
	{
		type TList a;
		fromListTuple :: TList a -> a;
		toListTuple :: a -> TList a;
	};

	instance IsTuple (a,b) where
	{
		type TList (a,b) = (a,(b,()));
		fromListTuple (a,(b,())) = (a,b);
		toListTuple (a,b) = (a,(b,()));
	};

	instance IsTuple (a,b,c) where
	{
		type TList (a,b,c) = (a,(b,(c,())));
		fromListTuple (a,(b,(c,()))) = (a,b,c);
		toListTuple (a,b,c) = (a,(b,(c,())));
	};

	data TListPartEdit t where
	{
		TListPartEdit :: (Editable a) => ListElementType t a -> Edit a -> TListPartEdit t;
	};

	applyTuplePartEdit :: TListPartEdit t -> t -> t;
	applyTuplePartEdit (TListPartEdit n edit) t = putListElement n (applyConstFunction (applyEdit edit) (getListElement n t)) t;

	invertTuplePartEdit :: TListPartEdit t -> t -> Maybe (TListPartEdit t);

	invertTuplePartEdit (TListPartEdit n edit) t = do
	{
		unedit <- invertEdit edit (getListElement n t);
		return (TListPartEdit n unedit);
	};

	instance (IsTuple t,tl ~ TList t,PartEdit t ~ TListPartEdit tl) => EditScheme (TListPartEdit tl) t where
	{
		--applyPartEdit :: TListPartEdit pet (TList x) -> ConstFunction x x;
		applyPartEdit pe = FunctionConstFunction (fromListTuple . (applyTuplePartEdit pe) . toListTuple);

		--invertPartEdit :: TListPartEdit pet (TList x) -> x -> Maybe (Edit' (TListPartEdit pet (TList x)) x);	-- "Nothing" means no change
		invertPartEdit pe x = do
		{
			unpe <- invertTuplePartEdit pe (toListTuple x);
			return (PartEdit unpe);
		};
	};

	instance (Editable a,Editable b) => Editable (a,b) where
	{
		type PartEdit (a,b) = TListPartEdit (a,(b,()));
	};
	
	tupleElementCleanLens :: (IsTuple t,Editable a,PartEdit t ~ TListPartEdit (TList t)) => ListElementType (TList t) a -> CleanLens' Identity t a;
	tupleElementCleanLens n = MkCleanLens
	{
		cleanLensUpdate = \edit -> case edit of
		{
			(PartEdit (TListPartEdit n' edita)) -> do
			{
				MkEqualType <- matchWitness n n';
				return edita;
			};
			ReplaceEdit t -> Just (ReplaceEdit (getListElement n (toListTuple t)));
		},
		cleanLensGet = (getListElement n) . toListTuple,
		cleanLensPutEdit = \editb -> Identity (PartEdit (TListPartEdit n editb))
	};
}
