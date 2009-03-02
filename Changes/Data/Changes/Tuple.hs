module Data.Changes.Tuple where
{
	import Data.Changes.Edit;
	import Data.Witness;
	import Data.OpenWitness;
	import Data.TypeFunc;
	import Control.Monad;
	import Control.Category;
	import Prelude hiding (id,(.));

	headFixedLens :: forall head tail. FixedLens (head,tail) head;
	headFixedLens = MkFixedLens
	{
		fixedLensWitness = let
		{
			wit :: IOWitness TFMatch1;
			wit = unsafeIOWitnessFromString "Data.Changes.Tuple.headSimpleLens";
		} in makeLensWitness wit,
		fixedLensUpdate = \a edit -> let
		{
			matchEdit :: Edit (head,tail) -> Maybe (Maybe (Edit head));
			matchEdit (StateLensEdit lens _ editb) = do
			{
				MkEqualType <- matchWitness (lensWitness lens) (fixedLensWitness headFixedLens);
				return (Just editb);
			} `mplus` do
			{
				MkEqualType <- matchWitness (lensWitness lens) (fixedLensWitness tailFixedLens);
				return Nothing;
			}; 
			matchEdit _ = Nothing;
		} in case matchEdit edit of
		{
			Just mnewedit -> mnewedit;
			_ -> Just (ReplaceEdit (fst (applyEdit edit a)));
		},
		fixedLensGet = fst,
		fixedLensPutback = \b a -> Just (b,snd a)
	};

	tailFixedLens :: FixedLens (head,tail) tail;
	tailFixedLens = MkFixedLens
	{
		fixedLensWitness = let
		{
			wit :: IOWitness TFMatch;
			wit = unsafeIOWitnessFromString "Data.Changes.Tuple.tailSimpleLens";
		} in makeLensWitness wit,
		fixedLensUpdate = \a edit -> let
		{
			matchEdit :: Edit (head,tail) -> Maybe (Maybe (Edit tail));
			matchEdit (StateLensEdit lens _ editb) = do
			{
				MkEqualType <- matchWitness (lensWitness lens) (fixedLensWitness tailFixedLens);
				return (Just editb);
			} `mplus` do
			{
				MkEqualType <- matchWitness (lensWitness lens) (fixedLensWitness headFixedLens);
				return Nothing;
			}; 
			matchEdit _ = Nothing;
		} in case matchEdit edit of
		{
			Just mnewedit -> mnewedit;
			_ -> Just (ReplaceEdit (snd (applyEdit edit a)));
		},
		fixedLensGet = snd,
		fixedLensPutback = \b a -> Just (fst a,b)
	};

	class (Is (ListType Type) (Tuple a)) => IsTuple a where
	{
		type Tuple a;
		fromListTuple :: Tuple a -> a;
		toListTuple :: a -> Tuple a;
	};

	data TFTuple;
	type instance TF TFTuple a = Tuple a;

	tupleWholeWitness :: IsTuple a => WholeLens a (Tuple a);
	tupleWholeWitness = MkWholeLens
	{
		wholeLensWitness = let
		{
			wit :: IOWitness TFTuple;
			wit = unsafeIOWitnessFromString "Data.Changes.Tuple.tupleWholeWitness";
		} in makeLensWitness wit,
		wholeLensGet = toListTuple,
		wholeLensPutback = Just . fromListTuple
	};

	tupleFixedLens :: IsTuple a => FixedLens a (Tuple a);
	tupleFixedLens = simpleFixedLens (wholeSimpleLens tupleWholeWitness);

	firstTupleFixedLens :: (IsTuple a, (t0,rest) ~ Tuple a) => FixedLens a t0;
	firstTupleFixedLens = headFixedLens . tupleFixedLens;

	secondTupleFixedLens :: (IsTuple a, (t0,(t1,rest)) ~ Tuple a) => FixedLens a t1;
	secondTupleFixedLens = headFixedLens . tailFixedLens . tupleFixedLens;

	thirdTupleFixedLens :: (IsTuple a, (t0,(t1,(t2,rest))) ~ Tuple a) => FixedLens a t2;
	thirdTupleFixedLens = headFixedLens . tailFixedLens . tailFixedLens . tupleFixedLens;

	instance IsTuple (a,b) where
	{
		type Tuple (a,b) = (a,(b,()));
		fromListTuple (a,(b,())) = (a,b);
		toListTuple (a,b) = (a,(b,()));
	};

	instance IsTuple (a,b,c) where
	{
		type Tuple (a,b,c) = (a,(b,(c,())));
		fromListTuple (a,(b,(c,()))) = (a,b,c);
		toListTuple (a,b,c) = (a,(b,(c,())));
	};
}
