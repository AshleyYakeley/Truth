{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Changes.Tuple where
{
	import Data.Changes.FixedLens;
	import Data.Changes.Edit;
	import Data.Witness;
--	import Data.OpenWitness;
	import Data.ConstFunction;
--	import Data.TypeFunc;
--	import Control.Monad;
--	import Control.Applicative;
--	import Control.Arrow;
	import Control.Monad.Identity;
	import Control.Category;
	import Prelude hiding (id,(.));

--	data Zero;
--	data Succ n;
{-	
	data IntType n where
	{
		IZero :: IntType Zero;
		ISucc :: IntType n -> IntType (Succ n);
	};
-}
{-
	class HasElement n t where
	{
		type Element n t;
		getElement :: Type n -> t -> Element n t;
		putElement :: Type n -> Element n t -> t -> t;
	};

	instance HasElement Zero (a,b) where
	{
		type Element Zero (a,b) = a;
		getElement _ (a,_) = a;
		putElement _ a (_,b) = (a,b);
	};

	instance (HasElement n b) => HasElement (Succ n) (a,b) where
	{
		type Element (Succ n) (a,b) = Element n b;
		getElement _ (_,b) = getElement (Type :: Type n) b;
		putElement _ x (a,b) = (a,putElement (Type :: Type n) x b);
	};
-}

	data TListElement t a where
	{
		HeadTListElement :: TListElement (h,r) h;
		TailTListElement :: TListElement r a -> TListElement (h,r) a;
	};

	instance SimpleWitness1 TListElement where
	{
		matchWitness1 HeadTListElement HeadTListElement = Just MkEqualType;
		matchWitness1 (TailTListElement wa) (TailTListElement wb) = matchWitness1 wa wb;
		matchWitness1 _ _ = Nothing
	};

	getTListElement :: TListElement t a -> t -> a;
	getTListElement HeadTListElement (h,_) = h;
	getTListElement (TailTListElement n) (_,r) = getTListElement n r;

	putTListElement :: TListElement t a -> a -> t -> t;
	putTListElement HeadTListElement a (_,r) = (a,r);
	putTListElement (TailTListElement n) a (h,r) = (h,putTListElement n a r);

{-
	data ListElementTF tf where
	{
		HeadListElementTF :: ListElementTF TFMatch1;
		TailListElementTF :: ListElementTF tf -> ListElementTF (TFCompose tf TFMatch);
	};

	instance SimpleWitness ListElementTF where
	{
		matchWitness HeadListElementTF HeadListElementTF = Just MkEqualType;
		matchWitness (TailListElementTF ta) (TailListElementTF tb) = do
		{
			MkEqualType <- matchWitness ta tb;
			return MkEqualType;
		};
		matchWitness _ _ = Nothing;
	};

	type ListElement = TFWitness ListElementTF;

	getListElement :: (Is (ListType Type) t) => ListElementTF tf -> t -> TF tf t;
	
	representative :: ListType Type t
	
	getListElement HeadListElementTF (h,_) = h;
	getListElement (TailListElementTF te) (_,r) = getListElement te r;

	putListElement :: ListElementTF tf -> TF tf t -> t -> t;
	putListElement HeadListElementTF a (_,r) = (a,r);
	putListElement (TailListElementTF te) a (h,r) = (h,putListElement te a r);
-}
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

{-
	data TListPartEdit pet t where
	{
		TListPartEdit :: (HasElement n t,HasElement n pet,EditScheme (Element n pet) (Element n t)) =>
			Type n -> Edit' (Element n pet) (Element n t) -> TListPartEdit pet t;
	};
-}

	data TListPartEdit t where
	{
		TListPartEdit :: (Editable a) => TListElement t a -> Edit a -> TListPartEdit t;
	};

{-
	matchTuplePartEdit :: ListElement t a -> TListPartEdit pet t -> Maybe (forall pe. (EditScheme pe h) => Edit' pe a);
	matchTuplePartEdit HeadListElement (HeadPartEdit edit) = Just edit;
	matchTuplePartEdit (TailListElement le) (TailPartEdit tpe) = matchTuplePartEdit le tpe;
	matchTuplePartEdit _ _ = Nothing;
-}
	applyTuplePartEdit :: TListPartEdit t -> t -> t;
	applyTuplePartEdit (TListPartEdit n edit) t = putTListElement n (applyConstFunction (applyEditCF edit) (getTListElement n t)) t;
	
--	applyTuplePartEdit (HeadPartEdit edit) (h,r) = (applyConstFunction (applyEditCF edit) h,r);
--	applyTuplePartEdit (TailPartEdit pe) (h,r) = (h,applyTuplePartEdit pe r);

	invertTuplePartEdit :: TListPartEdit t -> t -> Maybe (TListPartEdit t);

	invertTuplePartEdit (TListPartEdit n edit) t = do
	{
		unedit <- invertEditCF edit (getTListElement n t);
		return (TListPartEdit n unedit);
	};
{-
	invertTuplePartEdit (HeadPartEdit edit) (h,_) = do
	{
		unedit <- invertEditCF edit h;
		return (HeadPartEdit unedit);
	};
	invertTuplePartEdit (TailPartEdit pe) (_,r) = do
	{
		unpe <- invertTuplePartEdit pe r;
		return (TailPartEdit unpe);
	};
-}
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

--	nthTuplePartEdit :: 

	instance (Editable a,Editable b) => Editable (a,b) where
	{
		type PartEdit (a,b) = TListPartEdit (a,(b,()));
	};
	
	tupleElementCleanLens :: (IsTuple t,Editable a,PartEdit t ~ TListPartEdit (TList t)) => TListElement (TList t) a -> CleanLens' Identity t a;
	tupleElementCleanLens n = MkCleanLens
	{
		cleanLensWitness = undefined,
{-
		cleanLensWitness = let
		{
			wit :: IOWitness TFMatch1;
			wit = unsafeIOWitnessFromString "Data.Changes.TList.headSimpleLens";
		} in makeLensWitness wit,
-}
		--cleanLensUpdateCF :: Edit t -> Maybe (Edit a),

		cleanLensUpdateCF = \edit -> case edit of
		{
			(PartEdit (TListPartEdit n' edita)) -> do
			{
				MkEqualType <- matchWitness n n';
				return edita;
			};
			ReplaceEdit t -> Just (ReplaceEdit (getTListElement n (toListTuple t)));
		},

		cleanLensGet = (getTListElement n) . toListTuple,
		cleanLensPutEdit = \editb -> Identity (PartEdit (TListPartEdit n editb))

	};

{-
	headCleanLens :: forall head tail. CleanLens' Identity (head,tail) head;
	headCleanLens = MkCleanLens
	{
		cleanLensWitness = let
		{
			wit :: IOWitness TFMatch1;
			wit = unsafeIOWitnessFromString "Data.Changes.TList.headSimpleLens";
		} in makeLensWitness wit,
		cleanLensUpdateCF = \edit -> case edit of
		{
			(PartEdit (HeadEdit editb)) -> Just editb;
			(PartEdit (TailEdit _)) -> Nothing;
			ReplaceEdit (b,_) -> Just (ReplaceEdit b);
		},
		cleanLensGet = fst,
		cleanLensPutEdit = \editb -> Identity (PartEdit (HeadEdit editb))
	};

	tailCleanLens :: CleanLens' Identity (head,tail) tail;
	tailCleanLens = MkCleanLens
	{
		cleanLensWitness = let
		{
			wit :: IOWitness TFMatch;
			wit = unsafeIOWitnessFromString "Data.Changes.TList.tailSimpleLens";
		} in makeLensWitness wit,
		cleanLensUpdateCF = \edit -> case edit of
		{
			(PartEdit (TailEdit editb)) -> Just editb;
			(PartEdit (HeadEdit _)) -> Nothing;
			ReplaceEdit (_,b) -> Just (ReplaceEdit b);
		},
		cleanLensGet = snd,
		cleanLensPutEdit = \editb -> Identity (PartEdit (TailEdit editb))
	};
-}
{-
	applyTuplePartEdit :: forall a. (IsTuple a,Editable a) => PartEdit (TList a) -> ConstFunction a a;
	applyTuplePartEdit edit = cofmap1CF toListTuple (fmap fromListTuple (applyPartEdit edit));

	invertTuplePartEdit :: forall a. (IsTuple a,Editable a) => (PartEdit (TList a) -> PartEdit a) -> PartEdit (TList a) -> a -> Maybe (Edit a);
	invertTuplePartEdit mapPartEdit edit a = fmap mapEdit (invertPartEdit edit (toListTuple a)) where
	{
		mapEdit :: (IsTuple a) => Edit (TList a) -> Edit a;
		mapEdit (ReplaceEdit ta) = ReplaceEdit (fromListTuple ta);
		mapEdit (PartEdit pe) = PartEdit (mapPartEdit pe);
	};

	data TFTuple;
	type instance TF TFTuple a = TList a;

	tupleWholeLens :: IsTuple a => WholeLens' Identity a (TList a);
	tupleWholeLens = MkWholeLens
	{
		wholeLensWitness = let
		{
			wit :: IOWitness TFTuple;
			wit = unsafeIOWitnessFromString "Data.Changes.TList.tupleWholeWitness";
		} in makeLensWitness wit,
		wholeLensGet = toListTuple,
		wholeLensPutback = Identity . fromListTuple
	};

	tupleCleanLens :: (IsTuple a,Editable a,Editable (TList a)) => CleanLens' Identity a (TList a);
	tupleCleanLens = simpleFixedLens (wholeSimpleLens tupleWholeLens);

	firstTupleCleanLens :: (IsTuple a, (t0,rest) ~ TList a,Editable a,Editable (TList a)) => CleanLens' Identity a t0;
	firstTupleCleanLens = headCleanLens . tupleCleanLens;

	secondTupleCleanLens :: (IsTuple a, (t0,(t1,rest)) ~ TList a,Editable a,Editable (TList a)) => CleanLens' Identity a t1;
	secondTupleCleanLens = headCleanLens . tailCleanLens . tupleCleanLens;

	thirdTupleCleanLens :: (IsTuple a, (t0,(t1,(t2,rest))) ~ TList a,Editable a,Editable (TList a)) => CleanLens' Identity a t2;
	thirdTupleCleanLens = headCleanLens . tailCleanLens . tailCleanLens . tupleCleanLens;
-}
}
