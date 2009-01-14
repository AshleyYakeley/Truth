module Data.TypeFunc where
{
	import Data.Witness;

	type family TF tf x;
	
	data TFIdentity;
	type instance TF TFIdentity x = x;
	
	data TFConst a;
	type instance TF (TFConst a) x = a;
	
	data TFApply (f :: * -> *);
	type instance TF (TFApply f) x = f x;
	
	data TFApply1 (f :: * -> * -> *);
	type instance TF (TFApply1 f) x = TFApply (f x);
	
	data TFApply2 (f :: * -> * -> * -> *);
	type instance TF (TFApply2 f) x = TFApply1 (f x);

	data TFLift x y;
	type instance TF (TFLift x y) z = TF (TF x z) (TF y z);
	
	data TFCompose tf1 tf2;
	type instance TF (TFCompose tf1 tf2) x = TF tf1 (TF tf2 x);
	
	data TFConverse tf1 tf2;
	type instance TF (TFConverse tf1 tf2) x = TF (TF tf1 x) tf2;
	
	type TFI = TFIdentity;
	type TFK = TFApply TFConst;
	type TFS = TFApply1 TFLift;
	type TFB = TFApply1 TFCompose;
	type TFC = TFApply1 TFConverse;

	data TFMatch;
	type instance TF TFMatch (f a) = a;

	data TFMatch1;
	type instance TF TFMatch1 (f a b) = a;
	
	data TFMap tf;
	type instance TF (TFMap tf) (f a) = f (TF tf a);
	
	data TFWitness w x y where
	{
		MkTFWitness :: w tf -> TFWitness w x (TF tf x);
	};
	
	instance (SimpleWitness w) => SimpleWitness (TFWitness w x) where
	{
		matchWitness (MkTFWitness wtf1) (MkTFWitness wtf2) = do
		{
			MkEqualType <- matchWitness wtf1 wtf2;
			return MkEqualType;
		};
	};

	matchTWitness :: (SimpleWitness w) => Type p -> TFWitness w p a -> TFWitness w p b -> Maybe (EqualType a b);
	matchTWitness _ = matchWitness;
	
	data TFComposite w tf where
	{
		ConstTFComposite :: w tf -> TFComposite w tf;
		ApplyTFComposite :: TFComposite w tf1 -> TFComposite w tf2 -> TFComposite w (TF tf1 tf2);
	};
	
	instance (SimpleWitness w) => SimpleWitness (TFComposite w) where
	{
		matchWitness (ConstTFComposite w1) (ConstTFComposite w2) = matchWitness w1 w2;
		matchWitness (ApplyTFComposite a1 a2) (ApplyTFComposite b1 b2) = do
		{
			MkEqualType <- matchWitness a1 b1;
			MkEqualType <- matchWitness a2 b2;
			return MkEqualType;
		};
		matchWitness _ _ = Nothing;
	};
}
