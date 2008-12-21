module TypeFunc where
{
	import Data.Witness;

	type family TF tf x;
	
	data TFIdentity;
	type instance TF TFIdentity x = x;
	
	data TFConst a;
	type instance TF (TFConst a) x = a;
	
	data TFApply (f :: * -> *);
	type instance TF (TFApply f) x = f x;

	data TFMatch;
	type instance TF TFMatch (f a) = a;

	data TFMatch1;
	type instance TF TFMatch1 (f a b) = a;
	
	data TFCompose tf1 tf2;
	type instance TF (TFCompose tf1 tf2) x = TF tf1 (TF tf2 x);
	
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
}
