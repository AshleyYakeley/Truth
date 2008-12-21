module ValueType where
{
	import Data.Witness;
	import Data.ByteString hiding (foldr,drop,splitAt);
	import Distribution.PackageDescription;
	import Data.Word;
	import Data.List;

	data ValueType a where
	{
		CollectionValueType :: ListType ValueType a -> ValueType a;
		MaybeValueType :: ValueType a -> ValueType (Maybe a);
		ListValueType :: ValueType a -> ValueType [a];
		OctetValueType :: ValueType Word8;
		CharValueType :: ValueType Char;
		ByteStringValueType :: ValueType ByteString;
		SourceValueType :: String -> ValueType String;
		PackageDescriptionValueType :: ValueType PackageDescription;
	};

	type AnyV = AnyF ValueType;
	
	defaultListValue :: ListType ValueType a -> a;
	defaultListValue NilListType = ();
	defaultListValue (ConsListType vt rt) = (defaultValue vt,defaultListValue rt);
	
	defaultValue :: ValueType a -> a;
	defaultValue (CollectionValueType lt) = defaultListValue lt;
	defaultValue (MaybeValueType _) = Nothing;
	defaultValue (ListValueType _) = [];
	defaultValue OctetValueType = 0;
	defaultValue CharValueType = '\0';
	defaultValue ByteStringValueType = empty;
	defaultValue (SourceValueType _) = [];
	defaultValue (PackageDescriptionValueType) = emptyPackageDescription;
	
	instance SimpleWitness ValueType where
	{
		matchWitness (CollectionValueType lwa) (CollectionValueType lwb) = matchWitness lwa lwb;
		matchWitness (MaybeValueType wa) (MaybeValueType wb) = do
		{
			MkEqualType <- matchWitness wa wb;
			return MkEqualType;
		};
		matchWitness (ListValueType wa) (ListValueType wb) = do
		{
			MkEqualType <- matchWitness wa wb;
			return MkEqualType;
		};
		matchWitness OctetValueType OctetValueType = Just MkEqualType;
		matchWitness CharValueType CharValueType = Just MkEqualType;
		matchWitness ByteStringValueType ByteStringValueType = Just MkEqualType;
		matchWitness (SourceValueType sa) (SourceValueType sb) | sa == sb = Just MkEqualType;
		matchWitness PackageDescriptionValueType PackageDescriptionValueType = Just MkEqualType;
		matchWitness _ _ = Nothing;
	};
	
	instance Show (ValueType a) where
	{
		show (CollectionValueType _) = "collection";	-- should show types here
		show (MaybeValueType ot) = "maybe "++(show ot);
		show (ListValueType t) = "list of " ++ (show t);
		show OctetValueType = "octet";
		show CharValueType = "char";
		show ByteStringValueType = "octet string";
		show (SourceValueType s) = "source code "++s;
		show PackageDescriptionValueType = "Cabal package description";
	};
	
--	makeEquals :: forall a r. (forall a'. (Eq a') => ValueType a' -> r) -> ValueType a -> r;
--	makeEquals foo OctetValueType = foo OctetValueType;
	
	
	{-
	data IsEq a where
	{
		MkIsEq :: (Eq a) => IsEq a;
	};
	
	toEq :: IsEq a -> IsEq a;
	toEq MkIsEq = MkIsEq;
	
	toMaybeEq :: IsEq a -> IsEq (Maybe a);
	toMaybeEq MkIsEq = MkIsEq;
	
	valueListIsEq :: ListType ValueType a -> IsEq a;
	valueListIsEq NilListType = MkIsEq;
	valueListIsEq (ConsListType _ _) = MkIsEq;
	
	valueIsEq :: ValueType a -> IsEq a;
	valueIsEq (CollectionValueType lt) = valueListIsEq lt;
	valueIsEq (MaybeValueType t) = (\_ -> MkIsEq) (valueIsEq t);
	valueIsEq (ListValueType t) = valueIsEq t;
	valueIsEq OctetValueType = MkIsEq;
	valueIsEq CharValueType = MkIsEq;
	valueIsEq (SourceValueType _) = MkIsEq;
	valueIsEq PackageDescriptionValueType = MkIsEq;
	-}
}
