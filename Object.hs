module Object where
{
	import System.Gnome.VFS;
	import Data.Word;
	import Distribution.PackageDescription;
	import Data.Witness;
	import Control.Compositor;

	data ValueType a where
	{
		CollectionValueType :: ListType ValueType a -> ValueType a;
		MaybeValueType :: ValueType a -> ValueType (Maybe a);
		ListValueType :: ValueType a -> ValueType [a];
		OctetValueType :: ValueType Word8;
		CharValueType :: ValueType Char;
		SourceValueType :: String -> ValueType String;
		PackageDescriptionValueType :: ValueType PackageDescription;
	};
	
	defaultListValue :: ListType ValueType a -> a;
	defaultListValue NilListType = ();
	defaultListValue (ConsListType vt rt) = (defaultValue vt,defaultListValue rt);
	
	defaultValue :: ValueType a -> a;
	defaultValue (CollectionValueType lt) = defaultListValue lt;
	defaultValue (MaybeValueType _) = Nothing;
	defaultValue (ListValueType _) = [];
	defaultValue OctetValueType = 0;
	defaultValue CharValueType = '\0';
	defaultValue (SourceValueType _) = [];
	defaultValue (PackageDescriptionValueType) = emptyPackageDescription;
	
	instance Witness ValueType where
	{
		matchWitnessF (CollectionValueType lwa) (CollectionValueType lwb) = matchWitnessF lwa lwb;
		matchWitnessF (MaybeValueType wa) (MaybeValueType wb) = fmap mapCompose (matchWitnessF wa wb);
		matchWitnessF (ListValueType wa) (ListValueType wb) = fmap mapCompose (matchWitnessF wa wb);
		matchWitnessF OctetValueType OctetValueType = Just identity;
		matchWitnessF CharValueType CharValueType = Just identity;
		matchWitnessF (SourceValueType sa) (SourceValueType sb) | sa == sb = Just identity;
		matchWitnessF PackageDescriptionValueType PackageDescriptionValueType = Just identity;
		matchWitnessF _ _ = Nothing;
	};
	
	instance Show (ValueType a) where
	{
		show (CollectionValueType _) = "collection";	-- should show types here
		show (MaybeValueType ot) = "maybe "++(show ot);
		show (ListValueType t) = "list of " ++ (show t);
		show OctetValueType = "octet";
		show CharValueType = "char";
		show (SourceValueType s) = "source code "++s;
		show PackageDescriptionValueType = "Cabal package description";
	};
	
	data Reference a = MkReference
	{
		getRef :: IO a,
		setRef :: a -> IO ()
	};
	
	data Object a = MkObject
	{
		objContext :: URI,
		objRef :: Reference a
	};
	
	type AnyV = AnyF ValueType;
	
	type AnyObject = AnyV Object;
	
	{-
	collectObjects :: URI -> [AnyObject] -> AnyObject;
	collectObjects [] = MkAnyF (CollectionValueType NilListType) ();
	collectObjects _ = undefined;
	
	oneOrCollectObjects :: [AnyObject] -> Maybe AnyObject;
	oneOrCollectObjects [] = Nothing;
	oneOrCollectObjects [obj] = Just obj;
	oneOrCollectObjects objs = Just (collectObjects objs);
	-}
}
