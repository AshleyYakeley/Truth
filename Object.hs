module Object where
{
	import Data.Word;
	import Distribution.PackageDescription;

	data ObjectType a where
	{
		BytesObjectType :: ObjectType [Word8];
		TextObjectType :: ObjectType String;
		SourceObjectType :: String -> ObjectType String;
		PackageDescriptionObjectType :: ObjectType PackageDescription;
	};
	
	data Object = forall a. MkObject (ObjectType a) a;
}
