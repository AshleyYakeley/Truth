module Interpret where
{
	import Object;
	import Distribution.PackageDescription;
	import Data.Char;
	import Data.Word;
	
	class Compositor comp where
	{
		identity :: comp a a;
		compose :: comp b c -> comp a b -> comp a c;
	};
	
	data Codec a b = MkCodec
	{
		decode :: a -> Maybe b,
		encode :: b -> a
	};
	
	instance Compositor Codec where
	{
		identity = MkCodec Just id;
		compose (MkCodec bmc cb) (MkCodec amb ba) = MkCodec (\a -> (amb a) >>= bmc) (ba . cb);
	};
	
	latin1 :: Codec [Word8] String;
	latin1 = MkCodec (Just . (fmap (chr . fromIntegral))) (fmap (fromIntegral . ord));
	
	packageDescriptionCodec :: Codec String PackageDescription;
	packageDescriptionCodec = MkCodec
		(\str -> case parseDescription str of
		{
			ParseOk _ pd -> Just pd;
			_ -> Nothing;
		})
		(\pd -> "");
	
	data Interpreter = forall a. MkInterpreter (ObjectType a) (Codec [Word8] a);

	data MIMEType = MkMIMEType String String [(String,String)];
	
	interpret :: MIMEType -> Interpreter;
	interpret (MkMIMEType "text" "cabal" _) = MkInterpreter PackageDescriptionObjectType (compose packageDescriptionCodec latin1);
	interpret (MkMIMEType "text" _ _) = MkInterpreter TextObjectType latin1;
	interpret _ = MkInterpreter BytesObjectType identity;
}
