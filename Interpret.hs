module Interpret where
{
	import MIME;
	import Object;
	import Distribution.PackageDescription;
	import Data.Witness;
	import Control.Compositor;
	import Data.Traversable;
	import Data.Char;
	import Data.Word;
	import Data.ByteString;
	
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
	
	codecRef :: Codec a b -> Reference a -> Reference b;
	codecRef codec ref = MkReference
	{
		getRef = do
		{
			a <- getRef ref;
			case (decode codec a) of
			{
				Just b -> return b;
				_ -> fail "decode error";
			};
		},
		setRef = \b -> setRef ref (encode codec b)
	};
	
	codecObj :: Codec a b -> Object a -> Object b;
	codecObj codec (MkObject context ref) = MkObject context (codecRef codec ref);
	
	codecMapRef :: (Traversable f) => Codec a b -> Reference (f a) -> Reference (f b);
	codecMapRef codec ref = MkReference
	{
		getRef = do
		{
			fa <- getRef ref;
			case (sequenceA (fmap (decode codec) fa)) of
			{
				Just fb -> return fb;
				_ -> fail "decode error";
			};
		},
		setRef = \fb -> setRef ref (fmap (encode codec) fb)
	};
	
	codecMapObj :: (Traversable f) => Codec a b -> Object (f a) -> Object (f b);
	codecMapObj codec (MkObject context ref) = MkObject context (codecMapRef codec ref);
	
	byteStringCodec :: Codec ByteString [Word8];
	byteStringCodec = MkCodec (Just . unpack) pack;
	
	latin1 :: Codec [Word8] String;
	latin1 = MkCodec (Just . (fmap (chr . fromIntegral))) (fmap (fromIntegral . ord));
	
	packageDescriptionCodec :: Codec String PackageDescription;
	packageDescriptionCodec = MkCodec
		(\str -> case parseDescription str of
		{
			ParseOk _ pd -> Just pd;
			_ -> Nothing;
		})
		(\_ -> "");
	
	data Interpreter base = forall a. MkInterpreter (ValueType a) (Codec base a);
	
	interpret :: Interpreter base -> Object base -> AnyObject;
	interpret (MkInterpreter ot codec) obj = MkAnyF ot (codecObj codec obj);
	
	interpretMaybe :: Interpreter base -> Object (Maybe base) -> AnyObject;
	interpretMaybe (MkInterpreter ot codec) obj = MkAnyF (MaybeValueType ot) (codecMapObj codec obj);
	
	mimeInterpreter :: MIMEType -> Interpreter [Word8];
	mimeInterpreter (MkMIMEType "text" "cabal" _) = MkInterpreter PackageDescriptionValueType (compose packageDescriptionCodec latin1);
	mimeInterpreter (MkMIMEType "text" "plain" _) = MkInterpreter (ListValueType CharValueType) latin1;
	mimeInterpreter (MkMIMEType "text" subtype _) = MkInterpreter (SourceValueType subtype) latin1;
	mimeInterpreter _ = MkInterpreter (ListValueType OctetValueType) identity;
	
	data Lens a b = MkLens
	{
		lensGet :: a -> b,
		lensSet :: b -> a -> a
	};
	
	instance Compositor Lens where
	{
		identity = MkLens id (\b _ -> b);
		compose (MkLens bc cbb) (MkLens ab baa) = MkLens (bc . ab) (\c a -> baa (cbb c (ab a)) a);
	};
	
	pairFirst :: Lens (a,b) a;
	pairFirst = MkLens fst (\a (_,b) -> (a,b));
}
