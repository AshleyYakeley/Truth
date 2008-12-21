module Lens where
{
	import Codec;
	import Control.Category;
	import Control.Applicative;
	import Prelude hiding (id,(.));

	-- http://www.cis.upenn.edu/~bcpierce/papers/lenses-etapsslides.pdf
	data LensT clean s t = MkLensT
	{
		lensGet :: s -> t,
		lensPutback :: t -> s -> s
	};
	-- Obligatory:
	-- lensGet (lensPutback t s) = t [acceptibility]
	-- lensPutback (lensGet s) s = s [stability] 
	-- Optional:
	-- lensPutback t2 (lensPutback t1 s) = lensPutback t2 s   [forgetfulness]
	
	raiseLens :: Applicative f => LensT clean s t -> LensT clean (f s) (f t);
	raiseLens lens = MkLensT
	{
		lensGet = fmap (lensGet lens),
		lensPutback = liftA2 (lensPutback lens)
	};
	
	instance Category (LensT clean) where
	{
		id = MkLensT id (\b _ -> b);
		(MkLensT bc cbb) . (MkLensT ab baa) = MkLensT (bc . ab) (\c a -> baa (cbb c (ab a)) a);
	};
	
	type LensD = LensT ();
	--type LensC = forall clean. LensT clean;
	type LensC s t = forall clean. LensT clean s t;
	
	pairFirst :: LensC (a,b) a;
	pairFirst = MkLensT fst (\a (_,b) -> (a,b));
	
	pairSecond :: LensC (a,b) b;
	pairSecond = MkLensT snd (\b (a,_) -> (a,b));

	firstLens :: LensC (a,r) a;
	firstLens = pairFirst;
	
	consLens :: LensT c r x -> LensT c (a,r) x;
	consLens lens = lens . pairSecond;

	--	objContext :: URI,
	--	subscribe :: IO (a,IO (Edit a),Edit a -> IO (Maybe (Edit a)),IO ())
	
	{-
	lensMap :: Lens a b -> Object a -> Object b;
	lensMap lens obj = MkObject
	{
		objContext = objContext obj,
		subscribe = do
		{
			(current,pull,push,close) <- subscribe obj;
			
		};
	};
	-}


	data Flens s t = MkFlens
	{
		flensGet :: s -> t,
		flensPutback :: t -> s -> Maybe s
	};
	
	instance Category Flens where
	{
		id = MkFlens id (\b _ -> Just b);
		(MkFlens bc cbmb) . (MkFlens ab bama) = MkFlens (bc . ab) (\c a -> do
		{
			b <- cbmb c (ab a);
			bama b a;
		});
	};
	
	codecFlens :: Codec b d -> Flens b (Maybe d);
	codecFlens codec = MkFlens
	{
		flensGet = decode codec,
		flensPutback = \md _ -> fmap (encode codec) md
	};
	
	-- This is not the "arr" of an Arrow, because it wouldn't satisfy "arr id = id"
	readOnlyFlens :: (a -> b) -> Flens a b;
	readOnlyFlens ab = MkFlens
	{
		flensGet = ab,
		flensPutback = \_ _ -> Nothing
	};
	
	lensFlens :: LensT clean a b -> Flens a b;
	lensFlens lens = MkFlens
	{
		flensGet = lensGet lens,
		flensPutback = \b a -> Just (lensPutback lens b a)
	};
	-- Obligatory:
	-- fmap flensGet (flensPutback t s) = Just t or Nothing [acceptibility]
	-- flensPutback (flensGet s) s = Just s or Nothing [stability] 
	-- Optional:
	-- flensPutback t2 >>= (flensPutback t1 s) = flensPutback t2 s   [forgetfulness]
	
	
	{-
	
	flength :: Flens [a] Int;
	flength = MkFlens get putback where
	{
		get = length;
		putback 0 = Just (\_ -> []);
		putback _ = Nothing;
	};
	
	-- flensPutback t
	
	s -> s -- OK (Lens)
	Maybe s -- OK (Codec)
	Maybe (s -> s) -- bad
	s -> Maybe s -- OK
	-}
}
