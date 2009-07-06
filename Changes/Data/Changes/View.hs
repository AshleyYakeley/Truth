module Data.Changes.View where
{
	import Data.Changes.Object;
	import Data.Changes.Edit;

	data ViewResult w a = MkViewResult
	{
		vrWidget :: w,
		vrUpdate :: Edit a -> IO ()
	};
	
	mapViewResult :: (w1 -> w2) -> ViewResult w1 a -> ViewResult w2 a;
	mapViewResult f (MkViewResult w1 u) = MkViewResult (f w1) u;

	type View w a = a -> Push a -> IO (ViewResult w a);
	
	mapView :: (w1 -> w2) -> View w1 a -> View w2 a;
	mapView f view a push = fmap (mapViewResult f) (view a push);

	subscribeView :: View w a -> Subscribe a -> IO (Subscribe a,w,IO ());
	subscribeView view subscribe = do
	{
		(vr,sub) <- subscribe view vrUpdate;
		return (subCopy sub,vrWidget vr,subClose sub);
	};
}
