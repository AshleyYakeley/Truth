module Data.Changes.View where
{
	import Data.Changes.Object;
	import Data.Changes.Edit;

	data ViewResult w a = MkViewResult
	{
		vrWidget :: w,
		vrUpdate :: Edit a -> IO ()
	};

	type View w a = a -> Push a -> IO (ViewResult w a);

	subscribeView :: View w a -> Subscribe a -> IO (Subscribe a,w,IO ());
	subscribeView view subscribe = do
	{
		(vr,sub) <- subscribe view vrUpdate;
		return (subCopy sub,vrWidget vr,subClose sub);
	};
}
