module Data.Changes.Editor where
{
	import Data.Changes.Object;
	import Data.Changes.Edit;
	import Control.Exception hiding (catch);

	data Editor a b = forall r. MkEditor
	{
		editorInit :: a -> Push a -> IO r,
		editorUpdate :: r -> Edit a -> IO (),
		editorDo :: r -> Subscribe a -> IO b
	};

	subscribeEdit :: Subscribe a -> Editor a b -> IO b;
	subscribeEdit subscribe editor = case editor of 
	{
		(MkEditor initr update f) -> do
		{
			(r, sub) <- subscribe initr update;
			finally
				(f r (subCopy sub))
				(subClose sub);
		};
	};
	
	subscribeRead :: Subscribe a -> IO a;
	subscribeRead object = subscribeEdit object (MkEditor
	{
		editorInit = \a _ -> return a,
		editorUpdate = \_ _ -> return (),
		editorDo = \a _ -> return a
	});

	subscribeWrite :: a -> Subscribe a -> IO (Maybe ());
	subscribeWrite a object = subscribeEdit object (MkEditor
	{
		editorInit = \_ push -> return push,
		editorUpdate = \_ _ -> return (),
		editorDo = \push _ -> push (ReplaceEdit a)
	});
}
