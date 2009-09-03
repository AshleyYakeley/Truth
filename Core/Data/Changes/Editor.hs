module Data.Changes.Editor where
{
    import Data.Changes.Object;
    import Data.Changes.EditScheme;
    import Control.Exception hiding (catch);

    data Editor a edit b = forall r. MkEditor
    {
        editorInit :: a -> Push edit -> IO r,
        editorUpdate :: r -> edit -> IO (),
        editorDo :: r -> Subscribe a edit -> IO b
    };

    subscribeEdit :: Subscribe a edit -> Editor a edit b -> IO b;
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
    
    subscribeRead :: Subscribe a edit -> IO a;
    subscribeRead object = subscribeEdit object (MkEditor
    {
        editorInit = \a _ -> return a,
        editorUpdate = \_ _ -> return (),
        editorDo = \a _ -> return a
    });

    subscribeWrite :: (CompleteEditScheme a edit) => a -> Subscribe a edit -> IO (Maybe ());
    subscribeWrite a object = subscribeEdit object (MkEditor
    {
        editorInit = \_ push -> return push,
        editorUpdate = \_ _ -> return (),
        editorDo = \push _ -> push (replaceEdit a)
    });
}
