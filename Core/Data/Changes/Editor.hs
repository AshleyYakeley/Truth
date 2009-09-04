module Data.Changes.Editor where
{
    import Data.Changes.Object;
    import Data.Changes.EditScheme;
    import Control.Exception hiding (catch);

    data Editor edit b = forall r. MkEditor
    {
        editorInit :: Subject edit -> Push edit -> IO r,
        editorUpdate :: r -> edit -> IO (),
        editorDo :: r -> Subscribe edit -> IO b
    };

    subscribeEdit :: Subscribe edit -> Editor edit b -> IO b;
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
    
    subscribeRead :: Subscribe edit -> IO (Subject edit);
    subscribeRead object = subscribeEdit object (MkEditor
    {
        editorInit = \a _ -> return a,
        editorUpdate = \_ _ -> return (),
        editorDo = \a _ -> return a
    });

    subscribeWrite :: (Edit edit) => Subject edit -> Subscribe edit -> IO (Maybe ());
    subscribeWrite a object = subscribeEdit object (MkEditor
    {
        editorInit = \_ push -> return push,
        editorUpdate = \_ _ -> return (),
        editorDo = \push _ -> push (replaceEdit a)
    });
}
