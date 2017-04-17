module Truth.Core.Object.MutableEdit where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    data MutableEdit m edit userstate = MkMutableEdit
    {
        mutableRead :: MutableRead m (EditReader edit),
        mutableAllowed :: edit -> m Bool,
        mutableEdit :: [edit] -> m (Maybe userstate)
    };

    mutableReadEdit :: Monad m => MutableEdit m edit userstate -> Readable (EditReader edit) [edit] -> m (Maybe userstate);
    mutableReadEdit MkMutableEdit{..}  MkReadable{..} = do
    {
        edits <- unReadable mutableRead;
        mutableEdit edits;
    };

    mutableAlloweds :: Monad m => MutableEdit m edit userstate -> [edit] -> m Bool;
    mutableAlloweds _api [] = return True;
    mutableAlloweds api (e:ee) = do
    {
        allowed <- mutableAllowed api e;
        if allowed then mutableAlloweds api ee else return False;
    };

    singleMutableEdit :: Applicative m => (edit -> m (Maybe ())) -> [edit] -> m (Maybe ());
    singleMutableEdit mutableEdit' edits = fmap combine $ traverse mutableEdit' edits where
    {
        combine :: [Maybe ()] -> Maybe ();
        combine [] = return ();
        combine (m:mm) = do
        {
            () <- m;
            combine mm;
        }
    };

    instance Functor m => Functor (MutableEdit m edit) where
    {
        fmap ab (MkMutableEdit r a e) = MkMutableEdit r a $ fmap (fmap (fmap ab)) e;
    };
}
