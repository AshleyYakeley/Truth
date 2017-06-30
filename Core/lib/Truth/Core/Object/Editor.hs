module Truth.Core.Object.Editor where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Object.MutableEdit;
    import Truth.Core.Object.Object;
    import Truth.Core.Object.Subscription;


    data Editor (edit :: *) actions r = forall editor userstate. MkEditor
    {
        editorFirst :: userstate,
        editorInit :: Object edit userstate -> IO editor,
        editorUpdate :: forall m. IsStateIO m => editor -> MutableRead m (EditReader edit) -> [edit] -> StateT userstate m (),
        editorDo :: editor -> actions -> IO r
    };

    instance Functor (Editor edit actions) where
    {
        fmap ab (MkEditor ef ei eu ed) = MkEditor ef ei eu $ \e a -> fmap ab $ ed e a;
    };

    instance Applicative (Editor edit actions) where
    {
        pure a = let
        {
            editorFirst = ();
            editorInit _ = return ();
            editorUpdate () _ _ = return ();
            editorDo () _ = return a;
        } in MkEditor{..};

        (MkEditor ef1 (ei1 :: Object edit userstate1 -> IO editor1) eu1 ed1) <*> (MkEditor ef2 (ei2 :: Object edit userstate2 -> IO editor2) eu2 ed2) = let
        {
            editorFirst :: (userstate1,userstate2);
            editorFirst = (ef1,ef2);

            editorInit :: Object edit (userstate1,userstate2) -> IO (editor1,editor2);
            editorInit object = do
            {
                e1 <- ei1 $ lensObject fstLens object;
                e2 <- ei2 $ lensObject sndLens object;
                return (e1,e2);
            };
            editorUpdate :: forall m. IsStateIO m => (editor1,editor2) -> MutableRead m (EditReader edit) -> [edit] -> StateT (userstate1,userstate2) m ();
            editorUpdate (e1,e2) mr edits = do
            {
                stateFst $ eu1 e1 mr edits;
                stateSnd $ eu2 e2 mr edits;
            };
            editorDo (e1,e2) actions = do
            {
                ab <- ed1 e1 actions;
                a <- ed2 e2 actions;
                return $ ab a;
            };
        } in MkEditor{..};
    };

    subscribeEdit :: Subscription edit actions -> Editor edit actions r -> IO r;
    subscribeEdit subscribe editor = case editor of
    {
        (MkEditor firstSt initr update f) -> do
        {
            (e, close, actions) <- subscribe firstSt initr update;
            finally (f e actions) close;
        };
    };

    oneTransactionEditor :: forall actions edit r. (forall m. Monad m => MutableEdit m edit () -> m r) -> Editor edit actions r;
    oneTransactionEditor f = let
    {
        editorFirst = ();

        editorInit :: Object edit () -> IO (Object edit ());
        editorInit object = return object;

        editorUpdate _lapiw _mr _edits = return ();
        editorDo (MkObject object) _ = object $ \muted -> lift $ f muted;
    } in MkEditor{..};

    readEditor :: FullReader (EditReader edit) => Editor edit actions (EditSubject edit);
    readEditor = oneTransactionEditor $ \muted -> unReadable fromReader $ mutableRead muted;

    writeEditor :: FullEdit edit => EditSubject edit -> Editor edit actions (Maybe ());
    writeEditor subj = oneTransactionEditor $ \muted -> do
    {
        maction <- mutableEdit muted $ getReplaceEdits subj;
        case maction of
        {
            Just action -> do
            {
                action;
                return $ Just ();
            };
            Nothing -> return Nothing;
        }
    };
}
