module Truth.Core.Object.Editor where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Object.MutableEdit;
    import Truth.Core.Object.Object;
    import Truth.Core.Object.Subscription;


    data Editor (edit :: *) r = forall editor userstate. MkEditor
    {
        editorInit :: Object edit userstate -> IO editor,
        editorUpdate :: forall m. IsStateIO m => editor -> MutableRead m (EditReader edit) -> [edit] -> StateT userstate m (),
        editorDo :: editor -> IO r
    };

    instance Functor (Editor edit) where
    {
        fmap ab (MkEditor ei eu ed) = MkEditor ei eu $ \e -> fmap ab $ ed e;
    };

    instance Applicative (Editor edit) where
    {
        pure a = let
        {
            editorInit _ = return ();
            editorUpdate () _ _ = return ();
            editorDo () = return a;
        } in MkEditor{..};

        (MkEditor (ei1 :: Object edit userstate1 -> IO editor1) eu1 ed1) <*> (MkEditor (ei2 :: Object edit userstate2 -> IO editor2) eu2 ed2) = let
        {
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
            editorDo (e1,e2) = do
            {
                ab <- ed1 e1;
                a <- ed2 e2;
                return $ ab a;
            };
        } in MkEditor{..};
    };

    subscribeEdit :: Subscription edit actions -> Editor edit r -> IO r;
    subscribeEdit subscribe editor = case editor of
    {
        (MkEditor initr update f) -> do
        {
            (e, close, _actions) <- subscribe (error "uninitialised object (subscribeEdit)") initr update;
            finally (f e) close;
        };
    };

    oneTransactionEditor :: forall edit r. (forall m. Monad m => MutableEdit m edit () -> m r) -> Editor edit r;
    oneTransactionEditor f = let
    {
        editorInit :: Object edit () -> IO (Object edit ());
        editorInit object = return object;

        editorUpdate _lapiw _mr _edits = return ();
        editorDo (MkObject object) = object $ \muted -> lift $ f muted;
    } in MkEditor{..};

    readEditor :: FullReader (EditReader edit) => Editor edit (EditSubject edit);
    readEditor = oneTransactionEditor $ \muted -> unReadable fromReader $ mutableRead muted;

    writeEditor :: FullEdit edit => EditSubject edit -> Editor edit (Maybe ());
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
