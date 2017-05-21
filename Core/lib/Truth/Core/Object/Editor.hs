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
        editorInit :: Object edit userstate -> IO (editor,userstate),
        editorUpdate :: forall m. MonadIOInvert m => editor -> MutableRead m (EditReader edit) -> userstate -> [edit] -> m userstate,
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
            editorInit _ = return ((),());
            editorUpdate () _ () _ = return ();
            editorDo () = return a;
        } in MkEditor{..};

        (MkEditor (ei1 :: Object edit userstate1 -> IO (editor1,userstate1)) eu1 ed1) <*> (MkEditor (ei2 :: Object edit userstate2 -> IO (editor2,userstate2)) eu2 ed2) = let
        {
            editorInit :: Object edit (userstate1,userstate2) -> IO ((editor1,editor2),(userstate1,userstate2));
            editorInit lapi = do
            {
                (e1,t1) <- ei1 $ fmap fst lapi;
                (e2,t2) <- ei2 $ fmap snd lapi;
                return ((e1,e2),(t1,t2));
            };
            editorUpdate :: forall m. MonadIOInvert m => (editor1,editor2) -> MutableRead m (EditReader edit) -> (userstate1,userstate2) -> [edit] -> m (userstate1,userstate2);
            editorUpdate (e1,e2) mr (old1,old2) edit = do
            {
                new1 <- eu1 e1 mr old1 edit;
                new2 <- eu2 e2 mr old2 edit;
                return (new1,new2);
            };
            editorDo (e1,e2) = do
            {
                ab <- ed1 e1;
                a <- ed2 e2;
                return $ ab a;
            };
        } in MkEditor{..};
    };

    subscribeEdit :: Subscription edit -> Editor edit r -> IO r;
    subscribeEdit subscribe editor = case editor of
    {
        (MkEditor initr update f) -> do
        {
            (e, close) <- subscribe initr update;
            finally (f e) close;
        };
    };

    oneTransactionEditor :: forall edit r. (forall m. Monad m => MutableEdit m edit () -> m r) -> Editor edit r;
    oneTransactionEditor f = let
    {
        editorInit :: Object edit () -> IO (Object edit (),());
        editorInit lapi = return (lapi,());

        editorUpdate _lapiw _mr () _edit = return ();
        editorDo (MkObject lapi) = lapi $ \() -> f;
    } in MkEditor{..};

    readEditor :: FullReader (EditReader edit) => Editor edit (EditSubject edit);
    readEditor = oneTransactionEditor $ \api -> unReadable fromReader $ mutableRead api;

    writeEditor :: FullEdit edit => EditSubject edit -> Editor edit (Maybe ());
    writeEditor subj = oneTransactionEditor $ \api -> do
    {
        maction <- mutableEdit api $ getReplaceEdits subj;
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
