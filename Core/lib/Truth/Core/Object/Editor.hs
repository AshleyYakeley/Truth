module Truth.Core.Object.Editor where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Object.API;
    import Truth.Core.Object.Object;


    data Editor (edit :: *) r = forall editor userstate. MkEditor
    {
        editorInit :: LockAPI edit userstate -> IO (editor,userstate),
        editorUpdate :: editor -> userstate -> [edit] -> IO userstate,
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
            editorUpdate () () _ = return ();
            editorDo () = return a;
        } in MkEditor{..};

        (MkEditor (ei1 :: LockAPI edit token1 -> IO (editor1,token1)) eu1 ed1) <*> (MkEditor (ei2 :: LockAPI edit token2 -> IO (editor2,token2)) eu2 ed2) = let
        {
            editorInit :: LockAPI edit (token1,token2) -> IO ((editor1,editor2),(token1,token2));
            editorInit lapi = do
            {
                (e1,t1) <- ei1 $ fmap fst lapi;
                (e2,t2) <- ei2 $ fmap snd lapi;
                return ((e1,e2),(t1,t2));
            };
            editorUpdate (e1,e2) (old1,old2) edit = do
            {
                new1 <- eu1 e1 old1 edit;
                new2 <- eu2 e2 old2 edit;
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

    subscribeEdit :: Object edit -> Editor edit r -> IO r;
    subscribeEdit subscribe editor = case editor of
    {
        (MkEditor initr update f) -> do
        {
            (e, close) <- subscribe initr update;
            finally (f e) close;
        };
    };

    oneTransactionEditor :: forall edit r. (forall m. Monad m => API m edit () -> m r) -> Editor edit r;
    oneTransactionEditor f = let
    {
        editorInit :: LockAPI edit () -> IO (LockAPI edit (),());
        editorInit lapi = return (lapi,());

        editorUpdate _lapiw () _edit = return ();
        editorDo (MkLockAPI lapi) = lapi $ \() -> f;
    } in MkEditor{..};

    readEditor :: FullReader (EditReader edit) => Editor edit (EditSubject edit);
    readEditor = oneTransactionEditor $ \api -> unReadable fromReader $ apiRead api;

    writeEditor :: FullEdit edit => EditSubject edit -> Editor edit (Maybe ());
    writeEditor subj = oneTransactionEditor $ \api -> apiEdit api $ getReplaceEdits subj;
}
