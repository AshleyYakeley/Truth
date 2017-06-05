module Truth.Core.Types.Undo where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Context;


    -- data WithContext context content = MkWithContext context content;

    data StateReader context reader t where
    {
        SRContext :: StateReader context reader context;
        SRRead :: reader t -> StateReader context reader t;
    };

    instance Reader reader => Reader (StateReader context reader) where
    {
        type ReaderSubject (StateReader context reader) = WithContext context (ReaderSubject reader);

        readFrom (MkWithContext context _content) SRContext = context;
        readFrom (MkWithContext _context content) (SRRead reader) = readFrom content reader;
    };

    stateContentRF :: ReadFunction (StateReader context reader) reader;
    stateContentRF = readable . SRRead;


    data UndoQueue edit = MkUndoQueue
    {
        uqUndoEdits :: [NonEmpty edit],
        uqRedoEdits :: [NonEmpty edit]
    };

    undoQueueAddUndo :: [edit] -> UndoQueue edit -> UndoQueue edit;
    undoQueueAddUndo [] uq = uq;
    undoQueueAddUndo (e:ee) (MkUndoQueue ue _) = MkUndoQueue ((e:|ee):ue) [];

    type UndoReader edit = StateReader (UndoQueue edit) (EditReader edit);

    type UndoBuffer edit = WithContext (UndoQueue edit) (EditSubject edit);

    data UndoEdit edit = UEUndo | UERedo | UEEdit edit;

    instance Floating edit edit => Floating (UndoEdit edit) (UndoEdit edit) where
    {
        floatingUpdate (UEEdit edit1) (UEEdit edit2) = UEEdit $ floatingUpdate edit1 edit2;
        floatingUpdate _ edit = edit;
    };

    instance Edit edit => Edit (UndoEdit edit) where
    {
        type EditReader (UndoEdit edit) = UndoReader edit;

        applyEdit (UEEdit edit) (SRRead reader) = mapReadable stateContentRF $ applyEdit edit reader;
        applyEdit (UEEdit edit) SRContext = do
        {
            uq <- readable SRContext;
            unedits <- mapReadable stateContentRF $ invertEdit edit;
            return $ undoQueueAddUndo unedits uq;
        };
        applyEdit UEUndo (SRRead reader) = do
        {
            MkUndoQueue ee _ <- readable SRContext;
            mapReadable stateContentRF $ case ee of
            {
                es:_ -> applyEdits (toList es) reader;
                [] -> readable reader;
            };
        };
        applyEdit UEUndo SRContext = do
        {
            uq@(MkUndoQueue uu rr) <- readable SRContext;
            case uu of
            {
                us:usr -> do
                {
                    reedits <- mapReadable stateContentRF $ invertEdits $ toList us;
                    let
                    {
                        uqUndoEdits = usr;
                        uqRedoEdits = case reedits of
                        {
                            rs:rsr -> (rs:|rsr):rr;
                            [] -> rr;
                        };
                    };
                    return MkUndoQueue{..};
                };
                [] -> return uq;
            };
        };
        applyEdit UERedo (SRRead reader) = do
        {
            MkUndoQueue _ ee <- readable SRContext;
            mapReadable stateContentRF $ case ee of
            {
                es:_ -> applyEdits (toList es) reader;
                [] -> readable reader;
            };
        };
        applyEdit UERedo SRContext = do
        {
            uq@(MkUndoQueue uu rr) <- readable SRContext;
            case rr of
            {
                rs:rsr -> do
                {
                    unedits <- mapReadable stateContentRF $ invertEdits $ toList rs;
                    let
                    {
                        uqUndoEdits = case unedits of
                        {
                            us:usr -> (us:|usr):uu;
                            [] -> uu;
                        };
                        uqRedoEdits = rsr;
                    };
                    return MkUndoQueue{..};
                };
                [] -> return uq;
            };
        };

        invertEdit _ = return $ error "invert UndoEdit NYI";
    };
}
