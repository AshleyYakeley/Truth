module Truth.Core.Types.State where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    data StateBuffer state val = MkStateBuffer state val;

    data StateReader state reader t where
    {
        SRContext :: StateReader state reader state;
        SRRead :: reader t -> StateReader state reader t;
    };

    instance Reader reader => Reader (StateReader state reader) where
    {
        type ReaderSubject (StateReader state reader) = StateBuffer state (ReaderSubject reader);

        readFrom (MkStateBuffer state _val) SRContext = state;
        readFrom (MkStateBuffer _state val) (SRRead reader) = readFrom val reader;
    };

    stateContentRF :: ReadFunction (StateReader state reader) reader;
    stateContentRF = readable . SRRead;


    class StateOperation stateop where
    {
        type StateOperationState stateop :: * -> *;

        emptyState :: proxy stateop -> StateOperationState stateop edit;
        editState :: Edit edit => proxy stateop -> [edit] -> StateOperationState stateop edit -> Readable (EditReader edit) (StateOperationState stateop edit);
        stateOperation :: Edit edit => stateop -> StateOperationState stateop edit -> Readable (EditReader edit) (StateOperationState stateop edit,[edit]);
    };

    data StateEdit stateop edit = EditStateEdit [edit] | StateStateEdit stateop;

    instance Floating edit edit => Floating (StateEdit stateop edit) (StateEdit stateop edit) where
    {
        floatingUpdate (EditStateEdit edit1) (EditStateEdit edit2) = EditStateEdit $ fmap (floatingUpdate edit1) edit2;
        floatingUpdate _ edit = edit;
    };

    instance (StateOperation stateop,Edit edit) => Edit (StateEdit stateop edit) where
    {
        type EditReader (StateEdit stateop edit) = StateReader (StateOperationState stateop edit) (EditReader edit);

        applyEdit (EditStateEdit edits) (SRRead reader) = mapReadable stateContentRF $ applyEdits edits reader;
        applyEdit (EditStateEdit edits) SRContext = do
        {
            oldstate <- readable SRContext;
            newstate <- mapReadable stateContentRF $ editState (Proxy::Proxy stateop) edits oldstate;
            return newstate;
        };
        applyEdit (StateStateEdit stateop) (SRRead reader) = do
        {
            oldstate <- readable SRContext;
            mapReadable stateContentRF $ do
            {
                (_,edits) <- stateOperation stateop oldstate;
                applyEdits edits reader;
            };
        };
        applyEdit (StateStateEdit stateop) SRContext = do
        {
            oldstate <- readable SRContext;
            mapReadable stateContentRF $ do
            {
                (newstate,_) <- stateOperation stateop oldstate;
                return newstate;
            };
        };

        invertEdit _ = return $ error "invert StateEdit NYI";
    };

    fromStateBufferLens :: forall m stateop edit. (StateOperation stateop,Edit edit,Applicative m) =>
        EditLens' m (StateEdit stateop edit) edit;
    fromStateBufferLens = let
    {
        editGet :: ReadFunction (StateReader (StateOperationState stateop edit) (EditReader edit)) (EditReader edit);
        editGet = stateContentRF;

        editUpdate :: StateEdit stateop edit -> Readable (StateReader (StateOperationState stateop edit) (EditReader edit)) [edit];
        editUpdate (EditStateEdit edits) = return edits;
        editUpdate (StateStateEdit stateop) = do
        {
            oldstate <- readable SRContext;
            (_newstate,edits) <- mapReadable stateContentRF $ stateOperation stateop oldstate;
            return edits;
        };

        editLensFunction :: EditFunction (StateEdit stateop edit) edit;
        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: edit -> Readable (StateReader (StateOperationState stateop edit) (EditReader edit)) (m [StateEdit stateop edit]);
        editLensPutEdit edit = return $ pure [EditStateEdit [edit]];
    } in MkEditLens{..};

    toStateBufferLens :: forall m stateop edit. (StateOperation stateop,Edit edit,Applicative m) =>
        FloatingEditLens' m (StateOperationState stateop edit) edit (StateEdit stateop edit);
    toStateBufferLens = let
    {
        floatingEditInitial :: StateOperationState stateop edit;
        floatingEditInitial = emptyState (Proxy::Proxy stateop);

        floatingEditGet :: StateOperationState stateop edit -> ReadFunction (EditReader edit) (StateReader (StateOperationState stateop edit) (EditReader edit));
        floatingEditGet state SRContext = return state;
        floatingEditGet _state (SRRead reader) = readable reader;

        floatingEditUpdate :: edit -> StateOperationState stateop edit -> Readable (EditReader edit) (StateOperationState stateop edit,[StateEdit stateop edit]);
        floatingEditUpdate edit oldstate = do
        {
            newstate <- editState (Proxy::Proxy stateop) [edit] oldstate;
            return (newstate,[EditStateEdit [edit]]);
        };

        floatingEditLensFunction :: FloatingEditFunction (StateOperationState stateop edit) edit (StateEdit stateop edit);
        floatingEditLensFunction = MkFloatingEditFunction{..};

        floatingEditLensPutEdit :: StateOperationState stateop edit -> StateEdit stateop edit -> Readable (EditReader edit) (m (StateOperationState stateop edit,[edit]));
        floatingEditLensPutEdit oldstate (EditStateEdit edits) = do
        {
            newstate <- editState (Proxy::Proxy stateop) edits oldstate;
            return $ pure (newstate,edits);
        };
        floatingEditLensPutEdit oldstate (StateStateEdit stateop) = do
        {
            result <- stateOperation stateop oldstate;
            return $ pure result;
        };
    } in MkFloatingEditLens{..};

}

