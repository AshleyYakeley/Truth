module Truth.Core.Types.Undo where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    data UndoQueue edit = MkUndoQueue
    {
        uqUndoEdits :: [NonEmpty edit],
        uqRedoEdits :: [NonEmpty edit]
    };

    data UQAction edit = UQUndo | UQRedo | UQEdit [edit];

    undoQueueLens :: forall edit. (EditReader (UQAction edit) ~ EditReader edit,Edit edit) =>
        FloatingEditLens (UndoQueue edit) edit (UQAction edit);
    undoQueueLens = let
    {
        undoQueueUpdate :: [edit] -> UndoQueue edit -> Readable (EditReader edit) (UndoQueue edit,[edit]);
        undoQueueUpdate edits uq@(MkUndoQueue uu _) = do
        {
            unedits <- invertEdits edits;
            return (case unedits of
            {
                us:usr -> MkUndoQueue ((us:|usr):uu) [];
                [] -> uq;
            },edits);
        };

        undoQueueAction :: UQAction edit -> UndoQueue edit -> Readable (EditReader edit) (Maybe (UndoQueue edit,[edit]));
        undoQueueAction (UQEdit edits) uq = do
        {
            result <- undoQueueUpdate edits uq;
            return $ Just result;
        };
        undoQueueAction UQUndo (MkUndoQueue [] _) = return Nothing;
        undoQueueAction UQUndo (MkUndoQueue (us:usr) rr) = do
        {
            reedits <- invertEdits $ toList us;
            let
            {
                uqUndoEdits = usr;
                uqRedoEdits = case reedits of
                {
                    rs:rsr -> (rs:|rsr):rr;
                    [] -> rr;
                };
            };
            return $ Just (MkUndoQueue{..},toList us);
        };
        undoQueueAction UQRedo (MkUndoQueue _ []) = return Nothing;
        undoQueueAction UQRedo (MkUndoQueue uu (rs:rsr)) = do
        {
            unedits <- invertEdits $ toList rs;
            let
            {
                uqUndoEdits = case unedits of
                {
                    us:usr -> (us:|usr):uu;
                    [] -> uu;
                };
                uqRedoEdits = rsr;
            };
            return $ Just (MkUndoQueue{..},toList rs);
        };

        floatingEditInitial :: UndoQueue edit;
        floatingEditInitial = MkUndoQueue [] [];

        floatingEditGet :: UndoQueue edit -> ReadFunction (EditReader edit) (EditReader edit);
        floatingEditGet _ reader = readable reader;

        floatingEditUpdate :: edit -> UndoQueue edit -> Readable (EditReader edit) (UndoQueue edit,[UQAction edit]);
        floatingEditUpdate edit olduq = do
        {
            (newuq,edits) <- undoQueueUpdate [edit] olduq;
            return (newuq,[UQEdit edits]);
        };

        floatingEditLensFunction :: FloatingEditFunction (UndoQueue edit) edit (UQAction edit);
        floatingEditLensFunction = MkFloatingEditFunction{..};

        floatingEditLensPutEdit :: UndoQueue edit -> UQAction edit -> Readable (EditReader edit) (Maybe (UndoQueue edit,[edit]));
        floatingEditLensPutEdit uq action = undoQueueAction action uq;
    } in MkFloatingEditLens{..};
}
