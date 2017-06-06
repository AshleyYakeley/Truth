module Truth.Core.Types.Undo where
{
    import Truth.Core.Import;
    import Truth.Core.Edit;
    import Truth.Core.Types.State;


    data UndoQueue edit = MkUndoQueue
    {
        uqUndoEdits :: [NonEmpty edit],
        uqRedoEdits :: [NonEmpty edit]
    };

    data UQOperation = UQUndo | UQRedo;
    instance StateOperation UQOperation where
    {
        type StateOperationState UQOperation = UndoQueue;

        emptyState _ = MkUndoQueue [] [];
        editState _ edits uq@(MkUndoQueue uu _) = do
        {
            unedits <- invertEdits edits;
            return $ case unedits of
            {
                us:usr -> MkUndoQueue ((us:|usr):uu) [];
                [] -> uq;
            }
        };
        stateOperation UQUndo uq@(MkUndoQueue uu rr) = case uu of
        {
            [] -> return (uq,[]);
            us:usr -> do
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
                return (MkUndoQueue{..},toList us);
            };
        };
        stateOperation UQRedo uq@(MkUndoQueue uu rr) = case rr of
        {
            [] -> return (uq,[]);
            rs:rsr -> do
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
                return (MkUndoQueue{..},toList rs);
            };
        };
    };
}
