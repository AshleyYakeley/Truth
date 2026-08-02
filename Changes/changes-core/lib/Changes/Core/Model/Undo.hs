module Changes.Core.Model.Undo
    ( UndoHandler
    , newUndoHandler
    , undoHandlerUndo
    , undoHandlerRedo
    , undoHandlerReference
    , undoHandlerModel
    )
where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Model.EditContext
import Changes.Core.Model.Model
import Changes.Core.Model.Reference
import Changes.Core.Resource

data RefEdits = forall edit. MkRefEdits
    { _reRef :: Reference edit
    , _reOriginalEdits :: NonEmpty edit
    , _reUndoingEdits :: NonEmpty edit
    }

type UndoEntry = NonEmpty RefEdits

data UndoQueue = MkUndoQueue
    { _uqUndoEdits :: [UndoEntry]
    , _uqRedoEdits :: [UndoEntry]
    }

data UndoHandler = MkUndoHandler
    { uhVar :: MVar UndoQueue
    , uhRunner :: ResourceRunner '[WriterT [RefEdits]]
    }

undoVarUnlift :: MVar UndoQueue -> Unlift MonadTunnelIO (WriterT [RefEdits])
undoVarUnlift var wma = do
    (a, lrefedits) <- runWriterT wma
    case nonEmpty lrefedits of
        Nothing -> return ()
        Just nrefedits ->
            mVarRunStateT var $ do
                MkUndoQueue uq _ <- get
                put $ MkUndoQueue (nrefedits : uq) []
    return a

newUndoHandler :: IO UndoHandler
newUndoHandler = do
    uhVar <- newMVar $ MkUndoQueue [] []
    uhRunner <- newResourceRunner $ undoVarUnlift uhVar
    return MkUndoHandler{..}

undoHandlerUndo :: UndoHandler -> ResourceContext -> EditSource -> IO Bool
undoHandlerUndo MkUndoHandler{..} rc esrc =
    mVarRunStateT uhVar $ do
        MkUndoQueue ues res <- get
        case ues of
            [] -> return False -- nothing to undo
            (entry : ee) -> do
                did <-
                    for entry $ \(MkRefEdits (MkResource rrP (MkAReference _readP pushP _ctaskP)) _ edits) ->
                        lift
                            $ runResourceRunner rc rrP
                            $ do
                                maction <- pushP edits
                                case maction of
                                    Just action -> do
                                        action esrc
                                        return True
                                    Nothing -> return False
                if or did
                    then do
                        put $ MkUndoQueue ee (entry : res)
                        return True
                    else return False

undoHandlerRedo :: UndoHandler -> ResourceContext -> EditSource -> IO Bool
undoHandlerRedo MkUndoHandler{..} rc esrc =
    mVarRunStateT uhVar $ do
        MkUndoQueue ues res <- get
        case res of
            [] -> return False -- nothing to redo
            (entry : ee) -> do
                did <-
                    for entry $ \(MkRefEdits (MkResource rrP (MkAReference _readP pushP _ctaskP)) edits _) ->
                        lift
                            $ runResourceRunner rc rrP
                            $ do
                                maction <- pushP edits
                                case maction of
                                    Just action -> do
                                        action esrc
                                        return True
                                    Nothing -> return False
                if or did
                    then do
                        put $ MkUndoQueue (entry : ues) ee
                        return True
                    else return False

undoHandlerAReference ::
    forall edit m.
    (InvertibleEdit edit, MonadIO m) =>
    Reference edit ->
    (WriterT [RefEdits] IO --> m) ->
    AReference edit m ->
    AReference edit m
undoHandlerAReference ref liftw (MkAReference read push ctask) = let
    push' :: NonEmpty edit -> m (Maybe (EditSource -> m ()))
    push' edits = do
        unedits <- invertEdits (toList edits) read
        maction <- push edits
        return
            $ case maction of
                Just action ->
                    Just $ \esrc -> do
                        case nonEmpty unedits of
                            Just nunedits -> liftw $ tell $ pure $ MkRefEdits ref edits nunedits
                            Nothing -> return ()
                        action esrc
                Nothing -> Nothing
    in MkAReference read push' ctask

undoHandlerReference ::
    forall edit.
    InvertibleEdit edit =>
    UndoHandler ->
    Reference edit ->
    Reference edit
undoHandlerReference MkUndoHandler{..} ref@(MkResource rr aref) =
    combineResourceRunners uhRunner rr $ \rr' liftw liftr ->
        case resourceRunnerStackUnliftDict rr of
            Dict ->
                case resourceRunnerUnliftDict rr' of
                    Dict ->
                        case resourceRunnerStackUnliftDict rr' of
                            Dict ->
                                MkResource rr'
                                    $ undoHandlerAReference
                                        ref
                                        (tlfFunction liftw (Proxy @IO))
                                    $ mapResource (tlfFunction liftr (Proxy @IO)) aref

undoHandlerModel ::
    forall update.
    InvertibleEdit (UpdateEdit update) =>
    UndoHandler ->
    Model update ->
    Model update
undoHandlerModel MkUndoHandler{..} model@(MkResource rr amodel) =
    combineResourceRunners uhRunner rr $ \rr' liftw liftr ->
        case resourceRunnerStackUnliftDict rr of
            Dict ->
                case resourceRunnerUnliftDict rr' of
                    Dict ->
                        case resourceRunnerStackUnliftDict rr' of
                            Dict ->
                                case mapResource (tlfFunction liftr (Proxy @IO)) amodel of
                                    MkAModel aref subscribe utask -> let
                                        aref' =
                                            undoHandlerAReference
                                                (modelReference model)
                                                (tlfFunction liftw (Proxy @IO))
                                                aref
                                        in MkResource rr' $ MkAModel aref' subscribe utask
