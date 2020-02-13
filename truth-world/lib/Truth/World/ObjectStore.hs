module Truth.World.ObjectStore where

import Shapes
import Truth.Core
import Truth.World.FileSystem

data SingleObjectReader edit t where
    ReadSingleObjectStore :: SingleObjectReader edit (Maybe (Object edit))
    GetSingleObjectRecoveryCode :: SingleObjectReader edit (Maybe Int)

nullSingleObjectMutableRead :: Monad m => MutableRead m (SingleObjectReader edit)
nullSingleObjectMutableRead ReadSingleObjectStore = return Nothing
nullSingleObjectMutableRead GetSingleObjectRecoveryCode = return Nothing

data SingleObjectEdit (edit :: Type)
    = SingleObjectDeleteCreate
    | SingleObjectDelete
    | SingleObjectRecover Int

type instance EditReader (SingleObjectEdit edit) =
     SingleObjectReader edit

instance InvertibleEdit (SingleObjectEdit edit) where
    invertEdits edits mr =
        case lastM edits of
            Nothing -> return []
            Just SingleObjectDeleteCreate -> do
                mcode <- mr GetSingleObjectRecoveryCode
                return $
                    case mcode of
                        Nothing -> [SingleObjectDelete]
                        Just code -> [SingleObjectRecover code]
            Just SingleObjectDelete -> do
                mcode <- mr GetSingleObjectRecoveryCode
                return $
                    case mcode of
                        Nothing -> []
                        Just code -> [SingleObjectRecover code]
            Just (SingleObjectRecover newcode) -> do
                mcode <- mr GetSingleObjectRecoveryCode
                return $
                    case mcode of
                        Nothing -> [SingleObjectDelete]
                        Just code
                            | code == newcode -> []
                        Just code -> [SingleObjectRecover code]

type SingleObjectUpdate edit = EditUpdate (SingleObjectEdit edit)

type ObjectStoreUpdate name edit = FunctionUpdate name (SingleObjectUpdate edit)

singleObjectUpdateFunction :: forall edit. EditLens (SingleObjectUpdate edit) (ROWUpdate (Maybe (Object edit)))
singleObjectUpdateFunction = let
    elGet :: ReadFunction (SingleObjectReader edit) (WholeReader (Maybe (Object edit)))
    elGet mr ReadWhole = mr ReadSingleObjectStore
    elUpdate ::
           forall m. MonadIO m
        => SingleObjectUpdate edit
        -> MutableRead m (SingleObjectReader edit)
        -> m [ROWUpdate (Maybe (Object edit))]
    elUpdate (MkEditUpdate SingleObjectDelete) _ = return [MkReadOnlyUpdate $ MkWholeReaderUpdate Nothing]
    elUpdate _ mr = do
        mo <- mr ReadSingleObjectStore
        return [MkReadOnlyUpdate $ MkWholeReaderUpdate mo]
    in MkEditLens {elPutEdits = elPutEditsNone, ..}

directoryObjectStore ::
       forall name. Object FSEdit -> (name -> String) -> Object (UpdateEdit (ObjectStoreUpdate name ByteStringEdit))
directoryObjectStore (MkResource (rr :: ResourceRunner tt) (MkAnObject rd push)) nameStr =
    runResourceRunnerWith rr $ \_ -> let
        undoName :: String -> Int -> FilePath
        undoName name i = "undo/" ++ name ++ show i
        findUndoCode :: String -> Int -> ApplyStack tt IO Int
        findUndoCode name i = do
            mitem <- rd $ FSReadItem $ undoName name i
            case mitem of
                Nothing -> return i
                Just _ -> findUndoCode name $ i + 1
        objRead :: MutableRead (ApplyStack tt IO) (UpdateReader (ObjectStoreUpdate name ByteStringEdit))
        objRead (MkTupleUpdateReader (MkFunctionSelector (nameStr -> name)) edit) =
            case edit of
                ReadSingleObjectStore -> do
                    mitem <- rd $ FSReadItem name
                    return $
                        case mitem of
                            Just (FSFileItem fileobj) -> Just fileobj
                            _ -> Nothing
                GetSingleObjectRecoveryCode -> do
                    mitem <- rd $ FSReadItem name
                    case mitem of
                        Just (FSFileItem _) -> do
                            code <- findUndoCode name 0
                            return $ Just code
                        _ -> return $ Nothing
        objEdit ::
               NonEmpty (UpdateEdit (ObjectStoreUpdate name ByteStringEdit))
            -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
        objEdit edits =
            return $
            Just $ \esrc ->
                case last edits of
                    MkTupleUpdateEdit (MkFunctionSelector (nameStr -> name)) edit ->
                        case edit of
                            SingleObjectDelete -> do
                                mitem <- rd $ FSReadItem name
                                case mitem of
                                    Just (FSFileItem _) -> do
                                        code <- findUndoCode name 0
                                        pushOrFail ("couldn't rename FS item " <> show name) esrc $
                                            push $ pure $ FSEditRenameItem name (undoName name code)
                                    _ -> return ()
                            SingleObjectDeleteCreate -> do
                                mitem <- rd $ FSReadItem name
                                case mitem of
                                    Just (FSFileItem _) -> do
                                        code <- findUndoCode name 0
                                        pushOrFail ("couldn't rename FS item " <> show name) esrc $
                                            push $
                                            (FSEditRenameItem name (undoName name code)) :|
                                            [FSEditCreateFile name mempty]
                                    _ ->
                                        pushOrFail ("couldn't create FS item " <> show name) esrc $
                                        push $ pure $ FSEditCreateFile name mempty
                            SingleObjectRecover code ->
                                pushOrFail ("couldn't rename FS item " <> show name) esrc $
                                push $ pure $ FSEditRenameItem (undoName name code) name
        in MkResource rr MkAnObject {..}
