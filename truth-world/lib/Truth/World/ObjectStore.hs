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

singleObjectUpdateFunction :: forall edit. UpdateFunction (SingleObjectUpdate edit) (WholeUpdate (Maybe (Object edit)))
singleObjectUpdateFunction =
    MkCloseUnlift wUnIdentityT $ let
        ufGet :: ReadFunctionT IdentityT (SingleObjectReader edit) (WholeReader (Maybe (Object edit)))
        ufGet mr ReadWhole = lift $ mr ReadSingleObjectStore
        ufUpdate ::
               forall m. MonadIO m
            => SingleObjectUpdate edit
            -> MutableRead m (SingleObjectReader edit)
            -> IdentityT m [WholeUpdate (Maybe (Object edit))]
        ufUpdate (MkEditUpdate SingleObjectDelete) _ = return [MkWholeReaderUpdate Nothing]
        ufUpdate _ mr = do
            mo <- lift $ mr ReadSingleObjectStore
            return [MkWholeReaderUpdate mo]
        in MkAnUpdateFunction {..}

directoryObjectStore ::
       forall name. Object FSEdit -> (name -> String) -> Object (UpdateEdit (ObjectStoreUpdate name ByteStringEdit))
directoryObjectStore (MkCloseUnliftIO (objRun :: WIOFunction m) (MkAnObject rd push)) nameStr = let
    undoName :: String -> Int -> FilePath
    undoName name i = "undo/" ++ name ++ show i
    findUndoCode :: String -> Int -> m Int
    findUndoCode name i = do
        mitem <- rd $ FSReadItem $ undoName name i
        case mitem of
            Nothing -> return i
            Just _ -> findUndoCode name $ i + 1
    objRead :: MutableRead m (UpdateReader (ObjectStoreUpdate name ByteStringEdit))
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
    objEdit :: [UpdateEdit (ObjectStoreUpdate name ByteStringEdit)] -> m (Maybe (EditSource -> m ()))
    objEdit edits =
        return $
        Just $ \esrc ->
            case lastM edits of
                Nothing -> return ()
                Just (MkTupleUpdateEdit (MkFunctionSelector (nameStr -> name)) edit) ->
                    case edit of
                        SingleObjectDelete -> do
                            mitem <- rd $ FSReadItem name
                            case mitem of
                                Just (FSFileItem _) -> do
                                    code <- findUndoCode name 0
                                    pushOrFail ("couldn't rename FS item " <> show name) esrc $
                                        push [FSEditRenameItem name (undoName name code)]
                                _ -> return ()
                        SingleObjectDeleteCreate -> do
                            mitem <- rd $ FSReadItem name
                            case mitem of
                                Just (FSFileItem _) -> do
                                    code <- findUndoCode name 0
                                    pushOrFail ("couldn't rename FS item " <> show name) esrc $
                                        push [FSEditRenameItem name (undoName name code), FSEditCreateFile name mempty]
                                _ ->
                                    pushOrFail ("couldn't create FS item " <> show name) esrc $
                                    push [FSEditCreateFile name mempty]
                        SingleObjectRecover code ->
                            pushOrFail ("couldn't rename FS item " <> show name) esrc $
                            push [FSEditRenameItem (undoName name code) name]
    in MkCloseUnliftIO objRun MkAnObject {..}
