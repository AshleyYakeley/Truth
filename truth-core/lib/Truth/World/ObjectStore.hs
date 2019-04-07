module Truth.World.ObjectStore where

import Truth.Core
import Truth.Core.Import
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

type ObjectStoreEdit name edit = FunctionEdit name (SingleObjectEdit edit)

singleObjectEditFunction :: forall edit. EditFunction (SingleObjectEdit edit) (WholeEdit (Maybe (Object edit)))
singleObjectEditFunction =
    MkCloseUnlift identityUnlift $ let
        efGet :: ReadFunctionT IdentityT (SingleObjectReader edit) (WholeReader (Maybe (Object edit)))
        efGet mr ReadWhole = lift $ mr ReadSingleObjectStore
        efUpdate ::
               forall m. MonadIO m
            => SingleObjectEdit edit
            -> MutableRead m (SingleObjectReader edit)
            -> IdentityT m [WholeEdit (Maybe (Object edit))]
        efUpdate SingleObjectDelete _ = return [MkWholeEdit Nothing]
        efUpdate _ mr = do
            mo <- lift $ mr ReadSingleObjectStore
            return [MkWholeEdit mo]
        in MkAnEditFunction {..}

directoryObjectStore :: forall name. Object FSEdit -> (name -> String) -> Object (ObjectStoreEdit name ByteStringEdit)
directoryObjectStore (MkObject (objRun :: UnliftIO m) rd push) nameStr = let
    undoName :: String -> Int -> FilePath
    undoName name i = "undo/" ++ name ++ show i
    findUndoCode :: String -> Int -> m Int
    findUndoCode name i = do
        mitem <- rd $ FSReadItem $ undoName name i
        case mitem of
            Nothing -> return i
            Just _ -> findUndoCode name $ i + 1
    objRead :: MutableRead m (EditReader (ObjectStoreEdit name ByteStringEdit))
    objRead (MkTupleEditReader (MkFunctionSelector (nameStr -> name)) edit) =
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
    objEdit :: [ObjectStoreEdit name ByteStringEdit] -> m (Maybe (EditSource -> m ()))
    objEdit edits =
        return $
        Just $ \esrc ->
            case lastM edits of
                Nothing -> return ()
                Just (MkTupleEdit (MkFunctionSelector (nameStr -> name)) edit) ->
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
    in MkObject {..}
