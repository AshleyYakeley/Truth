module Truth.World.ObjectStore where

import Truth.Core
import Truth.Core.Import
import Truth.World.FileSystem

data SingleObjectReader edit t where
    ReadSingleObjectStore :: SingleObjectReader edit (Maybe (Object edit))
    GetSingleObjectRecoveryCode :: SingleObjectReader edit (Maybe Int)

data SingleObjectEdit (edit :: *)
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

type ObjectStoreEdit name edit = TupleEdit (FunctionSelector name (SingleObjectEdit edit))

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
    objEdit :: [ObjectStoreEdit name ByteStringEdit] -> m (Maybe (m ()))
    objEdit edits =
        return $
        Just $
        case lastM edits of
            Nothing -> return ()
            Just (MkTupleEdit (MkFunctionSelector (nameStr -> name)) edit) ->
                case edit of
                    SingleObjectDelete -> do
                        mitem <- rd $ FSReadItem name
                        case mitem of
                            Just (FSFileItem _) -> do
                                code <- findUndoCode name 0
                                pushEdit $ push [FSEditRenameItem name (undoName name code)]
                            _ -> return ()
                    SingleObjectDeleteCreate -> do
                        mitem <- rd $ FSReadItem name
                        case mitem of
                            Just (FSFileItem _) -> do
                                code <- findUndoCode name 0
                                pushEdit $
                                    push [FSEditRenameItem name (undoName name code), FSEditCreateFile name mempty]
                            _ -> pushEdit $ push [FSEditCreateFile name mempty]
                    SingleObjectRecover code -> pushEdit $ push [FSEditRenameItem (undoName name code) name]
    in MkObject {..}