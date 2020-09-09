module Changes.World.ReferenceStore where

import Changes.Core
import Changes.World.FileSystem
import Shapes

data SingleReferenceReader edit t where
    ReadSingleReferenceStore :: SingleReferenceReader edit (Maybe (Reference edit))
    GetSingleReferenceRecoveryCode :: SingleReferenceReader edit (Maybe Int)

nullSingleReferenceReadable :: Monad m => Readable m (SingleReferenceReader edit)
nullSingleReferenceReadable ReadSingleReferenceStore = return Nothing
nullSingleReferenceReadable GetSingleReferenceRecoveryCode = return Nothing

data SingleReferenceEdit (edit :: Type)
    = SingleReferenceDeleteCreate
    | SingleReferenceDelete
    | SingleReferenceRecover Int

type instance EditReader (SingleReferenceEdit edit) =
     SingleReferenceReader edit

instance InvertibleEdit (SingleReferenceEdit edit) where
    invertEdits edits mr =
        case lastM edits of
            Nothing -> return []
            Just SingleReferenceDeleteCreate -> do
                mcode <- mr GetSingleReferenceRecoveryCode
                return $
                    case mcode of
                        Nothing -> [SingleReferenceDelete]
                        Just code -> [SingleReferenceRecover code]
            Just SingleReferenceDelete -> do
                mcode <- mr GetSingleReferenceRecoveryCode
                return $
                    case mcode of
                        Nothing -> []
                        Just code -> [SingleReferenceRecover code]
            Just (SingleReferenceRecover newcode) -> do
                mcode <- mr GetSingleReferenceRecoveryCode
                return $
                    case mcode of
                        Nothing -> [SingleReferenceDelete]
                        Just code
                            | code == newcode -> []
                        Just code -> [SingleReferenceRecover code]

type SingleReferenceUpdate edit = EditUpdate (SingleReferenceEdit edit)

type ReferenceStoreUpdate name edit = FunctionUpdate name (SingleReferenceUpdate edit)

singleReferenceUpdateFunction ::
       forall edit. ChangeLens (SingleReferenceUpdate edit) (ROWUpdate (Maybe (Reference edit)))
singleReferenceUpdateFunction = let
    clRead :: ReadFunction (SingleReferenceReader edit) (WholeReader (Maybe (Reference edit)))
    clRead mr ReadWhole = mr ReadSingleReferenceStore
    clUpdate ::
           forall m. MonadIO m
        => SingleReferenceUpdate edit
        -> Readable m (SingleReferenceReader edit)
        -> m [ROWUpdate (Maybe (Reference edit))]
    clUpdate (MkEditUpdate SingleReferenceDelete) _ = return [MkReadOnlyUpdate $ MkWholeReaderUpdate Nothing]
    clUpdate _ mr = do
        mo <- mr ReadSingleReferenceStore
        return [MkReadOnlyUpdate $ MkWholeReaderUpdate mo]
    in MkChangeLens {clPutEdits = clPutEditsNone, ..}

directoryReferenceStore ::
       forall name.
       Reference FSEdit
    -> (name -> String)
    -> Reference (UpdateEdit (ReferenceStoreUpdate name ByteStringEdit))
directoryReferenceStore (MkResource (rr :: ResourceRunner tt) (MkAReference rd push refCommitTask)) nameStr =
    case resourceRunnerStackUnliftDict @IO rr of
        Dict -> let
            undoName :: String -> Int -> FilePath
            undoName name i = "undo/" ++ name ++ show i
            findUndoCode :: String -> Int -> ApplyStack tt IO Int
            findUndoCode name i = do
                mitem <- rd $ FSReadItem $ undoName name i
                case mitem of
                    Nothing -> return i
                    Just _ -> findUndoCode name $ i + 1
            refRead :: Readable (ApplyStack tt IO) (UpdateReader (ReferenceStoreUpdate name ByteStringEdit))
            refRead (MkTupleUpdateReader (MkFunctionSelector (nameStr -> name)) edit) =
                case edit of
                    ReadSingleReferenceStore -> do
                        mitem <- rd $ FSReadItem name
                        return $
                            case mitem of
                                Just (FSFileItem fileobj) -> Just fileobj
                                _ -> Nothing
                    GetSingleReferenceRecoveryCode -> do
                        mitem <- rd $ FSReadItem name
                        case mitem of
                            Just (FSFileItem _) -> do
                                code <- findUndoCode name 0
                                return $ Just code
                            _ -> return $ Nothing
            refEdit ::
                   NonEmpty (UpdateEdit (ReferenceStoreUpdate name ByteStringEdit))
                -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
            refEdit edits =
                return $
                Just $ \esrc ->
                    case last edits of
                        MkTupleUpdateEdit (MkFunctionSelector (nameStr -> name)) edit ->
                            case edit of
                                SingleReferenceDelete -> do
                                    mitem <- rd $ FSReadItem name
                                    case mitem of
                                        Just (FSFileItem _) -> do
                                            code <- findUndoCode name 0
                                            pushOrFail ("couldn't rename FS item " <> show name) esrc $
                                                push $ pure $ FSEditRenameItem name (undoName name code)
                                        _ -> return ()
                                SingleReferenceDeleteCreate -> do
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
                                SingleReferenceRecover code ->
                                    pushOrFail ("couldn't rename FS item " <> show name) esrc $
                                    push $ pure $ FSEditRenameItem (undoName name code) name
            in MkResource rr MkAReference {..}
