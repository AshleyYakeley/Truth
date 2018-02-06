module Soup.Edit
    ( UUID
    , UUIDElementEdit
    , SoupEdit
    , directorySoup
    , liftSoupLens
    ) where

import Data.UUID
import Shapes
import System.FilePath hiding ((<.>))
import Truth.Core
import Truth.World.FileSystem
import Truth.Debug.Object

type UUIDElementEdit edit = PairEdit (ConstEdit UUID) edit

type SoupEdit edit = KeyEdit [(UUID, EditSubject edit)] (UUIDElementEdit edit)

liftSoupLens ::
       forall edita editb. (Edit edita, FullSubjectReader (EditReader edita), FullSubjectReader (EditReader editb))
    => (forall m. MonadIO m =>
                      EditSubject editb -> m (Maybe (EditSubject edita)))
    -> EditLens edita editb
    -> EditLens (SoupEdit edita) (SoupEdit editb)
liftSoupLens bmfa = let
    conv ::
           forall m. MonadIO m
        => (UUID, EditSubject editb)
        -> m (Maybe (UUID, EditSubject edita))
    conv (uuid, b) = fmap (fmap $ \a -> (uuid, a)) $ bmfa b
    in liftKeyElementEditLens conv . sndLiftEditLens

nameToUUID :: String -> Maybe UUID
nameToUUID = Data.UUID.fromString

uuidToName :: UUID -> String
uuidToName = Data.UUID.toString

dictWorkaround ::
       forall m. MonadStackIO m
    => Dict (MonadTransUnlift (MonadStackTrans m))
dictWorkaround = Dict

directorySoup :: Object FSEdit -> FilePath -> Object (SoupEdit (MutableIOEdit ByteStringEdit))
directorySoup (MkObject (runFS :: UnliftIO m) readFS pushFS) dirpath =
    traceObject' "soupdir" $
    case hasTransConstraint @MonadUnliftIO @(MonadStackTrans m) @(AutoClose FilePath (Object ByteStringEdit)) of
        Dict -> let
            runSoup :: UnliftIO (CombineMonadIO m (AutoClose FilePath (Object ByteStringEdit)))
            runSoup = combineUnliftIOs runFS runAutoClose
            readSoup ::
                   MutableRead (CombineMonadIO m (AutoClose FilePath (Object ByteStringEdit))) (EditReader (SoupEdit (MutableIOEdit ByteStringEdit)))
            readSoup KeyReadKeys = do
                mnames <- combineLiftFst $ readFS $ FSReadDirectory dirpath
                return $
                    case mnames of
                        Just names -> mapMaybe nameToUUID $ MkFiniteSet names
                        Nothing -> mempty
            readSoup (KeyReadItem uuid (MkTupleEditReader EditFirst ReadWhole)) = do
                mitem <- combineLiftFst $ readFS $ FSReadItem $ dirpath </> uuidToName uuid
                case mitem of
                    Just (FSFileItem _) -> return $ Just uuid
                    _ -> return Nothing
            readSoup (KeyReadItem uuid (MkTupleEditReader EditSecond ReadMutableIO)) = do
                let path = dirpath </> uuidToName uuid
                mitem <- combineLiftFst $ readFS $ FSReadItem path
                case mitem of
                    Just (FSFileItem object) -> do
                        muted <- combineLiftSnd @m $ acOpenObject path $ \call -> call object -- pointless
                        return $ Just muted
                    _ -> return Nothing
            pushSoup ::
                   [SoupEdit (MutableIOEdit ByteStringEdit)]
                -> MonadStackTrans m (AutoClose FilePath (Object ByteStringEdit)) (Maybe (MonadStackTrans m (AutoClose FilePath (Object ByteStringEdit)) ()))
            pushSoup =
                singleEdit $ \edit ->
                    fmap (fmap combineLiftFst) $
                    combineLiftFst $
                    case edit of
                        KeyEditItem _uuid (MkTupleEdit EditFirst iedit) -> never iedit
                        KeyEditItem _uuid (MkTupleEdit EditSecond iedit) -> never iedit
                        KeyDeleteItem uuid -> pushFS [FSEditDeleteNonDirectory $ dirpath </> uuidToName uuid]
                        KeyInsertReplaceItem (uuid, bs) -> pushFS [FSEditCreateFile (dirpath </> uuidToName uuid) bs]
                        KeyClear -> do
                            mnames <- readFS $ FSReadDirectory dirpath
                            return $
                                case mnames of
                                    Just names ->
                                        Just $
                                        for_ names $ \name -> pushFS [FSEditDeleteNonDirectory $ dirpath </> name]
                                    Nothing -> Nothing
            in case dictWorkaround @m of
                   Dict -> MkObject runSoup readSoup pushSoup
