module Soup.Edit
    ( UUID
    , SoupUpdate
    , ObjectSoupUpdate
    , directorySoup
    , liftSoupLens
    ) where

import Data.UUID
import Shapes
import System.FilePath hiding ((<.>))
import Truth.Core
import Truth.World.FileSystem

type UUIDElementUpdate update = PairUpdate (ConstUpdate UUID) update

type SoupUpdate update = KeyUpdate [(UUID, UpdateSubject update)] (UUIDElementUpdate update)

liftSoupLens ::
       forall updateA updateB.
       ( ApplicableEdit (UpdateEdit updateA)
       , FullSubjectReader (UpdateReader updateA)
       , FullSubjectReader (UpdateReader updateB)
       )
    => (forall m. MonadIO m => UpdateSubject updateB -> m (Maybe (UpdateSubject updateA)))
    -> EditLens updateA updateB
    -> EditLens (SoupUpdate updateA) (SoupUpdate updateB)
liftSoupLens bmfa = let
    conv ::
           forall m. MonadIO m
        => (UUID, UpdateSubject updateB)
        -> m (Maybe (UUID, UpdateSubject updateA))
    conv (uuid, b) = fmap (fmap $ \a -> (uuid, a)) $ bmfa b
    in liftKeyElementEditLens conv . sndLiftEditLens

nameToUUID :: String -> Maybe UUID
nameToUUID = Data.UUID.fromString

uuidToName :: UUID -> String
uuidToName = Data.UUID.toString

dictWorkaround ::
       forall m. MonadStackIO m
    => Dict (MonadTransUntrans (CombineMonadIO m))
dictWorkaround = Dict

type ObjectSoupUpdate = SoupUpdate (ObjectUpdate ByteStringUpdate)

directorySoup :: Object FSEdit -> FilePath -> Object (UpdateEdit ObjectSoupUpdate)
directorySoup (MkRunnableIO (runFS :: IOFunction m) (MkAnObject readFS pushFS)) dirpath =
    case hasTransConstraint @MonadUnliftIO @(CombineMonadIO m) @(AutoClose FilePath (Object ByteStringEdit)) of
        Dict -> let
            runSoup :: IOFunction (CombineMonadIO m (AutoClose FilePath (Object ByteStringEdit)))
            runSoup = combineIOFunctions runFS runAutoClose
            readSoup ::
                   MutableRead (CombineMonadIO m (AutoClose FilePath (Object ByteStringEdit))) (UpdateReader ObjectSoupUpdate)
            readSoup KeyReadKeys = do
                mnames <- combineFstMFunction $ readFS $ FSReadDirectory dirpath
                return $
                    case mnames of
                        Just names -> mapMaybe nameToUUID $ MkFiniteSet names
                        Nothing -> mempty
            readSoup (KeyReadItem uuid (MkTupleUpdateReader SelectFirst ReadWhole)) = do
                mitem <- combineFstMFunction $ readFS $ FSReadItem $ dirpath </> uuidToName uuid
                case mitem of
                    Just (FSFileItem _) -> return $ Just uuid
                    _ -> return Nothing
            readSoup (KeyReadItem uuid (MkTupleUpdateReader SelectSecond ReadObject)) = do
                let path = dirpath </> uuidToName uuid
                mitem <- combineFstMFunction $ readFS $ FSReadItem path
                case mitem of
                    Just (FSFileItem object) -> do
                        muted <- combineSndMFunction @m $ acOpenObject path $ \call -> call object -- pointless
                        return $ Just muted
                    _ -> return Nothing
            pushSoup ::
                   [UpdateEdit ObjectSoupUpdate]
                -> CombineMonadIO m (AutoClose FilePath (Object ByteStringEdit)) (Maybe (EditSource -> CombineMonadIO m (AutoClose FilePath (Object ByteStringEdit)) ()))
            pushSoup =
                singleEdit $ \edit ->
                    fmap (fmap (fmap combineFstMFunction)) $
                    combineFstMFunction $
                    case edit of
                        KeyEditItem _uuid (MkTupleUpdateEdit SelectFirst iedit) -> never iedit
                        KeyEditItem _uuid (MkTupleUpdateEdit SelectSecond iedit) -> never iedit
                        KeyEditDelete uuid -> pushFS [FSEditDeleteNonDirectory $ dirpath </> uuidToName uuid]
                        KeyEditInsertReplace (uuid, bs) -> pushFS [FSEditCreateFile (dirpath </> uuidToName uuid) bs]
                        KeyEditClear -> do
                            mnames <- readFS $ FSReadDirectory dirpath
                            return $
                                case mnames of
                                    Just names ->
                                        Just $ \_ ->
                                            for_ names $ \name -> pushFS [FSEditDeleteNonDirectory $ dirpath </> name]
                                    Nothing -> Nothing
            in case dictWorkaround @m of
                   Dict -> MkRunnableIO runSoup (MkAnObject readSoup pushSoup)
