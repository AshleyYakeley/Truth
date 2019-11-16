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

type ObjectSoupUpdate = SoupUpdate (ObjectUpdate ByteStringUpdate)

type AutoCloseFileT = AutoCloseT FilePath (Object ByteStringEdit)

trunAutoClose :: TransStackRunner '[ AutoCloseFileT]
trunAutoClose = singleTransStackRunner runAutoClose

directorySoup :: Object FSEdit -> FilePath -> Object (UpdateEdit ObjectSoupUpdate)
directorySoup (MkRunnable1 (trunFS :: TransStackRunner tt) (MkAnObject readFS pushFS)) dirpath =
    case transStackRunnerUnliftAllDict trunFS of
        Dict ->
            case transStackDict @MonadUnliftIO @tt @IO of
                Dict -> let
                    runSoup :: TransStackRunner (AutoCloseFileT ': tt)
                    runSoup = cmAppend trunAutoClose trunFS
                    readSoup :: MutableRead (AutoCloseFileT (ApplyStack tt IO)) (UpdateReader ObjectSoupUpdate)
                    readSoup KeyReadKeys = do
                        mnames <- lift $ readFS $ FSReadDirectory dirpath
                        return $
                            case mnames of
                                Just names -> mapMaybe nameToUUID $ MkFiniteSet names
                                Nothing -> mempty
                    readSoup (KeyReadItem uuid (MkTupleUpdateReader SelectFirst ReadWhole)) = do
                        mitem <- lift $ readFS $ FSReadItem $ dirpath </> uuidToName uuid
                        case mitem of
                            Just (FSFileItem _) -> return $ Just uuid
                            _ -> return Nothing
                    readSoup (KeyReadItem uuid (MkTupleUpdateReader SelectSecond ReadObject)) = do
                        let path = dirpath </> uuidToName uuid
                        mitem <- lift $ readFS $ FSReadItem path
                        case mitem of
                            Just (FSFileItem object) -> do
                                muted <- remonad (stackLift @tt) $ acOpenObject path $ \call -> call object -- pointless
                                return $ Just muted
                            _ -> return Nothing
                    pushSoup ::
                           [UpdateEdit ObjectSoupUpdate]
                        -> AutoCloseFileT (ApplyStack tt IO) (Maybe (EditSource -> AutoCloseFileT (ApplyStack tt IO) ()))
                    pushSoup =
                        singleEdit $ \edit ->
                            fmap (fmap (fmap lift)) $
                            lift $
                            case edit of
                                KeyEditItem _uuid (MkTupleUpdateEdit SelectFirst iedit) -> never iedit
                                KeyEditItem _uuid (MkTupleUpdateEdit SelectSecond iedit) -> never iedit
                                KeyEditDelete uuid -> pushFS [FSEditDeleteNonDirectory $ dirpath </> uuidToName uuid]
                                KeyEditInsertReplace (uuid, bs) ->
                                    pushFS [FSEditCreateFile (dirpath </> uuidToName uuid) bs]
                                KeyEditClear -> do
                                    mnames <- readFS $ FSReadDirectory dirpath
                                    return $
                                        case mnames of
                                            Just names ->
                                                Just $ \_ ->
                                                    for_ names $ \name ->
                                                        pushFS [FSEditDeleteNonDirectory $ dirpath </> name]
                                            Nothing -> Nothing
                    in MkRunnable1 runSoup $ MkAnObject readSoup pushSoup
