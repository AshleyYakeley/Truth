module Soup.Edit
    ( UUID
    , SoupUpdate
    , UUIDElementUpdate
    , ObjectSoupUpdate
    , directorySoup
    , liftSoupLens
    ) where

import Data.UUID
import Shapes
import System.FilePath hiding ((<.>))
import Truth.Core
import Truth.World.FileSystem

type UUIDElementUpdate update = PairUpdate (ConstWholeUpdate UUID) update

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

directorySoup :: Object FSEdit -> FilePath -> Object (UpdateEdit ObjectSoupUpdate)
directorySoup (MkResource (runFS :: ResourceRunner tt) (MkAnObject readFS pushFS)) dirpath =
    case resourceRunnerUnliftAllDict runFS of
        Dict ->
            case transStackDict @MonadUnliftIO @tt @IO of
                Dict -> let
                    readSoup :: MutableRead (ApplyStack tt IO) (UpdateReader ObjectSoupUpdate)
                    readSoup KeyReadKeys = do
                        mnames <- readFS $ FSReadDirectory dirpath
                        return $
                            case mnames of
                                Just names -> mapMaybe nameToUUID $ MkFiniteSet names
                                Nothing -> mempty
                    readSoup (KeyReadItem uuid (MkTupleUpdateReader SelectFirst ReadWhole)) = do
                        mitem <- readFS $ FSReadItem $ dirpath </> uuidToName uuid
                        return $
                            case mitem of
                                Just (FSFileItem _) -> Just uuid
                                _ -> Nothing
                    readSoup (KeyReadItem uuid (MkTupleUpdateReader SelectSecond ReadObject)) = do
                        let path = dirpath </> uuidToName uuid
                        mitem <- readFS $ FSReadItem path
                        return $
                            case mitem of
                                Just (FSFileItem object) -> Just object
                                _ -> Nothing
                    pushSoup ::
                           NonEmpty (UpdateEdit ObjectSoupUpdate)
                        -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
                    pushSoup =
                        singleEdit $ \edit ->
                            case edit of
                                KeyEditItem _uuid (MkTupleUpdateEdit SelectFirst iedit) -> never iedit
                                KeyEditItem _uuid (MkTupleUpdateEdit SelectSecond iedit) -> never iedit
                                KeyEditDelete uuid ->
                                    pushFS $ pure $ FSEditDeleteNonDirectory $ dirpath </> uuidToName uuid
                                KeyEditInsertReplace (uuid, bs) ->
                                    pushFS $ pure $ FSEditCreateFile (dirpath </> uuidToName uuid) bs
                                KeyEditClear -> do
                                    mnames <- readFS $ FSReadDirectory dirpath
                                    return $
                                        case mnames of
                                            Just names ->
                                                Just $ \_ ->
                                                    for_ names $ \name ->
                                                        pushFS $ pure $ FSEditDeleteNonDirectory $ dirpath </> name
                                            Nothing -> Nothing
                    in MkResource runFS $ MkAnObject readSoup pushSoup
