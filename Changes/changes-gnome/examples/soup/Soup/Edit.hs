module Soup.Edit
    ( UUID
    , SoupUpdate
    , UUIDWidgetUpdate
    , ReferenceSoupUpdate
    , directorySoup
    , liftSoupLens
    ) where

import Changes.Core
import Changes.World.FileSystem
import Data.UUID
import Shapes
import System.FilePath

type UUIDWidgetUpdate update = PairUpdate (ConstWholeUpdate UUID) update

type SoupUpdate update = KeyUpdate [(UUID, UpdateSubject update)] (UUIDWidgetUpdate update)

liftSoupLens ::
       forall updateA updateB.
       ( ApplicableEdit (UpdateEdit updateA)
       , FullSubjectReader (UpdateReader updateA)
       , FullSubjectReader (UpdateReader updateB)
       )
    => (forall m. MonadIO m => UpdateSubject updateB -> m (Maybe (UpdateSubject updateA)))
    -> ChangeLens updateA updateB
    -> ChangeLens (SoupUpdate updateA) (SoupUpdate updateB)
liftSoupLens bmfa = let
    conv ::
           forall m. MonadIO m
        => (UUID, UpdateSubject updateB)
        -> m (Maybe (UUID, UpdateSubject updateA))
    conv (uuid, b) = fmap (fmap $ \a -> (uuid, a)) $ bmfa b
    in liftKeyElementChangeLens conv . sndLiftChangeLens

nameUUID :: Codec String UUID
nameUUID = MkCodec Data.UUID.fromString Data.UUID.toString

type ReferenceSoupUpdate = SoupUpdate (ReferenceUpdate ByteStringUpdate)

directorySoup :: Reference FSEdit -> FilePath -> Reference (UpdateEdit ReferenceSoupUpdate)
directorySoup (MkResource (runFS :: ResourceRunner tt) (MkAReference readFS pushFS ctask)) dirpath =
    case resourceRunnerUnliftDict runFS of
        Dict ->
            case transStackDict @MonadUnliftIO @tt @IO of
                Dict -> let
                    readSoup :: Readable (ApplyStack tt IO) (UpdateReader ReferenceSoupUpdate)
                    readSoup KeyReadKeys = do
                        mnames <- readFS $ FSReadDirectory dirpath
                        return $
                            case mnames of
                                Just names -> injectiveFilter nameUUID $ setFromList names
                                Nothing -> mempty
                    readSoup (KeyReadItem uuid (MkTupleUpdateReader SelectFirst ReadWhole)) = do
                        mitem <- readFS $ FSReadItem $ dirpath </> encode nameUUID uuid
                        return $
                            case mitem of
                                Just (FSFileItem _) -> Just uuid
                                _ -> Nothing
                    readSoup (KeyReadItem _uuid (MkTupleUpdateReader SelectSecond ReadReferenceResourceContext)) =
                        return $ Just emptyResourceContext
                    readSoup (KeyReadItem uuid (MkTupleUpdateReader SelectSecond ReadReference)) = do
                        let path = dirpath </> encode nameUUID uuid
                        mitem <- readFS $ FSReadItem path
                        return $
                            case mitem of
                                Just (FSFileItem reference) -> Just reference
                                _ -> Nothing
                    pushSoup ::
                           NonEmpty (UpdateEdit ReferenceSoupUpdate)
                        -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
                    pushSoup =
                        singleEdit $ \edit ->
                            case edit of
                                KeyEditItem _uuid (MkTupleUpdateEdit SelectFirst iedit) -> never iedit
                                KeyEditItem _uuid (MkTupleUpdateEdit SelectSecond iedit) -> never iedit
                                KeyEditDelete uuid ->
                                    pushFS $ pure $ FSEditDeleteNonDirectory $ dirpath </> encode nameUUID uuid
                                KeyEditInsertReplace (uuid, bs) ->
                                    pushFS $ pure $ FSEditCreateFile (dirpath </> encode nameUUID uuid) bs
                                KeyEditClear -> do
                                    mnames <- readFS $ FSReadDirectory dirpath
                                    return $
                                        case mnames of
                                            Just names ->
                                                Just $ \_ ->
                                                    for_ names $ \name ->
                                                        pushFS $ pure $ FSEditDeleteNonDirectory $ dirpath </> name
                                            Nothing -> Nothing
                    in MkResource runFS $ MkAReference readSoup pushSoup ctask
