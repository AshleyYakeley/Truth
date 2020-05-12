module Soup.Edit
    ( UUID
    , SoupUpdate
    , UUIDElementUpdate
    , ReferenceSoupUpdate
    , directorySoup
    , liftSoupLens
    ) where

import Data.UUID
import Shapes
import System.FilePath hiding ((<.>))
import Truth.Core
import Truth.World.FileSystem
import Truth.Debug.Object

type UUIDElementUpdate update = PairUpdate (ConstWholeUpdate UUID) update

type SoupUpdate update = KeyUpdate [(UUID, UpdateSubject update)] (UUIDElementUpdate update)

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

nameToUUID :: String -> Maybe UUID
nameToUUID = Data.UUID.fromString

uuidToName :: UUID -> String
uuidToName = Data.UUID.toString

type ReferenceSoupUpdate = SoupUpdate (ReferenceUpdate ByteStringUpdate)

directorySoup :: Reference FSEdit -> FilePath -> Reference (UpdateEdit ReferenceSoupUpdate)
directorySoup (MkResource (runFS :: ResourceRunner tt) (MkAReference readFS pushFS ctask)) dirpath =
    traceArgThing "soupdir" $
    case resourceRunnerUnliftAllDict runFS of
        Dict ->
            case transStackDict @MonadUnliftIO @tt @IO of
                Dict -> let
                    readSoup :: Readable (ApplyStack tt IO) (UpdateReader ReferenceSoupUpdate)
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
                    readSoup (KeyReadItem _uuid (MkTupleUpdateReader SelectSecond ReadReferenceResourceContext)) =
                        return $ Just emptyResourceContext
                    readSoup (KeyReadItem uuid (MkTupleUpdateReader SelectSecond ReadReference)) = do
                        let path = dirpath </> uuidToName uuid
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
                    in MkResource runFS $ MkAReference readSoup pushSoup ctask
