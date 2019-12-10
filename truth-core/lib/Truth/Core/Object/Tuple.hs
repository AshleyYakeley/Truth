module Truth.Core.Object.Tuple
    ( tupleObject
    , pairObjects
    , tupleObjectMaker
    , tupleSubscriber
    , pairSubscribers
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.ObjectMaker
import Truth.Core.Object.Subscriber
import Truth.Core.Read
import Truth.Core.Resource
import Truth.Core.Types

class (forall update. MapResource (f update)) => TupleResource (f :: Type -> [TransKind] -> Type) where
    noneTupleAResource :: f (TupleUpdate (ListElementType '[])) '[]
    consTupleAResource ::
           forall tt update updates. MonadTransStackUnliftAll tt
        => f update tt
        -> f (TupleUpdate (ListElementType updates)) tt
        -> f (TupleUpdate (ListElementType (update : updates))) tt
    mapResourceUpdate :: EditLens updateA updateB -> Resource (f updateA) -> Resource (f updateB)

newtype UAnObject (update :: Type) (tt :: [TransKind]) = MkUAnObject
    { unUAnObject :: AnObject (UpdateEdit update) tt
    }

type UObject update = Resource (UAnObject update)

uObjToObj :: UObject update -> Object (UpdateEdit update)
uObjToObj (MkResource rr (MkUAnObject anobj)) = MkResource rr anobj

objToUObj :: Object (UpdateEdit update) -> UObject update
objToUObj (MkResource rr anobj) = MkResource rr $ MkUAnObject anobj

instance MapResource (UAnObject update) where
    mapResource ::
           forall tt1 tt2. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2)
        => TransListFunction tt1 tt2
        -> UAnObject update tt1
        -> UAnObject update tt2
    mapResource f (MkUAnObject obj) = MkUAnObject $ mapResource f obj

noneTupleResource :: TupleResource f => Resource (f (TupleUpdate (ListElementType '[])))
noneTupleResource = MkResource nilResourceRunner noneTupleAResource

consTupleResource ::
       forall f update updates. TupleResource f
    => Resource (f update)
    -> Resource (f (TupleUpdate (ListElementType updates)))
    -> Resource (f (TupleUpdate (ListElementType (update : updates))))
consTupleResource = joinResource consTupleAResource

tupleListResourceM ::
       forall f m updates. (TupleResource f, Applicative m)
    => ListType Proxy updates
    -> (forall update. ListElementType updates update -> m (Resource (f update)))
    -> m (Resource (f (TupleUpdate (ListElementType updates))))
tupleListResourceM lt getObject =
    case lt of
        NilListType -> pure noneTupleResource
        ConsListType Proxy lt' ->
            consTupleResource <$> (getObject FirstElementType) <*>
            (tupleListResourceM lt' $ \sel -> getObject $ RestElementType sel)

tupleResourceM ::
       forall f m sel. (TupleResource f, IsFiniteConsWitness sel, Applicative m)
    => (forall update. sel update -> m (Resource (f update)))
    -> m (Resource (f (TupleUpdate sel)))
tupleResourceM pick =
    fmap (mapResourceUpdate (tupleIsoLens fromLTW toLTW)) $
    tupleListResourceM representative $ \sel -> pick $ fromLTW sel

tupleResource ::
       forall f sel. (TupleResource f, IsFiniteConsWitness sel)
    => (forall update. sel update -> Resource (f update))
    -> Resource (f (TupleUpdate sel))
tupleResource pick = runIdentity $ tupleResourceM $ \sel -> Identity $ pick sel

pairResource ::
       forall f updatea updateb. TupleResource f
    => Resource (f updatea)
    -> Resource (f updateb)
    -> Resource (f (PairUpdate updatea updateb))
pairResource ra rb =
    tupleResource $ \case
        SelectFirst -> ra
        SelectSecond -> rb

partitionListTupleUpdateEdits ::
       forall update updates.
       [TupleUpdateEdit (ListElementType (update : updates))]
    -> ([UpdateEdit update], [TupleUpdateEdit (ListElementType updates)])
partitionListTupleUpdateEdits pes = let
    toEither ::
           TupleUpdateEdit (ListElementType (update : updates))
        -> Either (UpdateEdit update) (TupleUpdateEdit (ListElementType updates))
    toEither (MkTupleUpdateEdit FirstElementType ea) = Left ea
    toEither (MkTupleUpdateEdit (RestElementType sel) eb) = Right $ MkTupleUpdateEdit sel eb
    in partitionEithers $ fmap toEither pes

instance TupleResource UAnObject where
    noneTupleAResource = let
        objRead :: forall t. TupleUpdateReader (ListElementType '[]) t -> IO t
        objRead (MkTupleUpdateReader sel _) = case sel of {}
        objEdit :: NonEmpty (TupleUpdateEdit (ListElementType '[])) -> IO (Maybe (EditSource -> IO ()))
        objEdit (MkTupleUpdateEdit sel _ :| _) = case sel of {}
        in MkUAnObject $ MkAnObject {..}
    consTupleAResource ::
           forall tt update updates. MonadTransStackUnliftAll tt
        => UAnObject update tt
        -> UAnObject (TupleUpdate (ListElementType updates)) tt
        -> UAnObject (TupleUpdate (ListElementType (update : updates))) tt
    consTupleAResource (MkUAnObject (MkAnObject readA editA)) (MkUAnObject (MkAnObject readB editB)) =
        case transStackDict @MonadIO @tt @IO of
            Dict -> let
                readAB :: MutableRead (ApplyStack tt IO) (TupleUpdateReader (ListElementType (update : updates)))
                readAB (MkTupleUpdateReader FirstElementType r) = readA r
                readAB (MkTupleUpdateReader (RestElementType sel) r) = readB $ MkTupleUpdateReader sel r
                editAB ::
                       NonEmpty (TupleUpdateEdit (ListElementType (update : updates)))
                    -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
                editAB edits = let
                    (eas, ebs) = partitionListTupleUpdateEdits (toList edits)
                    in case (nonEmpty eas, nonEmpty ebs) of
                           (Nothing, Nothing) -> return $ Just $ \_ -> return ()
                           (Just eas', Nothing) -> editA eas'
                           (Nothing, Just ebs') -> editB ebs'
                           (Just eas', Just ebs') -> (liftA2 $ liftA2 $ liftA2 (>>)) (editA eas') (editB ebs')
                in MkUAnObject $ MkAnObject readAB editAB
    mapResourceUpdate lens uobj = objToUObj $ mapObject lens $ uObjToObj uobj

instance TupleResource ASubscriber where
    noneTupleAResource :: ASubscriber (TupleUpdate (ListElementType '[])) '[]
    noneTupleAResource = MkASubscriber (unUAnObject noneTupleAResource) $ \_ -> return ()
    consTupleAResource ::
           forall tt update updates. MonadTransStackUnliftAll tt
        => ASubscriber update tt
        -> ASubscriber (TupleUpdate (ListElementType updates)) tt
        -> ASubscriber (TupleUpdate (ListElementType (update : updates))) tt
    consTupleAResource (MkASubscriber anobj1 sub1) (MkASubscriber anobj2 sub2) =
        case transStackDict @MonadIO @tt @IO of
            Dict -> let
                anobj12 = unUAnObject $ consTupleAResource (MkUAnObject anobj1) (MkUAnObject anobj2)
                sub12 recv12 = do
                    let
                        recv1 u1 ec = recv12 (fmap (\u -> MkTupleUpdate FirstElementType u) u1) ec
                        recv2 u2 ec =
                            recv12 (fmap (\(MkTupleUpdate sel u) -> MkTupleUpdate (RestElementType sel) u) u2) ec
                    sub1 recv1
                    sub2 recv2
                in MkASubscriber anobj12 sub12
    mapResourceUpdate :: EditLens updateA updateB -> Subscriber updateA -> Subscriber updateB
    mapResourceUpdate = mapPureSubscriber

tupleObject ::
       forall sel. IsFiniteConsWitness sel
    => (forall update. sel update -> Object (UpdateEdit update))
    -> Object (TupleUpdateEdit sel)
tupleObject pick = uObjToObj $ tupleResource $ \selu -> objToUObj $ pick selu

tupleObjectMaker ::
       forall sel. IsFiniteConsWitness sel
    => (forall update. sel update -> ObjectMaker update ())
    -> ObjectMaker (TupleUpdate sel) ()
tupleObjectMaker pick recv = do
    uobj <-
        tupleResourceM $ \sel -> fmap (objToUObj . fst) $ pick sel $ \updates -> recv $ fmap (MkTupleUpdate sel) updates
    return (uObjToObj uobj, ())

tupleSubscriber ::
       forall sel. IsFiniteConsWitness sel
    => (forall update. sel update -> Subscriber update)
    -> Subscriber (TupleUpdate sel)
tupleSubscriber = tupleResource

pairObjects ::
       forall updatea updateb.
       Object (UpdateEdit updatea)
    -> Object (UpdateEdit updateb)
    -> Object (PairUpdateEdit updatea updateb)
pairObjects obja objb = uObjToObj $ pairResource (objToUObj obja) (objToUObj objb)

pairSubscribers ::
       forall updatea updateb. Subscriber updatea -> Subscriber updateb -> Subscriber (PairUpdate updatea updateb)
pairSubscribers = pairResource
