module Truth.Core.Object.Tuple
    ( tupleObject
    , tupleObjectMaker
    , pairObjects
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.ObjectMaker
import Truth.Core.Read
import Truth.Core.Resource
import Truth.Core.Types

noneTupleObject :: Object (TupleUpdateEdit (ListElementType '[]))
noneTupleObject = MkResource1 nilResourceRunner noneTupleAObject

consTupleObjects ::
       forall update updates.
       Object (UpdateEdit update)
    -> Object (TupleUpdateEdit (ListElementType updates))
    -> Object (TupleUpdateEdit (ListElementType (update : updates)))
consTupleObjects =
    joinResource1 $ \(MkAnObject readA editA :: AnObject tt _) (MkAnObject readB editB) ->
        case transStackDict @MonadIO @tt @IO of
            Dict -> let
                readAB :: MutableRead (ApplyStack tt IO) (TupleUpdateReader (ListElementType (update : updates)))
                readAB (MkTupleUpdateReader FirstElementType r) = readA r
                readAB (MkTupleUpdateReader (RestElementType sel) r) = readB $ MkTupleUpdateReader sel r
                editAB ::
                       [TupleUpdateEdit (ListElementType (update : updates))]
                    -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
                editAB edits = let
                    (eas, ebs) = partitionListTupleUpdateEdits edits
                    in (liftA2 $ liftA2 $ liftA2 (>>)) (editA eas) (editB ebs)
                in MkAnObject readAB editAB

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

noneTupleAObject :: AnObject '[] (TupleUpdateEdit (ListElementType '[]))
noneTupleAObject = let
    objRead :: forall t. TupleUpdateReader (ListElementType '[]) t -> IO t
    objRead (MkTupleUpdateReader sel _) = case sel of {}
    objEdit :: [TupleUpdateEdit (ListElementType '[])] -> IO (Maybe (EditSource -> IO ()))
    objEdit [] = return $ Just $ \_ -> return ()
    objEdit (MkTupleUpdateEdit sel _:_) = case sel of {}
    in MkAnObject {..}

tupleListObjectM ::
       forall m updates. Applicative m
    => ListType Proxy updates
    -> (forall update. ListElementType updates update -> m (Object (UpdateEdit update)))
    -> m (Object (TupleUpdateEdit (ListElementType updates)))
tupleListObjectM lt getObject =
    case lt of
        NilListType -> pure noneTupleObject
        ConsListType Proxy lt' ->
            consTupleObjects <$> (getObject FirstElementType) <*>
            (tupleListObjectM lt' $ \sel -> getObject $ RestElementType sel)

tupleObjectM ::
       forall m sel. (IsFiniteConsWitness sel, Applicative m)
    => (forall update. sel update -> m (Object (UpdateEdit update)))
    -> m (Object (TupleUpdateEdit sel))
tupleObjectM pick =
    fmap (mapObject (tupleIsoLens fromLTW toLTW)) $ tupleListObjectM representative $ \sel -> pick $ fromLTW sel

tupleObject ::
       forall sel. IsFiniteConsWitness sel
    => (forall update. sel update -> Object (UpdateEdit update))
    -> Object (TupleUpdateEdit sel)
tupleObject pick = runIdentity $ tupleObjectM $ \sel -> Identity $ pick sel

tupleObjectMaker ::
       forall sel. IsFiniteConsWitness sel
    => (forall update. sel update -> ObjectMaker update ())
    -> ObjectMaker (TupleUpdate sel) ()
tupleObjectMaker pick recv = do
    obj <- tupleObjectM $ \sel -> fmap fst $ pick sel $ \updates -> recv $ fmap (MkTupleUpdate sel) updates
    return (obj, ())

pairObjects ::
       forall updatea updateb.
       Object (UpdateEdit updatea)
    -> Object (UpdateEdit updateb)
    -> Object (PairUpdateEdit updatea updateb)
pairObjects obja objb =
    tupleObject $ \case
        SelectFirst -> obja
        SelectSecond -> objb
