module Truth.Core.Object.Tuple
    ( tupleObject
    , pairObjects
    , pairSubscribers
    ) where

import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Object.Subscriber
import Truth.Core.Read
import Truth.Core.Types

noneTupleObject :: Object (TupleEdit (ListElementType '[]))
noneTupleObject = let
    objRun = id
    objRead :: forall t. TupleEditReader (ListElementType '[]) t -> IO t
    objRead (MkTupleEditReader sel _) = case sel of {}
    objEdit :: [TupleEdit (ListElementType '[])] -> IO (Maybe (EditSource -> IO ()))
    objEdit [] = return $ Just $ \_ -> return ()
    objEdit (MkTupleEdit sel _:_) = case sel of {}
    in MkObject {..}

partitionListTupleEdits ::
       forall edit edits. [TupleEdit (ListElementType (edit : edits))] -> ([edit], [TupleEdit (ListElementType edits)])
partitionListTupleEdits pes = let
    toEither :: TupleEdit (ListElementType (edit : edits)) -> Either edit (TupleEdit (ListElementType edits))
    toEither (MkTupleEdit FirstElementType ea) = Left ea
    toEither (MkTupleEdit (RestElementType sel) eb) = Right $ MkTupleEdit sel eb
    in partitionEithers $ fmap toEither pes

consTupleObjects ::
       forall edit edits.
       Object edit
    -> Object (TupleEdit (ListElementType edits))
    -> Object (TupleEdit (ListElementType (edit : edits)))
consTupleObjects (MkObject (runA :: UnliftIO ma) readA editA) (MkObject (runB :: UnliftIO mb) readB editB) =
    case isCombineMonadIO @ma @mb of
        Dict -> let
            runAB :: UnliftIO (CombineMonadIO ma mb)
            runAB = combineUnliftIOs runA runB
            readAB :: MutableRead (CombineMonadIO ma mb) (TupleEditReader (ListElementType (edit : edits)))
            readAB (MkTupleEditReader FirstElementType r) = combineLiftFst @ma @mb $ readA r
            readAB (MkTupleEditReader (RestElementType sel) r) =
                combineLiftSnd @ma @mb $ readB $ MkTupleEditReader sel r
            editAB ::
                   [TupleEdit (ListElementType (edit : edits))]
                -> CombineMonadIO ma mb (Maybe (EditSource -> CombineMonadIO ma mb ()))
            editAB edits = let
                (eas, ebs) = partitionListTupleEdits edits
                in liftA2
                       (liftA2 $ liftA2 $ \mau mbu -> (>>) (combineLiftFst @ma @mb mau) (combineLiftSnd @ma @mb mbu))
                       (combineLiftFst @ma @mb $ editA eas)
                       (combineLiftSnd @ma @mb $ editB ebs)
            in MkObject runAB readAB editAB

tupleListObject ::
       forall edits.
       ListType Proxy edits
    -> (forall edit. ListElementType edits edit -> Object edit)
    -> Object (TupleEdit (ListElementType edits))
tupleListObject lt getObject =
    case lt of
        NilListType -> noneTupleObject
        ConsListType Proxy lt' ->
            consTupleObjects (getObject FirstElementType) $
            tupleListObject lt' $ \sel -> getObject $ RestElementType sel

tupleObject ::
       forall sel. IsFiniteConsWitness sel
    => (forall edit. sel edit -> Object edit)
    -> Object (TupleEdit sel)
tupleObject pickObject =
    mapObject (tupleIsoLens fromLTW toLTW) $ tupleListObject representative $ \sel -> pickObject $ fromLTW sel

pairObjects :: forall edita editb. Object edita -> Object editb -> Object (PairEdit edita editb)
pairObjects obja objb =
    tupleObject $ \case
        SelectFirst -> obja
        SelectSecond -> objb

pairSubscribers :: forall edita editb. Subscriber edita -> Subscriber editb -> Subscriber (PairEdit edita editb)
pairSubscribers (MkSubscriber obja suba) (MkSubscriber objb subb) = let
    objab = pairObjects obja objb
    subab recvab = let
        recva edits = recvab $ fmap (MkTupleEdit SelectFirst) edits
        recvb edits = recvab $ fmap (MkTupleEdit SelectSecond) edits
        in do
               suba recva
               subb recvb
    in MkSubscriber objab subab
