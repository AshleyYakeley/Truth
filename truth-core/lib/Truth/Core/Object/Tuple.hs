module Truth.Core.Object.Tuple
    ( tupleObject
    ) where

import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Core.Types

{-
pairObjects :: forall edita editb. Object edita -> Object editb -> Object (PairEdit edita editb)
pairObjects (MkObject (runA :: UnliftIO ma) readA editA) (MkObject (runB :: UnliftIO mb) readB editB) =
    case isCombineMonadIO @ma @mb of
        Dict -> let
            runAB :: UnliftIO (CombineMonadIO ma mb)
            runAB = combineUnliftIOs runA runB
            readAB :: MutableRead (CombineMonadIO ma mb) (PairEditReader edita editb)
            readAB (MkTupleEditReader SelectFirst r) = combineLiftFst @ma @mb $ readA r
            readAB (MkTupleEditReader SelectSecond r) = combineLiftSnd @ma @mb $ readB r
            editAB :: [PairEdit edita editb] -> CombineMonadIO ma mb (Maybe (CombineMonadIO ma mb ()))
            editAB edits = let
                (eas, ebs) = partitionPairEdits edits
                in liftA2
                       (liftA2 $ \mau mbu -> (>>) (combineLiftFst @ma @mb mau) (combineLiftSnd @ma @mb mbu))
                       (combineLiftFst @ma @mb $ editA eas)
                       (combineLiftSnd @ma @mb $ editB ebs)
            in MkObject runAB readAB editAB
-}
noneTupleObject :: Object (TupleEdit (ListThingWitness '[]))
noneTupleObject = let
    objRun = identityUnliftIO
    objRead :: forall t. TupleEditReader (ListThingWitness '[]) t -> IO t
    objRead (MkTupleEditReader sel _) = case sel of {}
    objEdit :: [TupleEdit (ListThingWitness '[])] -> IO (Maybe (IO ()))
    objEdit [] = return $ Just $ return ()
    objEdit (MkTupleEdit sel _:_) = case sel of {}
    in MkObject {..}

partitionListTupleEdits ::
       forall edit edits.
       [TupleEdit (ListThingWitness (edit : edits))]
    -> ([edit], [TupleEdit (ListThingWitness edits)])
partitionListTupleEdits pes = let
    toEither :: TupleEdit (ListThingWitness (edit : edits)) -> Either edit (TupleEdit (ListThingWitness edits))
    toEither (MkTupleEdit FirstListThingWitness ea) = Left ea
    toEither (MkTupleEdit (RestListThingWitness sel) eb) = Right $ MkTupleEdit sel eb
    in partitionEithers $ fmap toEither pes

consTupleObjects ::
       forall edit edits.
       Object edit
    -> Object (TupleEdit (ListThingWitness edits))
    -> Object (TupleEdit (ListThingWitness (edit : edits)))
consTupleObjects (MkObject (runA :: UnliftIO ma) readA editA) (MkObject (runB :: UnliftIO mb) readB editB) =
    case isCombineMonadIO @ma @mb of
        Dict -> let
            runAB :: UnliftIO (CombineMonadIO ma mb)
            runAB = combineUnliftIOs runA runB
            readAB :: MutableRead (CombineMonadIO ma mb) (TupleEditReader (ListThingWitness (edit : edits)))
            readAB (MkTupleEditReader FirstListThingWitness r) = combineLiftFst @ma @mb $ readA r
            readAB (MkTupleEditReader (RestListThingWitness sel) r) =
                combineLiftSnd @ma @mb $ readB $ MkTupleEditReader sel r
            editAB ::
                   [TupleEdit (ListThingWitness (edit : edits))]
                -> CombineMonadIO ma mb (Maybe (CombineMonadIO ma mb ()))
            editAB edits = let
                (eas, ebs) = partitionListTupleEdits edits
                in liftA2
                       (liftA2 $ \mau mbu -> (>>) (combineLiftFst @ma @mb mau) (combineLiftSnd @ma @mb mbu))
                       (combineLiftFst @ma @mb $ editA eas)
                       (combineLiftSnd @ma @mb $ editB ebs)
            in MkObject runAB readAB editAB

tupleListObject_ ::
       forall edits.
       ListType' Proxy edits
    -> (forall edit. ListThingWitness edits edit -> Object edit)
    -> Object (TupleEdit (ListThingWitness edits))
tupleListObject_ lt getObject =
    case lt of
        NilListType' -> noneTupleObject
        ConsListType' Proxy lt' ->
            consTupleObjects (getObject FirstListThingWitness) $
            tupleListObject_ lt' $ \sel -> getObject $ RestListThingWitness sel

tupleListObject ::
       forall edits. KnownList edits
    => (forall edit. ListThingWitness edits edit -> Object edit)
    -> Object (TupleEdit (ListThingWitness edits))
tupleListObject = tupleListObject_ listType

tupleObject ::
       forall sel. IsFiniteConsWitness sel
    => (forall edit. sel edit -> Object edit)
    -> Object (TupleEdit sel)
tupleObject pickObject =
    mapObject (tupleIsoLens fromLTW toLTW) $ tupleListObject $ \seledit -> pickObject $ fromLTW seledit
