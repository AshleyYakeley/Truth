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
noneTupleObject :: Object (TupleEdit (ListElementWitness '[]))
noneTupleObject = let
    objRun = id
    objRead :: forall t. TupleEditReader (ListElementWitness '[]) t -> IO t
    objRead (MkTupleEditReader sel _) = case sel of {}
    objEdit :: [TupleEdit (ListElementWitness '[])] -> IO (Maybe (IO ()))
    objEdit [] = return $ Just $ return ()
    objEdit (MkTupleEdit sel _:_) = case sel of {}
    in MkObject {..}

partitionListTupleEdits ::
       forall edit edits.
       [TupleEdit (ListElementWitness (edit : edits))]
    -> ([edit], [TupleEdit (ListElementWitness edits)])
partitionListTupleEdits pes = let
    toEither :: TupleEdit (ListElementWitness (edit : edits)) -> Either edit (TupleEdit (ListElementWitness edits))
    toEither (MkTupleEdit FirstListElementWitness ea) = Left ea
    toEither (MkTupleEdit (RestListElementWitness sel) eb) = Right $ MkTupleEdit sel eb
    in partitionEithers $ fmap toEither pes

consTupleObjects ::
       forall edit edits.
       Object edit
    -> Object (TupleEdit (ListElementWitness edits))
    -> Object (TupleEdit (ListElementWitness (edit : edits)))
consTupleObjects (MkObject (runA :: UnliftIO ma) readA editA) (MkObject (runB :: UnliftIO mb) readB editB) =
    case isCombineMonadIO @ma @mb of
        Dict -> let
            runAB :: UnliftIO (CombineMonadIO ma mb)
            runAB = combineUnliftIOs runA runB
            readAB :: MutableRead (CombineMonadIO ma mb) (TupleEditReader (ListElementWitness (edit : edits)))
            readAB (MkTupleEditReader FirstListElementWitness r) = combineLiftFst @ma @mb $ readA r
            readAB (MkTupleEditReader (RestListElementWitness sel) r) =
                combineLiftSnd @ma @mb $ readB $ MkTupleEditReader sel r
            editAB ::
                   [TupleEdit (ListElementWitness (edit : edits))]
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
       ListType Proxy edits
    -> (forall edit. ListElementWitness edits edit -> Object edit)
    -> Object (TupleEdit (ListElementWitness edits))
tupleListObject_ lt getObject =
    case lt of
        NilListType -> noneTupleObject
        ConsListType Proxy lt' ->
            consTupleObjects (getObject FirstListElementWitness) $
            tupleListObject_ lt' $ \sel -> getObject $ RestListElementWitness sel

tupleListObject ::
       forall edits. Is (ListType Proxy) edits
    => (forall edit. ListElementWitness edits edit -> Object edit)
    -> Object (TupleEdit (ListElementWitness edits))
tupleListObject = tupleListObject_ representative

tupleObject ::
       forall sel. IsFiniteConsWitness sel
    => (forall edit. sel edit -> Object edit)
    -> Object (TupleEdit sel)
tupleObject pickObject =
    mapObject (tupleIsoLens fromLTW toLTW) $ tupleListObject $ \seledit -> pickObject $ fromLTW seledit
