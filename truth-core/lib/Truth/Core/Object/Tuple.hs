module Truth.Core.Object.Tuple
    ( tupleObject
    , tupleObjectMaker
    , pairObjects
    ) where

import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.ObjectMaker
import Truth.Core.Object.UnliftIO
import Truth.Core.Read
import Truth.Core.Types

noneTupleObject :: Object (TupleEdit (ListElementType '[]))
noneTupleObject = MkCloseUnliftIO id noneTupleAObject

consTupleObjects ::
       forall edit edits.
       Object edit
    -> Object (TupleEdit (ListElementType edits))
    -> Object (TupleEdit (ListElementType (edit : edits)))
consTupleObjects (MkCloseUnliftIO (runA :: UnliftIO ma) anobjA) (MkCloseUnliftIO (runB :: UnliftIO mb) anobjB) =
    case isCombineMonadIO @ma @mb of
        Dict -> let
            runAB :: UnliftIO (CombineMonadIO ma mb)
            runAB = combineUnliftIOs runA runB
            in MkCloseUnliftIO runAB $ consTupleAObjects anobjA anobjB

partitionListTupleEdits ::
       forall edit edits. [TupleEdit (ListElementType (edit : edits))] -> ([edit], [TupleEdit (ListElementType edits)])
partitionListTupleEdits pes = let
    toEither :: TupleEdit (ListElementType (edit : edits)) -> Either edit (TupleEdit (ListElementType edits))
    toEither (MkTupleEdit FirstElementType ea) = Left ea
    toEither (MkTupleEdit (RestElementType sel) eb) = Right $ MkTupleEdit sel eb
    in partitionEithers $ fmap toEither pes

noneTupleAObject :: AnObject IO (TupleEdit (ListElementType '[]))
noneTupleAObject = let
    objRead :: forall t. TupleEditReader (ListElementType '[]) t -> IO t
    objRead (MkTupleEditReader sel _) = case sel of {}
    objEdit :: [TupleEdit (ListElementType '[])] -> IO (Maybe (EditSource -> IO ()))
    objEdit [] = return $ Just $ \_ -> return ()
    objEdit (MkTupleEdit sel _:_) = case sel of {}
    in MkAnObject {..}

consTupleAObjects ::
       forall ma mb edit edits. (MonadStackIO ma, MonadStackIO mb)
    => AnObject ma edit
    -> AnObject mb (TupleEdit (ListElementType edits))
    -> AnObject (CombineMonadIO ma mb) (TupleEdit (ListElementType (edit : edits)))
consTupleAObjects (MkAnObject readA editA) (MkAnObject readB editB) =
    case isCombineMonadIO @ma @mb of
        Dict -> let
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
            in MkAnObject readAB editAB

tupleListObjectM ::
       forall m edits. Applicative m
    => ListType Proxy edits
    -> (forall edit. ListElementType edits edit -> m (Object edit))
    -> m (Object (TupleEdit (ListElementType edits)))
tupleListObjectM lt getObject =
    case lt of
        NilListType -> pure noneTupleObject
        ConsListType Proxy lt' ->
            consTupleObjects <$> (getObject FirstElementType) <*>
            (tupleListObjectM lt' $ \sel -> getObject $ RestElementType sel)

tupleObjectM ::
       forall m sel. (IsFiniteConsWitness sel, Applicative m)
    => (forall edit. sel edit -> m (Object edit))
    -> m (Object (TupleEdit sel))
tupleObjectM pick =
    fmap (mapObject (tupleIsoLens fromLTW toLTW)) $ tupleListObjectM representative $ \sel -> pick $ fromLTW sel

tupleObject ::
       forall sel. IsFiniteConsWitness sel
    => (forall edit. sel edit -> Object edit)
    -> Object (TupleEdit sel)
tupleObject pick = runIdentity $ tupleObjectM $ \sel -> Identity $ pick sel

tupleObjectMaker ::
       forall sel. IsFiniteConsWitness sel
    => (forall edit. sel edit -> ObjectMaker edit ())
    -> ObjectMaker (TupleEdit sel) ()
tupleObjectMaker pick update = do
    obj <- tupleObjectM $ \sel -> fmap fst $ pick sel (\edits -> update $ fmap (MkTupleEdit sel) edits)
    return (obj, ())

pairObjects :: forall edita editb. Object edita -> Object editb -> Object (PairEdit edita editb)
pairObjects obja objb =
    tupleObject $ \case
        SelectFirst -> obja
        SelectSecond -> objb
