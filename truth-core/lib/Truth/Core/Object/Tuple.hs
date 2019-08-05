module Truth.Core.Object.Tuple
    ( tupleObject
    , tupleUpdatingObject
    , tupleSubscribers
    , pairObjects
    , pairSubscribers
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Lens
import Truth.Core.Object.Object
import Truth.Core.Object.Subscriber
import Truth.Core.Object.UnliftIO
import Truth.Core.Object.UpdatingObject
import Truth.Core.Read
import Truth.Core.Types

class ObjectOrSubscriber (f :: (Type -> Type) -> Type -> Type) where
    mapThing :: EditLens edita editb -> CloseUnliftIO f edita -> CloseUnliftIO f editb
    noneTupleAThing :: f IO (TupleEdit (ListElementType '[]))
    consTupleAThings ::
           forall ma mb edit edits. (MonadStackIO ma, MonadStackIO mb)
        => f ma edit
        -> f mb (TupleEdit (ListElementType edits))
        -> f (CombineMonadIO ma mb) (TupleEdit (ListElementType (edit : edits)))

noneTupleThing ::
       forall f. ObjectOrSubscriber f
    => CloseUnliftIO f (TupleEdit (ListElementType '[]))
noneTupleThing = MkCloseUnliftIO id noneTupleAThing

consTupleThings ::
       forall f edit edits. ObjectOrSubscriber f
    => CloseUnliftIO f edit
    -> CloseUnliftIO f (TupleEdit (ListElementType edits))
    -> CloseUnliftIO f (TupleEdit (ListElementType (edit : edits)))
consTupleThings (MkCloseUnliftIO (runA :: UnliftIO ma) anobjA) (MkCloseUnliftIO (runB :: UnliftIO mb) anobjB) =
    case isCombineMonadIO @ma @mb of
        Dict -> let
            runAB :: UnliftIO (CombineMonadIO ma mb)
            runAB = combineUnliftIOs runA runB
            in MkCloseUnliftIO runAB $ consTupleAThings anobjA anobjB

partitionListTupleEdits ::
       forall edit edits. [TupleEdit (ListElementType (edit : edits))] -> ([edit], [TupleEdit (ListElementType edits)])
partitionListTupleEdits pes = let
    toEither :: TupleEdit (ListElementType (edit : edits)) -> Either edit (TupleEdit (ListElementType edits))
    toEither (MkTupleEdit FirstElementType ea) = Left ea
    toEither (MkTupleEdit (RestElementType sel) eb) = Right $ MkTupleEdit sel eb
    in partitionEithers $ fmap toEither pes

instance ObjectOrSubscriber AnObject where
    mapThing = mapObject
    noneTupleAThing :: AnObject IO (TupleEdit (ListElementType '[]))
    noneTupleAThing = let
        objRead :: forall t. TupleEditReader (ListElementType '[]) t -> IO t
        objRead (MkTupleEditReader sel _) = case sel of {}
        objEdit :: [TupleEdit (ListElementType '[])] -> IO (Maybe (EditSource -> IO ()))
        objEdit [] = return $ Just $ \_ -> return ()
        objEdit (MkTupleEdit sel _:_) = case sel of {}
        in MkAnObject {..}
    consTupleAThings ::
           forall ma mb edit edits. (MonadStackIO ma, MonadStackIO mb)
        => AnObject ma edit
        -> AnObject mb (TupleEdit (ListElementType edits))
        -> AnObject (CombineMonadIO ma mb) (TupleEdit (ListElementType (edit : edits)))
    consTupleAThings (MkAnObject readA editA) (MkAnObject readB editB) =
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

instance ObjectOrSubscriber ASubscriber where
    mapThing = mapSubscriber
    noneTupleAThing :: ASubscriber IO (TupleEdit (ListElementType '[]))
    noneTupleAThing = MkASubscriber noneTupleAThing $ \_ -> return ()
    consTupleAThings ::
           forall ma mb edit edits. (MonadStackIO ma, MonadStackIO mb)
        => ASubscriber ma edit
        -> ASubscriber mb (TupleEdit (ListElementType edits))
        -> ASubscriber (CombineMonadIO ma mb) (TupleEdit (ListElementType (edit : edits)))
    consTupleAThings (MkASubscriber aobjA subA) (MkASubscriber aobjB subB) =
        case isCombineMonadIO @ma @mb of
            Dict -> let
                aobjAB :: AnObject (CombineMonadIO ma mb) (TupleEdit (ListElementType (edit : edits)))
                aobjAB = consTupleAThings aobjA aobjB
                subAB ::
                       ([TupleEdit (ListElementType (edit : edits))] -> EditContext -> IO ())
                    -> LifeCycleT (MonadStackTrans ma mb) ()
                subAB update = do
                    remonad (combineLiftFst @ma @mb) $
                        subA $ \ee ec -> update (fmap (MkTupleEdit FirstElementType) ee) ec
                    remonad (combineLiftSnd @ma @mb) $
                        subB $ \ee ec ->
                            update (fmap (\(MkTupleEdit sel e) -> MkTupleEdit (RestElementType sel) e) ee) ec
                in MkASubscriber aobjAB subAB

tupleListThingM ::
       forall m f edits. (Applicative m, ObjectOrSubscriber f)
    => ListType Proxy edits
    -> (forall edit. ListElementType edits edit -> m (CloseUnliftIO f edit))
    -> m (CloseUnliftIO f (TupleEdit (ListElementType edits)))
tupleListThingM lt getObject =
    case lt of
        NilListType -> pure noneTupleThing
        ConsListType Proxy lt' ->
            consTupleThings <$> (getObject FirstElementType) <*>
            (tupleListThingM lt' $ \sel -> getObject $ RestElementType sel)

tupleThingM ::
       forall m f sel. (ObjectOrSubscriber f, IsFiniteConsWitness sel, Applicative m)
    => (forall edit. sel edit -> m (CloseUnliftIO f edit))
    -> m (CloseUnliftIO f (TupleEdit sel))
tupleThingM pick =
    fmap (mapThing (tupleIsoLens fromLTW toLTW)) $ tupleListThingM representative $ \sel -> pick $ fromLTW sel

tupleThing ::
       forall f sel. (ObjectOrSubscriber f, IsFiniteConsWitness sel)
    => (forall edit. sel edit -> CloseUnliftIO f edit)
    -> CloseUnliftIO f (TupleEdit sel)
tupleThing pick = runIdentity $ tupleThingM $ \sel -> Identity $ pick sel

tupleObject ::
       forall sel. IsFiniteConsWitness sel
    => (forall edit. sel edit -> Object edit)
    -> Object (TupleEdit sel)
tupleObject = tupleThing

tupleUpdatingObject ::
       forall sel. IsFiniteConsWitness sel
    => (forall edit. sel edit -> UpdatingObject edit ())
    -> UpdatingObject (TupleEdit sel) ()
tupleUpdatingObject pick update = do
    obj <- tupleThingM $ \sel -> fmap fst $ pick sel (\edits -> update $ fmap (MkTupleEdit sel) edits)
    return (obj, ())

tupleSubscribers ::
       forall sel. IsFiniteConsWitness sel
    => (forall edit. sel edit -> Subscriber edit)
    -> Subscriber (TupleEdit sel)
tupleSubscribers = tupleThing

pairThings ::
       forall f edita editb. ObjectOrSubscriber f
    => CloseUnliftIO f edita
    -> CloseUnliftIO f editb
    -> CloseUnliftIO f (PairEdit edita editb)
pairThings obja objb =
    tupleThing $ \case
        SelectFirst -> obja
        SelectSecond -> objb

pairObjects :: forall edita editb. Object edita -> Object editb -> Object (PairEdit edita editb)
pairObjects = pairThings

pairSubscribers :: forall edita editb. Subscriber edita -> Subscriber editb -> Subscriber (PairEdit edita editb)
pairSubscribers = pairThings
