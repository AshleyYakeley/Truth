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
import Truth.Core.Types

noneTupleObject :: Object (TupleUpdateEdit (ListElementType '[]))
noneTupleObject = MkRunnable1 cmEmpty noneTupleAObject

consTupleObjects ::
       forall update updates.
       Object (UpdateEdit update)
    -> Object (TupleUpdateEdit (ListElementType updates))
    -> Object (TupleUpdateEdit (ListElementType (update : updates)))
consTupleObjects (MkRunnable1 (runA :: TransStackRunner tta) anobjA) (MkRunnable1 (runB :: TransStackRunner ttb) anobjB) =
    case transStackRunnerUnliftAllDict runA of
        Dict ->
            case transStackRunnerUnliftAllDict runB of
                Dict ->
                    case concatMonadTransStackUnliftAllDict @tta @ttb of
                        Dict -> let
                            runAB :: TransStackRunner (Concat tta ttb)
                            runAB = cmAppend runA runB
                            in MkRunnable1 runAB $ consTupleAObjects anobjA anobjB

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

consTupleAObjects ::
       forall tta ttb update updates. (MonadTransStackUnliftAll tta, MonadTransStackUnliftAll ttb)
    => AnObject tta (UpdateEdit update)
    -> AnObject ttb (TupleUpdateEdit (ListElementType updates))
    -> AnObject (Concat tta ttb) (TupleUpdateEdit (ListElementType (update : updates)))
consTupleAObjects (MkAnObject readA editA) (MkAnObject readB editB) =
    case concatMonadTransStackUnliftAllDict @tta @ttb of
        Dict ->
            case transStackDict @MonadIO @(Concat tta ttb) @IO of
                Dict -> let
                    readAB ::
                           MutableRead (ApplyStack (Concat tta ttb) IO) (TupleUpdateReader (ListElementType (update : updates)))
                    readAB (MkTupleUpdateReader FirstElementType r) = concatFstMFunction @tta @ttb @IO $ readA r
                    readAB (MkTupleUpdateReader (RestElementType sel) r) =
                        concatSndMFunction @tta @ttb @IO $ readB $ MkTupleUpdateReader sel r
                    editAB ::
                           [TupleUpdateEdit (ListElementType (update : updates))]
                        -> ApplyStack (Concat tta ttb) IO (Maybe (EditSource -> ApplyStack (Concat tta ttb) IO ()))
                    editAB edits = let
                        (eas, ebs) = partitionListTupleUpdateEdits edits
                        in liftA2
                               (liftA2 $
                                liftA2 $ \mau mbu ->
                                    (>>) (concatFstMFunction @tta @ttb @IO mau) (concatSndMFunction @tta @ttb @IO mbu))
                               (concatFstMFunction @tta @ttb @IO $ editA eas)
                               (concatSndMFunction @tta @ttb @IO $ editB ebs)
                    in MkAnObject readAB editAB

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
