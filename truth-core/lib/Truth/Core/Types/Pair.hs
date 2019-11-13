module Truth.Core.Types.Pair where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Resource
import Truth.Core.Types.Tuple

data PairSelector (updateA :: Type) (updateB :: Type) (update :: Type) where
    SelectFirst :: PairSelector updateA updateB updateA
    SelectSecond :: PairSelector updateA updateB updateB

instance (c updateA, c updateB) => WitnessConstraint c (PairSelector updateA updateB) where
    witnessConstraint SelectFirst = Dict
    witnessConstraint SelectSecond = Dict

instance Show (PairSelector updateA updateB et) where
    show SelectFirst = "first"
    show SelectSecond = "second"

instance AllWitnessConstraint Show (PairSelector updateA updateB) where
    allWitnessConstraint = Dict

type PairUpdateReader updateA updateB = TupleUpdateReader (PairSelector updateA updateB)

type PairUpdateEdit updateA updateB = TupleUpdateEdit (PairSelector updateA updateB)

type PairUpdate updateA updateB = TupleUpdate (PairSelector updateA updateB)

firstReadFunction :: ReadFunction (PairUpdateReader updateA updateB) (UpdateReader updateA)
firstReadFunction = tupleReadFunction SelectFirst

secondReadFunction :: ReadFunction (PairUpdateReader updateA updateB) (UpdateReader updateB)
secondReadFunction = tupleReadFunction SelectSecond

instance TestEquality (PairSelector updateA updateB) where
    testEquality SelectFirst SelectFirst = Just Refl
    testEquality SelectSecond SelectSecond = Just Refl
    testEquality _ _ = Nothing

instance (SubjectReader (UpdateReader updateA), SubjectReader (UpdateReader updateB)) =>
             SubjectTupleSelector (PairSelector updateA updateB) where
    type TupleSubject (PairSelector updateA updateB) = (UpdateSubject updateA, UpdateSubject updateB)
    tupleReadFromSubject SelectFirst (a, _b) = a
    tupleReadFromSubject SelectSecond (_a, b) = b
    tupleWriteToSubject SelectFirst a (_, b) = (a, b)
    tupleWriteToSubject SelectSecond b (a, _) = (a, b)

instance FiniteTupleSelector (PairSelector updateA updateB) where
    tupleConstruct f = (,) <$> f SelectFirst <*> f SelectSecond

instance (c (UpdateReader updateA), c (UpdateReader updateB)) => TupleReaderWitness c (PairSelector updateA updateB) where
    tupleReaderWitness SelectFirst = Dict
    tupleReaderWitness SelectSecond = Dict

instance (c (UpdateEdit updateA), c (UpdateEdit updateB)) => TupleEditWitness c (PairSelector updateA updateB) where
    tupleEditWitness SelectFirst = Dict
    tupleEditWitness SelectSecond = Dict

instance (c updateA, c updateB) => TupleUpdateWitness c (PairSelector updateA updateB) where
    tupleUpdateWitness SelectFirst = Dict
    tupleUpdateWitness SelectSecond = Dict

instance IsFiniteConsWitness (PairSelector updateA updateB) where
    type FiniteConsWitness (PairSelector updateA updateB) = '[ updateA, updateB]
    toLTW SelectFirst = FirstElementType
    toLTW SelectSecond = RestElementType FirstElementType
    fromLTW FirstElementType = SelectFirst
    fromLTW (RestElementType FirstElementType) = SelectSecond
    fromLTW (RestElementType (RestElementType lt)) = never lt

partitionPairEdits ::
       forall updateA updateB. [PairUpdateEdit updateA updateB] -> ([UpdateEdit updateA], [UpdateEdit updateB])
partitionPairEdits pes = let
    toEither :: PairUpdateEdit updateA updateB -> Either (UpdateEdit updateA) (UpdateEdit updateB)
    toEither (MkTupleUpdateEdit SelectFirst updateA) = Left updateA
    toEither (MkTupleUpdateEdit SelectSecond updateB) = Right updateB
    in partitionEithers $ fmap toEither pes

pairMutableRead ::
       MutableRead m (UpdateReader updateA)
    -> MutableRead m (UpdateReader updateB)
    -> MutableRead m (PairUpdateReader updateA updateB)
pairMutableRead mra _mrb (MkTupleUpdateReader SelectFirst ra) = mra ra
pairMutableRead _mra mrb (MkTupleUpdateReader SelectSecond rb) = mrb rb

fstMutableRead :: MutableRead m (PairUpdateReader updateA updateB) -> MutableRead m (UpdateReader updateA)
fstMutableRead mr ra = mr $ MkTupleUpdateReader SelectFirst ra

sndMutableRead :: MutableRead m (PairUpdateReader updateA updateB) -> MutableRead m (UpdateReader updateB)
sndMutableRead mr rb = mr $ MkTupleUpdateReader SelectSecond rb

fstLiftEditLens ::
       forall updateX updateA updateB.
       EditLens updateA updateB
    -> EditLens (PairUpdate updateA updateX) (PairUpdate updateB updateX)
fstLiftEditLens (MkRunnable2 (trun :: TransStackRunner tt) (MkAnEditLens (MkAnUpdateFunction g u) pe)) =
    case transStackRunnerUnliftAllDict trun of
        Dict -> let
            ufGet :: ReadFunctionTT tt (PairUpdateReader updateA updateX) (PairUpdateReader updateB updateX)
            ufGet mr (MkTupleUpdateReader SelectFirst rt) = g (firstReadFunction mr) rt
            ufGet mr (MkTupleUpdateReader SelectSecond rt) = stackLift @tt $ mr (MkTupleUpdateReader SelectSecond rt)
            ufUpdate ::
                   forall m. MonadIO m
                => PairUpdate updateA updateX
                -> MutableRead m (PairUpdateReader updateA updateX)
                -> ApplyStack tt m [PairUpdate updateB updateX]
            ufUpdate (MkTupleUpdate SelectFirst updateA) mr =
                case transStackDict @MonadIO @tt @m of
                    Dict -> do
                        ebs <- u updateA $ firstReadFunction mr
                        return $ fmap (MkTupleUpdate SelectFirst) ebs
            ufUpdate (MkTupleUpdate SelectSecond ex) _ =
                case transStackDict @MonadIO @tt @m of
                    Dict -> return [MkTupleUpdate SelectSecond ex]
            elFunction :: AnUpdateFunction tt (PairUpdate updateA updateX) (PairUpdate updateB updateX)
            elFunction = MkAnUpdateFunction {..}
            elPutEdits ::
                   forall m. MonadIO m
                => [PairUpdateEdit updateB updateX]
                -> MutableRead m (PairUpdateReader updateA updateX)
                -> ApplyStack tt m (Maybe [PairUpdateEdit updateA updateX])
            elPutEdits edits mr =
                case transStackDict @MonadIO @tt @m of
                    Dict ->
                        case partitionPairEdits edits of
                            (ebs, exs) ->
                                getComposeM $ do
                                    eas <- MkComposeM $ pe ebs $ firstReadFunction mr
                                    return $
                                        (fmap (MkTupleUpdateEdit SelectFirst) eas) ++
                                        (fmap (MkTupleUpdateEdit SelectSecond) exs)
            in MkRunnable2 trun $ MkAnEditLens {..}

sndLiftEditLens ::
       forall updateX updateA updateB.
       EditLens updateA updateB
    -> EditLens (PairUpdate updateX updateA) (PairUpdate updateX updateB)
sndLiftEditLens (MkRunnable2 (trun :: TransStackRunner tt) (MkAnEditLens (MkAnUpdateFunction g u) pe)) =
    case transStackRunnerUnliftAllDict trun of
        Dict -> let
            ufGet :: ReadFunctionTT tt (PairUpdateReader updateX updateA) (PairUpdateReader updateX updateB)
            ufGet mr (MkTupleUpdateReader SelectFirst rt) = stackLift @tt $ mr (MkTupleUpdateReader SelectFirst rt)
            ufGet mr (MkTupleUpdateReader SelectSecond rt) = g (secondReadFunction mr) rt
            ufUpdate ::
                   forall m. MonadIO m
                => PairUpdate updateX updateA
                -> MutableRead m (PairUpdateReader updateX updateA)
                -> ApplyStack tt m [PairUpdate updateX updateB]
            ufUpdate (MkTupleUpdate SelectFirst ex) _ =
                case transStackDict @MonadIO @tt @m of
                    Dict -> return [MkTupleUpdate SelectFirst ex]
            ufUpdate (MkTupleUpdate SelectSecond updateA) mr =
                case transStackDict @MonadIO @tt @m of
                    Dict -> do
                        ebs <- u updateA $ secondReadFunction mr
                        return $ fmap (MkTupleUpdate SelectSecond) ebs
            elFunction :: AnUpdateFunction tt (PairUpdate updateX updateA) (PairUpdate updateX updateB)
            elFunction = MkAnUpdateFunction {..}
            elPutEdits ::
                   forall m. MonadIO m
                => [PairUpdateEdit updateX updateB]
                -> MutableRead m (PairUpdateReader updateX updateA)
                -> ApplyStack tt m (Maybe [PairUpdateEdit updateX updateA])
            elPutEdits edits mr =
                case transStackDict @MonadIO @tt @m of
                    Dict ->
                        case partitionPairEdits edits of
                            (exs, ebs) ->
                                getComposeM $ do
                                    eas <- MkComposeM $ pe ebs $ secondReadFunction mr
                                    return $
                                        (fmap (MkTupleUpdateEdit SelectFirst) exs) ++
                                        (fmap (MkTupleUpdateEdit SelectSecond) eas)
            in MkRunnable2 trun $ MkAnEditLens {..}

pairCombineAnUpdateFunctions ::
       forall tt updateA updateB1 updateB2. MonadTransStackUnliftAll tt
    => AnUpdateFunction tt updateA updateB1
    -> AnUpdateFunction tt updateA updateB2
    -> AnUpdateFunction tt updateA (PairUpdate updateB1 updateB2)
pairCombineAnUpdateFunctions (MkAnUpdateFunction g1 u1) (MkAnUpdateFunction g2 u2) = let
    g12 :: ReadFunctionTT tt (UpdateReader updateA) (PairUpdateReader updateB1 updateB2)
    g12 mr (MkTupleUpdateReader SelectFirst rt) = g1 mr rt
    g12 mr (MkTupleUpdateReader SelectSecond rt) = g2 mr rt
    u12 :: forall m. MonadIO m
        => updateA
        -> MutableRead m (UpdateReader updateA)
        -> ApplyStack tt m [PairUpdate updateB1 updateB2]
    u12 updateA mr =
        case transStackDict @MonadIO @tt @m of
            Dict -> do
                eb1s <- u1 updateA mr
                eb2s <- u2 updateA mr
                return $ fmap (MkTupleUpdate SelectFirst) eb1s ++ fmap (MkTupleUpdate SelectSecond) eb2s
    in MkAnUpdateFunction g12 u12

pairCombineUpdateFunctions ::
       forall updateA updateB1 updateB2.
       UpdateFunction updateA updateB1
    -> UpdateFunction updateA updateB2
    -> UpdateFunction updateA (PairUpdate updateB1 updateB2)
pairCombineUpdateFunctions = joinRunnable2Maps pairCombineAnUpdateFunctions

pairCombineEditLenses ::
       forall updateA updateB1 updateB2.
       EditLens updateA updateB1
    -> EditLens updateA updateB2
    -> EditLens updateA (PairUpdate updateB1 updateB2)
pairCombineEditLenses =
    joinRunnable2Maps $ \(MkAnEditLens af1 pe1 :: AnEditLens tt updateA updateB1) (MkAnEditLens af2 pe2) -> let
        af12 = pairCombineAnUpdateFunctions af1 af2
        pe12 ::
               forall m. MonadIO m
            => [PairUpdateEdit updateB1 updateB2]
            -> MutableRead m (UpdateReader updateA)
            -> ApplyStack tt m (Maybe [UpdateEdit updateA])
        pe12 edits mr =
            case transStackDict @MonadIO @tt @m of
                Dict ->
                    case partitionPairEdits edits of
                        (eb1, eb2) ->
                            getComposeM $ do
                                ea1 <- MkComposeM $ pe1 eb1 mr
                                ea2 <- MkComposeM $ pe2 eb2 mr
                                return $ ea1 ++ ea2
        in MkAnEditLens af12 pe12

partialPairEditLens ::
       forall updateA updateB.
       EditLens (PairUpdate (PartialUpdate updateA) (PartialUpdate updateB)) (PartialUpdate (PairUpdate updateA updateB))
partialPairEditLens = let
    ufGet ::
           forall m t. MonadIO m
        => MutableRead m (PairUpdateReader (PartialUpdate updateA) (PartialUpdate updateB))
        -> PairUpdateReader updateA updateB t
        -> m t
    ufGet mr (MkTupleUpdateReader SelectFirst rt) = mr $ MkTupleUpdateReader SelectFirst rt
    ufGet mr (MkTupleUpdateReader SelectSecond rt) = mr $ MkTupleUpdateReader SelectSecond rt
    ufUpdate ::
           forall m. MonadIO m
        => PairUpdate (PartialUpdate updateA) (PartialUpdate updateB)
        -> MutableRead m (PairUpdateReader (PartialUpdate updateA) (PartialUpdate updateB))
        -> m [PartialUpdate (PairUpdate updateA updateB)]
    ufUpdate (MkTupleUpdate SelectFirst (KnownPartialUpdate update)) _ =
        return [KnownPartialUpdate $ MkTupleUpdate SelectFirst update]
    ufUpdate (MkTupleUpdate SelectFirst (UnknownPartialUpdate selset)) _ =
        return $
        pure $
        UnknownPartialUpdate $ \(MkTupleUpdateReader sel rt) ->
            case sel of
                SelectFirst -> selset rt
                SelectSecond -> False
    ufUpdate (MkTupleUpdate SelectSecond (KnownPartialUpdate update)) _ =
        return [KnownPartialUpdate $ MkTupleUpdate SelectSecond update]
    ufUpdate (MkTupleUpdate SelectSecond (UnknownPartialUpdate selset)) _ =
        return $
        pure $
        UnknownPartialUpdate $ \(MkTupleUpdateReader sel rt) ->
            case sel of
                SelectFirst -> False
                SelectSecond -> selset rt
    elFunction ::
           AnUpdateFunction '[] (PairUpdate (PartialUpdate updateA) (PartialUpdate updateB)) (PartialUpdate (PairUpdate updateA updateB))
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [PairUpdateEdit updateA updateB]
        -> MutableRead m (PairUpdateReader (PartialUpdate updateA) (PartialUpdate updateB))
        -> m (Maybe [PairUpdateEdit (PartialUpdate updateA) (PartialUpdate updateB)])
    elPutEdits =
        elPutEditsFromSimplePutEdit @'[] $ \case
            MkTupleUpdateEdit SelectFirst edit -> return $ Just [MkTupleUpdateEdit SelectFirst edit]
            MkTupleUpdateEdit SelectSecond edit -> return $ Just [MkTupleUpdateEdit SelectSecond edit]
    in MkRunnable2 cmEmpty MkAnEditLens {..}
