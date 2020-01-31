module Truth.Core.Types.Pair where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Types.None
import Truth.Core.Types.Partial
import Truth.Core.Types.ReadOnly
import Truth.Core.Types.Tuple
import Truth.Core.Types.Whole

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
fstLiftEditLens (MkEditLens g u pe) = let
    elGet :: ReadFunction (PairUpdateReader updateA updateX) (PairUpdateReader updateB updateX)
    elGet mr (MkTupleUpdateReader SelectFirst rt) = g (firstReadFunction mr) rt
    elGet mr (MkTupleUpdateReader SelectSecond rt) = mr (MkTupleUpdateReader SelectSecond rt)
    elUpdate ::
           forall m. MonadIO m
        => PairUpdate updateA updateX
        -> MutableRead m (PairUpdateReader updateA updateX)
        -> m [PairUpdate updateB updateX]
    elUpdate (MkTupleUpdate SelectFirst updateA) mr = do
        ebs <- u updateA $ firstReadFunction mr
        return $ fmap (MkTupleUpdate SelectFirst) ebs
    elUpdate (MkTupleUpdate SelectSecond ex) _ = return [MkTupleUpdate SelectSecond ex]
    elPutEdits ::
           forall m. MonadIO m
        => [PairUpdateEdit updateB updateX]
        -> MutableRead m (PairUpdateReader updateA updateX)
        -> m (Maybe [PairUpdateEdit updateA updateX])
    elPutEdits edits mr =
        case partitionPairEdits edits of
            (ebs, exs) ->
                getComposeM $ do
                    eas <- MkComposeM $ pe ebs $ firstReadFunction mr
                    return $ (fmap (MkTupleUpdateEdit SelectFirst) eas) ++ (fmap (MkTupleUpdateEdit SelectSecond) exs)
    in MkEditLens {..}

sndLiftEditLens ::
       forall updateX updateA updateB.
       EditLens updateA updateB
    -> EditLens (PairUpdate updateX updateA) (PairUpdate updateX updateB)
sndLiftEditLens (MkEditLens g u pe) = let
    elGet :: ReadFunction (PairUpdateReader updateX updateA) (PairUpdateReader updateX updateB)
    elGet mr (MkTupleUpdateReader SelectFirst rt) = mr (MkTupleUpdateReader SelectFirst rt)
    elGet mr (MkTupleUpdateReader SelectSecond rt) = g (secondReadFunction mr) rt
    elUpdate ::
           forall m. MonadIO m
        => PairUpdate updateX updateA
        -> MutableRead m (PairUpdateReader updateX updateA)
        -> m [PairUpdate updateX updateB]
    elUpdate (MkTupleUpdate SelectFirst ex) _ = return [MkTupleUpdate SelectFirst ex]
    elUpdate (MkTupleUpdate SelectSecond updateA) mr = do
        ebs <- u updateA $ secondReadFunction mr
        return $ fmap (MkTupleUpdate SelectSecond) ebs
    elPutEdits ::
           forall m. MonadIO m
        => [PairUpdateEdit updateX updateB]
        -> MutableRead m (PairUpdateReader updateX updateA)
        -> m (Maybe [PairUpdateEdit updateX updateA])
    elPutEdits edits mr =
        case partitionPairEdits edits of
            (exs, ebs) ->
                getComposeM $ do
                    eas <- MkComposeM $ pe ebs $ secondReadFunction mr
                    return $ (fmap (MkTupleUpdateEdit SelectFirst) exs) ++ (fmap (MkTupleUpdateEdit SelectSecond) eas)
    in MkEditLens {..}

pairCombineEditLenses ::
       forall updateA updateB1 updateB2.
       EditLens updateA updateB1
    -> EditLens updateA updateB2
    -> EditLens updateA (PairUpdate updateB1 updateB2)
pairCombineEditLenses (MkEditLens g1 u1 pe1) (MkEditLens g2 u2 pe2) = let
    g12 :: ReadFunction (UpdateReader updateA) (PairUpdateReader updateB1 updateB2)
    g12 mr (MkTupleUpdateReader SelectFirst rt) = g1 mr rt
    g12 mr (MkTupleUpdateReader SelectSecond rt) = g2 mr rt
    u12 :: forall m. MonadIO m
        => updateA
        -> MutableRead m (UpdateReader updateA)
        -> m [PairUpdate updateB1 updateB2]
    u12 updateA mr = do
        eb1s <- u1 updateA mr
        eb2s <- u2 updateA mr
        return $ fmap (MkTupleUpdate SelectFirst) eb1s ++ fmap (MkTupleUpdate SelectSecond) eb2s
    pe12 ::
           forall m. MonadIO m
        => [PairUpdateEdit updateB1 updateB2]
        -> MutableRead m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    pe12 edits mr =
        case partitionPairEdits edits of
            (eb1, eb2) ->
                getComposeM $ do
                    ea1 <- MkComposeM $ pe1 eb1 mr
                    ea2 <- MkComposeM $ pe2 eb2 mr
                    return $ ea1 ++ ea2
    in MkEditLens g12 u12 pe12

pairCombineFloatingEditLenses ::
       forall updateA updateB1 updateB2.
       FloatingEditLens updateA updateB1
    -> FloatingEditLens updateA updateB2
    -> FloatingEditLens updateA (PairUpdate updateB1 updateB2)
pairCombineFloatingEditLenses (MkFloatingEditLens NoFloatInit rlens1) (MkFloatingEditLens NoFloatInit rlens2) =
    editLensToFloating $ pairCombineEditLenses (rlens1 ()) (rlens2 ())
pairCombineFloatingEditLenses (MkFloatingEditLens (init1 :: FloatInit (UpdateReader updateA) r1) rlens1) (MkFloatingEditLens (init2 :: FloatInit (UpdateReader updateA) r2) rlens2) = let
    init12 :: FloatInit (UpdateReader updateA) (r1, r2)
    init12 =
        ReadFloatInit $ \mr -> do
            r1 <- runFloatInit init1 mr
            r2 <- runFloatInit init2 mr
            return (r1, r2)
    rlens12 :: (r1, r2) -> EditLens updateA (PairUpdate updateB1 updateB2)
    rlens12 (r1, r2) = pairCombineEditLenses (rlens1 r1) (rlens2 r2)
    in MkFloatingEditLens init12 rlens12

partialPairEditLens ::
       forall updateA updateB.
       EditLens (PairUpdate (PartialUpdate updateA) (PartialUpdate updateB)) (PartialUpdate (PairUpdate updateA updateB))
partialPairEditLens = let
    elGet ::
           forall m t. MonadIO m
        => MutableRead m (PairUpdateReader (PartialUpdate updateA) (PartialUpdate updateB))
        -> PairUpdateReader updateA updateB t
        -> m t
    elGet mr (MkTupleUpdateReader SelectFirst rt) = mr $ MkTupleUpdateReader SelectFirst rt
    elGet mr (MkTupleUpdateReader SelectSecond rt) = mr $ MkTupleUpdateReader SelectSecond rt
    elUpdate ::
           forall m. MonadIO m
        => PairUpdate (PartialUpdate updateA) (PartialUpdate updateB)
        -> MutableRead m (PairUpdateReader (PartialUpdate updateA) (PartialUpdate updateB))
        -> m [PartialUpdate (PairUpdate updateA updateB)]
    elUpdate (MkTupleUpdate SelectFirst (KnownPartialUpdate update)) _ =
        return [KnownPartialUpdate $ MkTupleUpdate SelectFirst update]
    elUpdate (MkTupleUpdate SelectFirst (UnknownPartialUpdate selset)) _ =
        return $
        pure $
        UnknownPartialUpdate $ \(MkTupleUpdateReader sel rt) ->
            case sel of
                SelectFirst -> selset rt
                SelectSecond -> False
    elUpdate (MkTupleUpdate SelectSecond (KnownPartialUpdate update)) _ =
        return [KnownPartialUpdate $ MkTupleUpdate SelectSecond update]
    elUpdate (MkTupleUpdate SelectSecond (UnknownPartialUpdate selset)) _ =
        return $
        pure $
        UnknownPartialUpdate $ \(MkTupleUpdateReader sel rt) ->
            case sel of
                SelectFirst -> False
                SelectSecond -> selset rt
    elPutEdits ::
           forall m. MonadIO m
        => [PairUpdateEdit updateA updateB]
        -> MutableRead m (PairUpdateReader (PartialUpdate updateA) (PartialUpdate updateB))
        -> m (Maybe [PairUpdateEdit (PartialUpdate updateA) (PartialUpdate updateB)])
    elPutEdits =
        elPutEditsFromSimplePutEdit $ \case
            MkTupleUpdateEdit SelectFirst edit -> return $ Just [MkTupleUpdateEdit SelectFirst edit]
            MkTupleUpdateEdit SelectSecond edit -> return $ Just [MkTupleUpdateEdit SelectSecond edit]
    in MkEditLens {..}

pairWholeEditLens :: forall a b. EditLens (PairUpdate (WholeUpdate a) (WholeUpdate b)) (WholeUpdate (a, b))
pairWholeEditLens = let
    elGet :: ReadFunction (PairUpdateReader (WholeUpdate a) (WholeUpdate b)) (WholeReader (a, b))
    elGet (mr :: MutableRead m _) ReadWhole = do
        a <- mr $ MkTupleUpdateReader SelectFirst ReadWhole
        b <- mr $ MkTupleUpdateReader SelectSecond ReadWhole
        return (a, b)
    elUpdate ::
           forall m. MonadIO m
        => PairUpdate (WholeUpdate a) (WholeUpdate b)
        -> MutableRead m (PairUpdateReader (WholeUpdate a) (WholeUpdate b))
        -> m [WholeUpdate (a, b)]
    elUpdate (MkTupleUpdate SelectFirst (MkWholeUpdate a)) mr = do
        b <- mr $ MkTupleUpdateReader SelectSecond ReadWhole
        return [MkWholeUpdate (a, b)]
    elUpdate (MkTupleUpdate SelectSecond (MkWholeUpdate b)) mr = do
        a <- mr $ MkTupleUpdateReader SelectFirst ReadWhole
        return [MkWholeUpdate (a, b)]
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (a, b)]
        -> MutableRead m (PairUpdateReader (WholeUpdate a) (WholeUpdate b))
        -> m (Maybe [PairUpdateEdit (WholeUpdate a) (WholeUpdate b)])
    elPutEdits =
        elPutEditsFromSimplePutEdit $ \(MkWholeReaderEdit (a, b)) ->
            return $
            Just $
            [MkTupleUpdateEdit SelectFirst $ MkWholeReaderEdit a, MkTupleUpdateEdit SelectSecond $ MkWholeReaderEdit b]
    in MkEditLens {..}

readOnlyPairEditLens ::
       forall updateA updateB.
       EditLens (PairUpdate (ReadOnlyUpdate updateA) (ReadOnlyUpdate updateB)) (ReadOnlyUpdate (PairUpdate updateA updateB))
readOnlyPairEditLens = let
    elGet ::
           ReadFunction (PairUpdateReader (ReadOnlyUpdate updateA) (ReadOnlyUpdate updateB)) (PairUpdateReader updateA updateB)
    elGet mr (MkTupleUpdateReader SelectFirst rt) = mr $ MkTupleUpdateReader SelectFirst rt
    elGet mr (MkTupleUpdateReader SelectSecond rt) = mr $ MkTupleUpdateReader SelectSecond rt
    elUpdate ::
           forall m. MonadIO m
        => PairUpdate (ReadOnlyUpdate updateA) (ReadOnlyUpdate updateB)
        -> MutableRead m (PairUpdateReader (ReadOnlyUpdate updateA) (ReadOnlyUpdate updateB))
        -> m [ReadOnlyUpdate (PairUpdate updateA updateB)]
    elUpdate (MkTupleUpdate SelectFirst (MkReadOnlyUpdate update)) _ =
        return [MkReadOnlyUpdate $ MkTupleUpdate SelectFirst update]
    elUpdate (MkTupleUpdate SelectSecond (MkReadOnlyUpdate update)) _ =
        return [MkReadOnlyUpdate $ MkTupleUpdate SelectSecond update]
    in MkEditLens {elPutEdits = elPutEditsNone, ..}
