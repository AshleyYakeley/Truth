module Changes.Core.Types.Tuple.Pair where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.None
import Changes.Core.Types.Partial
import Changes.Core.Types.ReadOnly
import Changes.Core.Types.Tuple.Tuple
import Changes.Core.Types.Whole

data PairSelector (updateA :: Type) (updateB :: Type) (update :: Type) where
    SelectFirst :: PairSelector updateA updateB updateA
    SelectSecond :: PairSelector updateA updateB updateB

instance (c updateA, c updateB) => WitnessConstraint c (PairSelector updateA updateB) where
    witnessConstraint SelectFirst = Dict
    witnessConstraint SelectSecond = Dict

instance Show (PairSelector updateA updateB et) where
    show SelectFirst = "first"
    show SelectSecond = "second"

instance AllConstraint Show (PairSelector updateA updateB) where
    allConstraint = Dict

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

instance ListElementWitness (PairSelector updateA updateB) where
    type WitnessTypeList (PairSelector updateA updateB) = '[ updateA, updateB]
    toListElementWitness SelectFirst = FirstElementType
    toListElementWitness SelectSecond = RestElementType FirstElementType
    fromListElementWitness FirstElementType = SelectFirst
    fromListElementWitness (RestElementType FirstElementType) = SelectSecond
    fromListElementWitness (RestElementType (RestElementType lt)) = never lt

partitionPairEdits ::
       forall updateA updateB. [PairUpdateEdit updateA updateB] -> ([UpdateEdit updateA], [UpdateEdit updateB])
partitionPairEdits pes = let
    toEither :: PairUpdateEdit updateA updateB -> Either (UpdateEdit updateA) (UpdateEdit updateB)
    toEither (MkTupleUpdateEdit SelectFirst updateA) = Left updateA
    toEither (MkTupleUpdateEdit SelectSecond updateB) = Right updateB
    in partitionEithers $ fmap toEither pes

pairReadable ::
       Readable m (UpdateReader updateA)
    -> Readable m (UpdateReader updateB)
    -> Readable m (PairUpdateReader updateA updateB)
pairReadable mra _mrb (MkTupleUpdateReader SelectFirst ra) = mra ra
pairReadable _mra mrb (MkTupleUpdateReader SelectSecond rb) = mrb rb

fstReadable :: Readable m (PairUpdateReader updateA updateB) -> Readable m (UpdateReader updateA)
fstReadable mr ra = mr $ MkTupleUpdateReader SelectFirst ra

sndReadable :: Readable m (PairUpdateReader updateA updateB) -> Readable m (UpdateReader updateB)
sndReadable mr rb = mr $ MkTupleUpdateReader SelectSecond rb

fstLiftChangeLens ::
       forall updateX updateA updateB.
       ChangeLens updateA updateB
    -> ChangeLens (PairUpdate updateA updateX) (PairUpdate updateB updateX)
fstLiftChangeLens (MkChangeLens g u pe) = let
    clRead :: ReadFunction (PairUpdateReader updateA updateX) (PairUpdateReader updateB updateX)
    clRead mr (MkTupleUpdateReader SelectFirst rt) = g (firstReadFunction mr) rt
    clRead mr (MkTupleUpdateReader SelectSecond rt) = mr (MkTupleUpdateReader SelectSecond rt)
    clUpdate ::
           forall m. MonadIO m
        => PairUpdate updateA updateX
        -> Readable m (PairUpdateReader updateA updateX)
        -> m [PairUpdate updateB updateX]
    clUpdate (MkTupleUpdate SelectFirst updateA) mr = do
        ebs <- u updateA $ firstReadFunction mr
        return $ fmap (MkTupleUpdate SelectFirst) ebs
    clUpdate (MkTupleUpdate SelectSecond ex) _ = return [MkTupleUpdate SelectSecond ex]
    clPutEdits ::
           forall m. MonadIO m
        => [PairUpdateEdit updateB updateX]
        -> Readable m (PairUpdateReader updateA updateX)
        -> m (Maybe [PairUpdateEdit updateA updateX])
    clPutEdits edits mr =
        case partitionPairEdits edits of
            (ebs, exs) ->
                unComposeInner $ do
                    eas <- MkComposeInner $ pe ebs $ firstReadFunction mr
                    return $ (fmap (MkTupleUpdateEdit SelectFirst) eas) ++ (fmap (MkTupleUpdateEdit SelectSecond) exs)
    in MkChangeLens {..}

sndLiftChangeLens ::
       forall updateX updateA updateB.
       ChangeLens updateA updateB
    -> ChangeLens (PairUpdate updateX updateA) (PairUpdate updateX updateB)
sndLiftChangeLens (MkChangeLens g u pe) = let
    clRead :: ReadFunction (PairUpdateReader updateX updateA) (PairUpdateReader updateX updateB)
    clRead mr (MkTupleUpdateReader SelectFirst rt) = mr (MkTupleUpdateReader SelectFirst rt)
    clRead mr (MkTupleUpdateReader SelectSecond rt) = g (secondReadFunction mr) rt
    clUpdate ::
           forall m. MonadIO m
        => PairUpdate updateX updateA
        -> Readable m (PairUpdateReader updateX updateA)
        -> m [PairUpdate updateX updateB]
    clUpdate (MkTupleUpdate SelectFirst ex) _ = return [MkTupleUpdate SelectFirst ex]
    clUpdate (MkTupleUpdate SelectSecond updateA) mr = do
        ebs <- u updateA $ secondReadFunction mr
        return $ fmap (MkTupleUpdate SelectSecond) ebs
    clPutEdits ::
           forall m. MonadIO m
        => [PairUpdateEdit updateX updateB]
        -> Readable m (PairUpdateReader updateX updateA)
        -> m (Maybe [PairUpdateEdit updateX updateA])
    clPutEdits edits mr =
        case partitionPairEdits edits of
            (exs, ebs) ->
                unComposeInner $ do
                    eas <- MkComposeInner $ pe ebs $ secondReadFunction mr
                    return $ (fmap (MkTupleUpdateEdit SelectFirst) exs) ++ (fmap (MkTupleUpdateEdit SelectSecond) eas)
    in MkChangeLens {..}

pairCombineChangeLenses ::
       forall updateA updateB1 updateB2.
       ChangeLens updateA updateB1
    -> ChangeLens updateA updateB2
    -> ChangeLens updateA (PairUpdate updateB1 updateB2)
pairCombineChangeLenses (MkChangeLens g1 u1 pe1) (MkChangeLens g2 u2 pe2) = let
    g12 :: ReadFunction (UpdateReader updateA) (PairUpdateReader updateB1 updateB2)
    g12 mr (MkTupleUpdateReader SelectFirst rt) = g1 mr rt
    g12 mr (MkTupleUpdateReader SelectSecond rt) = g2 mr rt
    u12 :: forall m. MonadIO m
        => updateA
        -> Readable m (UpdateReader updateA)
        -> m [PairUpdate updateB1 updateB2]
    u12 updateA mr = do
        eb1s <- u1 updateA mr
        eb2s <- u2 updateA mr
        return $ fmap (MkTupleUpdate SelectFirst) eb1s ++ fmap (MkTupleUpdate SelectSecond) eb2s
    pe12 ::
           forall m. MonadIO m
        => [PairUpdateEdit updateB1 updateB2]
        -> Readable m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    pe12 edits mr =
        case partitionPairEdits edits of
            (eb1, eb2) ->
                unComposeInner $ do
                    ea1 <- MkComposeInner $ pe1 eb1 mr
                    ea2 <- MkComposeInner $ pe2 eb2 mr
                    return $ ea1 ++ ea2
    in MkChangeLens g12 u12 pe12

pairCombineFloatingChangeLenses ::
       forall updateA updateB1 updateB2.
       FloatingChangeLens updateA updateB1
    -> FloatingChangeLens updateA updateB2
    -> FloatingChangeLens updateA (PairUpdate updateB1 updateB2)
pairCombineFloatingChangeLenses (MkFloatingChangeLens (NoFloatInit r1) rlens1) (MkFloatingChangeLens (NoFloatInit r2) rlens2) =
    changeLensToFloating $ pairCombineChangeLenses (rlens1 r1) (rlens2 r2)
pairCombineFloatingChangeLenses (MkFloatingChangeLens (init1 :: FloatInit (UpdateReader updateA) r1) rlens1) (MkFloatingChangeLens (init2 :: FloatInit (UpdateReader updateA) r2) rlens2) = let
    init12 :: FloatInit (UpdateReader updateA) (r1, r2)
    init12 =
        ReadFloatInit $ \mr -> do
            r1 <- runFloatInit init1 mr
            r2 <- runFloatInit init2 mr
            return (r1, r2)
    rlens12 :: (r1, r2) -> ChangeLens updateA (PairUpdate updateB1 updateB2)
    rlens12 (r1, r2) = pairCombineChangeLenses (rlens1 r1) (rlens2 r2)
    in MkFloatingChangeLens init12 rlens12

partialPairChangeLens ::
       forall updateA updateB.
       ChangeLens (PairUpdate (PartialUpdate updateA) (PartialUpdate updateB)) (PartialUpdate (PairUpdate updateA updateB))
partialPairChangeLens = let
    clRead ::
           ReadFunction (PairUpdateReader (PartialUpdate updateA) (PartialUpdate updateB)) (PairUpdateReader updateA updateB)
    clRead mr (MkTupleUpdateReader SelectFirst rt) = mr $ MkTupleUpdateReader SelectFirst rt
    clRead mr (MkTupleUpdateReader SelectSecond rt) = mr $ MkTupleUpdateReader SelectSecond rt
    clUpdate ::
           forall m. MonadIO m
        => PairUpdate (PartialUpdate updateA) (PartialUpdate updateB)
        -> Readable m (PairUpdateReader (PartialUpdate updateA) (PartialUpdate updateB))
        -> m [PartialUpdate (PairUpdate updateA updateB)]
    clUpdate (MkTupleUpdate SelectFirst (KnownPartialUpdate update)) _ =
        return [KnownPartialUpdate $ MkTupleUpdate SelectFirst update]
    clUpdate (MkTupleUpdate SelectFirst (UnknownPartialUpdate selset)) _ =
        return $
        pure $
        UnknownPartialUpdate $ \(MkTupleUpdateReader sel rt) ->
            case sel of
                SelectFirst -> selset rt
                SelectSecond -> False
    clUpdate (MkTupleUpdate SelectSecond (KnownPartialUpdate update)) _ =
        return [KnownPartialUpdate $ MkTupleUpdate SelectSecond update]
    clUpdate (MkTupleUpdate SelectSecond (UnknownPartialUpdate selset)) _ =
        return $
        pure $
        UnknownPartialUpdate $ \(MkTupleUpdateReader sel rt) ->
            case sel of
                SelectFirst -> False
                SelectSecond -> selset rt
    clPutEdits ::
           forall m. MonadIO m
        => [PairUpdateEdit updateA updateB]
        -> Readable m (PairUpdateReader (PartialUpdate updateA) (PartialUpdate updateB))
        -> m (Maybe [PairUpdateEdit (PartialUpdate updateA) (PartialUpdate updateB)])
    clPutEdits =
        clPutEditsFromSimplePutEdit $ \case
            MkTupleUpdateEdit SelectFirst edit -> return $ Just [MkTupleUpdateEdit SelectFirst edit]
            MkTupleUpdateEdit SelectSecond edit -> return $ Just [MkTupleUpdateEdit SelectSecond edit]
    in MkChangeLens {..}

pairWholeChangeLens :: forall a b. ChangeLens (PairUpdate (WholeUpdate a) (WholeUpdate b)) (WholeUpdate (a, b))
pairWholeChangeLens = let
    clRead :: ReadFunction (PairUpdateReader (WholeUpdate a) (WholeUpdate b)) (WholeReader (a, b))
    clRead (mr :: Readable m _) ReadWhole = do
        a <- mr $ MkTupleUpdateReader SelectFirst ReadWhole
        b <- mr $ MkTupleUpdateReader SelectSecond ReadWhole
        return (a, b)
    clUpdate ::
           forall m. MonadIO m
        => PairUpdate (WholeUpdate a) (WholeUpdate b)
        -> Readable m (PairUpdateReader (WholeUpdate a) (WholeUpdate b))
        -> m [WholeUpdate (a, b)]
    clUpdate (MkTupleUpdate SelectFirst (MkWholeUpdate a)) mr = do
        b <- mr $ MkTupleUpdateReader SelectSecond ReadWhole
        return [MkWholeUpdate (a, b)]
    clUpdate (MkTupleUpdate SelectSecond (MkWholeUpdate b)) mr = do
        a <- mr $ MkTupleUpdateReader SelectFirst ReadWhole
        return [MkWholeUpdate (a, b)]
    clPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (a, b)]
        -> Readable m (PairUpdateReader (WholeUpdate a) (WholeUpdate b))
        -> m (Maybe [PairUpdateEdit (WholeUpdate a) (WholeUpdate b)])
    clPutEdits =
        clPutEditsFromSimplePutEdit $ \(MkWholeReaderEdit (a, b)) ->
            return $
            Just $
            [MkTupleUpdateEdit SelectFirst $ MkWholeReaderEdit a, MkTupleUpdateEdit SelectSecond $ MkWholeReaderEdit b]
    in MkChangeLens {..}

readOnlyPairChangeLens ::
       forall updateA updateB.
       ChangeLens (PairUpdate (ReadOnlyUpdate updateA) (ReadOnlyUpdate updateB)) (ReadOnlyUpdate (PairUpdate updateA updateB))
readOnlyPairChangeLens = let
    clRead ::
           ReadFunction (PairUpdateReader (ReadOnlyUpdate updateA) (ReadOnlyUpdate updateB)) (PairUpdateReader updateA updateB)
    clRead mr (MkTupleUpdateReader SelectFirst rt) = mr $ MkTupleUpdateReader SelectFirst rt
    clRead mr (MkTupleUpdateReader SelectSecond rt) = mr $ MkTupleUpdateReader SelectSecond rt
    clUpdate ::
           forall m. MonadIO m
        => PairUpdate (ReadOnlyUpdate updateA) (ReadOnlyUpdate updateB)
        -> Readable m (PairUpdateReader (ReadOnlyUpdate updateA) (ReadOnlyUpdate updateB))
        -> m [ReadOnlyUpdate (PairUpdate updateA updateB)]
    clUpdate (MkTupleUpdate SelectFirst (MkReadOnlyUpdate update)) _ =
        return [MkReadOnlyUpdate $ MkTupleUpdate SelectFirst update]
    clUpdate (MkTupleUpdate SelectSecond (MkReadOnlyUpdate update)) _ =
        return [MkReadOnlyUpdate $ MkTupleUpdate SelectSecond update]
    in MkChangeLens {clPutEdits = clPutEditsNone, ..}

bindChangeLens ::
       forall x updateA updateB. FullUpdate updateB
    => (x -> ChangeLens updateA updateB)
    -> ChangeLens (PairUpdate (ROWUpdate x) updateA) updateB
bindChangeLens xlens = let
    g :: ReadFunction (PairUpdateReader (ROWUpdate x) updateA) (UpdateReader updateB)
    g mra rb = do
        x <- firstReadFunction mra ReadWhole
        clRead (xlens x) (secondReadFunction mra) rb
    u :: forall m. MonadIO m
      => PairUpdate (ROWUpdate x) updateA
      -> Readable m (PairUpdateReader (ROWUpdate x) updateA)
      -> m [updateB]
    u (MkTupleUpdate SelectFirst (MkReadOnlyUpdate (MkWholeUpdate x))) mra =
        getReplaceUpdates $ clRead (xlens x) (secondReadFunction mra)
    u (MkTupleUpdate SelectSecond updateA) mra = do
        x <- firstReadFunction mra ReadWhole
        clUpdate (xlens x) updateA (secondReadFunction mra)
    pe :: forall m. MonadIO m
       => [UpdateEdit updateB]
       -> Readable m (PairUpdateReader (ROWUpdate x) updateA)
       -> m (Maybe [PairUpdateEdit (ROWUpdate x) updateA])
    pe editsB mra = do
        x <- firstReadFunction mra ReadWhole
        meditsA <- clPutEdits (xlens x) editsB (secondReadFunction mra)
        return $ fmap (fmap $ MkTupleUpdateEdit SelectSecond) meditsA
    in MkChangeLens g u pe
