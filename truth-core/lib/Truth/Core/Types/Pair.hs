module Truth.Core.Types.Pair where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Tuple

data PairSelector ea eb (et :: *) where
    SelectFirst :: PairSelector ea eb ea
    SelectSecond :: PairSelector ea eb eb

instance (c ea, c eb) => WitnessConstraint c (PairSelector ea eb) where
    witnessConstraint SelectFirst = Dict
    witnessConstraint SelectSecond = Dict

instance Show (PairSelector ea eb et) where
    show SelectFirst = "first"
    show SelectSecond = "second"

instance AllWitnessConstraint Show (PairSelector ea eb) where
    allWitnessConstraint = Dict

type PairEditReader ea eb = TupleEditReader (PairSelector ea eb)

type PairEdit ea eb = TupleEdit (PairSelector ea eb)

firstReadFunction :: ReadFunction (PairEditReader ea eb) (EditReader ea)
firstReadFunction = tupleReadFunction SelectFirst

secondReadFunction :: ReadFunction (PairEditReader ea eb) (EditReader eb)
secondReadFunction = tupleReadFunction SelectSecond

instance TestEquality (PairSelector ea eb) where
    testEquality SelectFirst SelectFirst = Just Refl
    testEquality SelectSecond SelectSecond = Just Refl
    testEquality _ _ = Nothing

instance (SubjectReader (EditReader ea), SubjectReader (EditReader eb)) =>
         SubjectTupleSelector (PairSelector ea eb) where
    type TupleSubject (PairSelector ea eb) = (EditSubject ea, EditSubject eb)
    tupleReadFromSubject SelectFirst (a, _b) = a
    tupleReadFromSubject SelectSecond (_a, b) = b

instance FiniteTupleSelector (PairSelector ea eb) where
    tupleConstruct f = (,) <$> f SelectFirst <*> f SelectSecond

instance (c (EditReader ea), c (EditReader eb)) => TupleReaderWitness c (PairSelector ea eb) where
    tupleReaderWitness SelectFirst = Dict
    tupleReaderWitness SelectSecond = Dict

instance (c ea, c eb) => TupleWitness c (PairSelector ea eb) where
    tupleWitness SelectFirst = Dict
    tupleWitness SelectSecond = Dict

instance IsFiniteConsWitness (PairSelector ea eb) where
    type FiniteConsWitness (PairSelector ea eb) = '[ ea, eb]
    toLTW SelectFirst = FirstListElementWitness
    toLTW SelectSecond = RestListElementWitness FirstListElementWitness
    fromLTW FirstListElementWitness = SelectFirst
    fromLTW (RestListElementWitness FirstListElementWitness) = SelectSecond
    fromLTW (RestListElementWitness (RestListElementWitness lt)) = never lt

partitionPairEdits :: forall ea eb. [PairEdit ea eb] -> ([ea], [eb])
partitionPairEdits pes = let
    toEither :: PairEdit ea eb -> Either ea eb
    toEither (MkTupleEdit SelectFirst ea) = Left ea
    toEither (MkTupleEdit SelectSecond eb) = Right eb
    in partitionEithers $ fmap toEither pes

pairMutableRead ::
       MutableRead m (EditReader ea) -> MutableRead m (EditReader eb) -> MutableRead m (PairEditReader ea eb)
pairMutableRead mra _mrb (MkTupleEditReader SelectFirst ra) = mra ra
pairMutableRead _mra mrb (MkTupleEditReader SelectSecond rb) = mrb rb

fstMutableRead :: MutableRead m (PairEditReader ea eb) -> MutableRead m (EditReader ea)
fstMutableRead mr ra = mr $ MkTupleEditReader SelectFirst ra

sndMutableRead :: MutableRead m (PairEditReader ea eb) -> MutableRead m (EditReader eb)
sndMutableRead mr rb = mr $ MkTupleEditReader SelectSecond rb

fstLiftEditLens ::
       forall editx edita editb. EditLens edita editb -> EditLens (PairEdit edita editx) (PairEdit editb editx)
fstLiftEditLens (MkCloseUnlift (unlift :: Unlift t) (MkAnEditLens (MkAnEditFunction g u) pe)) = let
    efGet :: ReadFunctionT t (PairEditReader edita editx) (PairEditReader editb editx)
    efGet mr (MkTupleEditReader SelectFirst rt) = g (firstReadFunction mr) rt
    efGet mr (MkTupleEditReader SelectSecond rt) = lift $ mr (MkTupleEditReader SelectSecond rt)
    efUpdate ::
           forall m. MonadIO m
        => PairEdit edita editx
        -> MutableRead m (EditReader (PairEdit edita editx))
        -> t m [PairEdit editb editx]
    efUpdate (MkTupleEdit SelectFirst ea) mr =
        withTransConstraintTM @MonadIO $ do
            ebs <- u ea $ firstReadFunction mr
            return $ fmap (MkTupleEdit SelectFirst) ebs
    efUpdate (MkTupleEdit SelectSecond ex) _ = withTransConstraintTM @MonadIO $ return [MkTupleEdit SelectSecond ex]
    elFunction :: AnEditFunction t (PairEdit edita editx) (PairEdit editb editx)
    elFunction = MkAnEditFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [PairEdit editb editx]
        -> MutableRead m (EditReader (PairEdit edita editx))
        -> t m (Maybe [PairEdit edita editx])
    elPutEdits edits mr =
        case partitionPairEdits edits of
            (ebs, exs) ->
                withTransConstraintTM @MonadIO $
                getComposeM $ do
                    eas <- MkComposeM $ pe ebs $ firstReadFunction mr
                    return $ (fmap (MkTupleEdit SelectFirst) eas) ++ (fmap (MkTupleEdit SelectSecond) exs)
    in MkCloseUnlift unlift $ MkAnEditLens {..}

sndLiftEditLens ::
       forall editx edita editb. EditLens edita editb -> EditLens (PairEdit editx edita) (PairEdit editx editb)
sndLiftEditLens (MkCloseUnlift (unlift :: Unlift t) (MkAnEditLens (MkAnEditFunction g u) pe)) = let
    efGet :: ReadFunctionT t (PairEditReader editx edita) (PairEditReader editx editb)
    efGet mr (MkTupleEditReader SelectFirst rt) = lift $ mr (MkTupleEditReader SelectFirst rt)
    efGet mr (MkTupleEditReader SelectSecond rt) = g (secondReadFunction mr) rt
    efUpdate ::
           forall m. MonadIO m
        => PairEdit editx edita
        -> MutableRead m (EditReader (PairEdit editx edita))
        -> t m [PairEdit editx editb]
    efUpdate (MkTupleEdit SelectFirst ex) _ = withTransConstraintTM @MonadIO $ return [MkTupleEdit SelectFirst ex]
    efUpdate (MkTupleEdit SelectSecond ea) mr =
        withTransConstraintTM @MonadIO $ do
            ebs <- u ea $ secondReadFunction mr
            return $ fmap (MkTupleEdit SelectSecond) ebs
    elFunction :: AnEditFunction t (PairEdit editx edita) (PairEdit editx editb)
    elFunction = MkAnEditFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [PairEdit editx editb]
        -> MutableRead m (EditReader (PairEdit editx edita))
        -> t m (Maybe [PairEdit editx edita])
    elPutEdits edits mr =
        case partitionPairEdits edits of
            (exs, ebs) ->
                withTransConstraintTM @MonadIO $
                getComposeM $ do
                    eas <- MkComposeM $ pe ebs $ secondReadFunction mr
                    return $ (fmap (MkTupleEdit SelectFirst) exs) ++ (fmap (MkTupleEdit SelectSecond) eas)
    in MkCloseUnlift unlift $ MkAnEditLens {..}

pairJoinAnEditFunctions ::
       forall t edita editb1 editb2. MonadTransUnlift t
    => AnEditFunction t edita editb1
    -> AnEditFunction t edita editb2
    -> AnEditFunction t edita (PairEdit editb1 editb2)
pairJoinAnEditFunctions (MkAnEditFunction g1 u1) (MkAnEditFunction g2 u2) = let
    g12 :: ReadFunctionT t (EditReader edita) (PairEditReader editb1 editb2)
    g12 mr (MkTupleEditReader SelectFirst rt) = g1 mr rt
    g12 mr (MkTupleEditReader SelectSecond rt) = g2 mr rt
    u12 :: forall m. MonadIO m
        => edita
        -> MutableRead m (EditReader edita)
        -> t m [PairEdit editb1 editb2]
    u12 ea mr =
        withTransConstraintTM @MonadIO $ do
            eb1s <- u1 ea mr
            eb2s <- u2 ea mr
            return $ fmap (MkTupleEdit SelectFirst) eb1s ++ fmap (MkTupleEdit SelectSecond) eb2s
    in MkAnEditFunction g12 u12

pairJoinEditFunctions ::
       forall edita editb1 editb2.
       EditFunction edita editb1
    -> EditFunction edita editb2
    -> EditFunction edita (PairEdit editb1 editb2)
pairJoinEditFunctions = joinUnliftables pairJoinAnEditFunctions

pairJoinEditLenses ::
       forall edita editb1 editb2.
       EditLens edita editb1
    -> EditLens edita editb2
    -> EditLens edita (PairEdit editb1 editb2)
pairJoinEditLenses =
    joinUnliftables $ \(MkAnEditLens af1 pe1 :: AnEditLens t edita editb1) (MkAnEditLens af2 pe2) -> let
        af12 = pairJoinAnEditFunctions af1 af2
        pe12 ::
               forall m. MonadIO m
            => [PairEdit editb1 editb2]
            -> MutableRead m (EditReader edita)
            -> t m (Maybe [edita])
        pe12 edits mr =
            case partitionPairEdits edits of
                (eb1, eb2) ->
                    withTransConstraintTM @MonadIO $
                    getComposeM $ do
                        ea1 <- MkComposeM $ pe1 eb1 mr
                        ea2 <- MkComposeM $ pe2 eb2 mr
                        return $ ea1 ++ ea2
        in MkAnEditLens af12 pe12
