module Truth.Core.Types.Pair where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Tuple

data PairSelector ea eb (et :: *) where
    EditFirst :: PairSelector ea eb ea
    EditSecond :: PairSelector ea eb eb

instance (c ea, c eb) => WitnessConstraint c (PairSelector ea eb) where
    witnessConstraint EditFirst = Dict
    witnessConstraint EditSecond = Dict

instance Show (PairSelector ea eb et) where
    show EditFirst = "first"
    show EditSecond = "second"

instance AllWitnessConstraint Show (PairSelector ea eb) where
    allWitnessConstraint = Dict

type PairEditReader ea eb = TupleEditReader (PairSelector ea eb)

type PairEdit ea eb = TupleEdit (PairSelector ea eb)

firstReadFunction :: ReadFunction (PairEditReader ea eb) (EditReader ea)
firstReadFunction = tupleReadFunction EditFirst

secondReadFunction :: ReadFunction (PairEditReader ea eb) (EditReader eb)
secondReadFunction = tupleReadFunction EditSecond

instance TestEquality (PairSelector ea eb) where
    testEquality EditFirst EditFirst = Just Refl
    testEquality EditSecond EditSecond = Just Refl
    testEquality _ _ = Nothing

instance (SubjectReader (EditReader ea), SubjectReader (EditReader eb)) =>
         SubjectTupleSelector (PairSelector ea eb) where
    type TupleSubject (PairSelector ea eb) = (EditSubject ea, EditSubject eb)
    tupleReadFromSubject EditFirst (a, _b) = a
    tupleReadFromSubject EditSecond (_a, b) = b

instance FiniteTupleSelector (PairSelector ea eb) where
    tupleConstruct f = (,) <$> f EditFirst <*> f EditSecond

instance (c (EditReader ea), c (EditReader eb)) => TupleReaderWitness c (PairSelector ea eb) where
    tupleReaderWitness EditFirst = Dict
    tupleReaderWitness EditSecond = Dict

instance (c ea, c eb) => TupleWitness c (PairSelector ea eb) where
    tupleWitness EditFirst = Dict
    tupleWitness EditSecond = Dict

partitionPairEdits :: forall ea eb. [PairEdit ea eb] -> ([ea], [eb])
partitionPairEdits pes = let
    toEither :: PairEdit ea eb -> Either ea eb
    toEither (MkTupleEdit EditFirst ea) = Left ea
    toEither (MkTupleEdit EditSecond eb) = Right eb
    in partitionEithers $ fmap toEither pes

pairMutableRead ::
       MutableRead m (EditReader ea) -> MutableRead m (EditReader eb) -> MutableRead m (PairEditReader ea eb)
pairMutableRead mra _mrb (MkTupleEditReader EditFirst ra) = mra ra
pairMutableRead _mra mrb (MkTupleEditReader EditSecond rb) = mrb rb

fstMutableRead :: MutableRead m (PairEditReader ea eb) -> MutableRead m (EditReader ea)
fstMutableRead mr ra = mr $ MkTupleEditReader EditFirst ra

sndMutableRead :: MutableRead m (PairEditReader ea eb) -> MutableRead m (EditReader eb)
sndMutableRead mr rb = mr $ MkTupleEditReader EditSecond rb

fstLiftEditLens ::
       forall editx edita editb. EditLens edita editb -> EditLens (PairEdit edita editx) (PairEdit editb editx)
fstLiftEditLens (MkCloseUnlift (unlift :: Unlift t) (MkAnEditLens (MkAnEditFunction g u) pe)) = let
    efGet :: ReadFunctionT t (PairEditReader edita editx) (PairEditReader editb editx)
    efGet mr (MkTupleEditReader EditFirst rt) = g (firstReadFunction mr) rt
    efGet mr (MkTupleEditReader EditSecond rt) = lift $ mr (MkTupleEditReader EditSecond rt)
    efUpdate ::
           forall m. MonadIO m
        => PairEdit edita editx
        -> MutableRead m (EditReader (PairEdit edita editx))
        -> t m [PairEdit editb editx]
    efUpdate (MkTupleEdit EditFirst ea) mr =
        withTransConstraintTM @MonadIO $ do
            ebs <- u ea $ firstReadFunction mr
            return $ fmap (MkTupleEdit EditFirst) ebs
    efUpdate (MkTupleEdit EditSecond ex) _ = withTransConstraintTM @MonadIO $ return [MkTupleEdit EditSecond ex]
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
                getCompose $ do
                    eas <- Compose $ pe ebs $ firstReadFunction mr
                    return $ (fmap (MkTupleEdit EditFirst) eas) ++ (fmap (MkTupleEdit EditSecond) exs)
    in MkCloseUnlift unlift $ MkAnEditLens {..}

sndLiftEditLens ::
       forall editx edita editb. EditLens edita editb -> EditLens (PairEdit editx edita) (PairEdit editx editb)
sndLiftEditLens (MkCloseUnlift (unlift :: Unlift t) (MkAnEditLens (MkAnEditFunction g u) pe)) = let
    efGet :: ReadFunctionT t (PairEditReader editx edita) (PairEditReader editx editb)
    efGet mr (MkTupleEditReader EditFirst rt) = lift $ mr (MkTupleEditReader EditFirst rt)
    efGet mr (MkTupleEditReader EditSecond rt) = g (secondReadFunction mr) rt
    efUpdate ::
           forall m. MonadIO m
        => PairEdit editx edita
        -> MutableRead m (EditReader (PairEdit editx edita))
        -> t m [PairEdit editx editb]
    efUpdate (MkTupleEdit EditFirst ex) _ = withTransConstraintTM @MonadIO $ return [MkTupleEdit EditFirst ex]
    efUpdate (MkTupleEdit EditSecond ea) mr =
        withTransConstraintTM @MonadIO $ do
            ebs <- u ea $ secondReadFunction mr
            return $ fmap (MkTupleEdit EditSecond) ebs
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
                getCompose $ do
                    eas <- Compose $ pe ebs $ secondReadFunction mr
                    return $ (fmap (MkTupleEdit EditFirst) exs) ++ (fmap (MkTupleEdit EditSecond) eas)
    in MkCloseUnlift unlift $ MkAnEditLens {..}

pairJoinAnEditFunctions ::
       forall t edita editb1 editb2. MonadTransUnlift t
    => AnEditFunction t edita editb1
    -> AnEditFunction t edita editb2
    -> AnEditFunction t edita (PairEdit editb1 editb2)
pairJoinAnEditFunctions (MkAnEditFunction g1 u1) (MkAnEditFunction g2 u2) = let
    g12 :: ReadFunctionT t (EditReader edita) (PairEditReader editb1 editb2)
    g12 mr (MkTupleEditReader EditFirst rt) = g1 mr rt
    g12 mr (MkTupleEditReader EditSecond rt) = g2 mr rt
    u12 :: forall m. MonadIO m
        => edita
        -> MutableRead m (EditReader edita)
        -> t m [PairEdit editb1 editb2]
    u12 ea mr =
        withTransConstraintTM @MonadIO $ do
            eb1s <- u1 ea mr
            eb2s <- u2 ea mr
            return $ fmap (MkTupleEdit EditFirst) eb1s ++ fmap (MkTupleEdit EditSecond) eb2s
    in MkAnEditFunction g12 u12

pairJoinEditFunctions ::
       forall edita editb1 editb2.
       EditFunction edita editb1
    -> EditFunction edita editb2
    -> EditFunction edita (PairEdit editb1 editb2)
pairJoinEditFunctions = joinUnlifts $ \unlift af1 af2 -> MkCloseUnlift unlift $ pairJoinAnEditFunctions af1 af2

pairJoinEditLenses ::
       forall edita editb1 editb2.
       EditLens edita editb1
    -> EditLens edita editb2
    -> EditLens edita (PairEdit editb1 editb2)
pairJoinEditLenses =
    joinUnlifts $ \(unlift :: Unlift t) (MkAnEditLens af1 pe1) (MkAnEditLens af2 pe2) -> let
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
                    getCompose $ do
                        ea1 <- Compose $ pe1 eb1 mr
                        ea2 <- Compose $ pe2 eb2 mr
                        return $ ea1 ++ ea2
        in MkCloseUnlift unlift $ MkAnEditLens af12 pe12
