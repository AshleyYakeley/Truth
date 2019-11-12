module Truth.Core.Types.Function
    ( FunctionSelector(..)
    , FunctionUpdateReader
    , FunctionUpdateEdit
    , FunctionUpdate
    , functionEditLens
    , constFunctionReadFunction
    , functionLiftUpdateFunction
    , functionLiftEditLens
    , contramapPartialFunctionEditLens
    , applyFunctionUpdateFunction
    , functionEditApply
    , applyFunctionEditLens
    , maybeFunctionUpdateFunction
    , functionEditMaybe
    , functionEitherPairEditLens
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Pair
import Truth.Core.Types.Tuple
import Truth.Core.Types.Whole

data FunctionSelector a (eb :: Type) (et :: Type) where
    MkFunctionSelector :: a -> FunctionSelector a update update

instance Eq a => TestEquality (FunctionSelector a updateB) where
    testEquality (MkFunctionSelector a1) (MkFunctionSelector a2)
        | a1 == a2 = Just Refl
    testEquality _ _ = Nothing

instance (Eq a, SubjectReader (UpdateReader update)) => SubjectTupleSelector (FunctionSelector a update) where
    type TupleSubject (FunctionSelector a update) = a -> UpdateSubject update
    tupleReadFromSubject (MkFunctionSelector a) ab = ab a
    tupleWriteToSubject (MkFunctionSelector a) b _ a'
        | a == a' = b
    tupleWriteToSubject _ _ ab a' = ab a'

instance Finite a => FiniteTupleSelector (FunctionSelector a update) where
    tupleConstruct f = assemble (\a -> f (MkFunctionSelector a))

instance c (UpdateReader update) => TupleReaderWitness c (FunctionSelector a update) where
    tupleReaderWitness (MkFunctionSelector _) = Dict

instance c (UpdateEdit update) => TupleEditWitness c (FunctionSelector a update) where
    tupleEditWitness (MkFunctionSelector _) = Dict

instance c update => TupleUpdateWitness c (FunctionSelector a update) where
    tupleUpdateWitness (MkFunctionSelector _) = Dict

type FunctionUpdateReader a update = TupleUpdateReader (FunctionSelector a update)

type FunctionUpdateEdit a update = TupleUpdateEdit (FunctionSelector a update)

type FunctionUpdate a update = TupleUpdate (FunctionSelector a update)

functionEditLens :: Eq a => a -> EditLens (FunctionUpdate a update) update
functionEditLens a = tupleEditLens $ MkFunctionSelector a

constFunctionReadFunction :: ReadFunction (UpdateReader update) (FunctionUpdateReader a update)
constFunctionReadFunction mr (MkTupleUpdateReader (MkFunctionSelector _) rt) = mr rt

functionUnliftMutableRead :: a -> MutableRead m (FunctionUpdateReader a update) -> MutableRead m (UpdateReader update)
functionUnliftMutableRead a mr rt = mr $ MkTupleUpdateReader (MkFunctionSelector a) rt

functionLiftAnUpdateFunction ::
       forall tt a updateA updateB. MonadTransStackUnliftAll tt
    => AnUpdateFunction tt updateA updateB
    -> AnUpdateFunction tt (FunctionUpdate a updateA) (FunctionUpdate a updateB)
functionLiftAnUpdateFunction (MkAnUpdateFunction g u) = let
    ufGet :: ReadFunctionTT tt (FunctionUpdateReader a updateA) (FunctionUpdateReader a updateB)
    ufGet mr (MkTupleUpdateReader (MkFunctionSelector a) rbt) = g (functionUnliftMutableRead a mr) rbt
    ufUpdate ::
           forall m. MonadIO m
        => FunctionUpdate a updateA
        -> MutableRead m (FunctionUpdateReader a updateA)
        -> ApplyStack tt m [FunctionUpdate a updateB]
    ufUpdate (MkTupleUpdate (MkFunctionSelector a) update) mr =
        case transStackDict @MonadIO @tt @m of
            Dict -> do
                ebs <- u update (functionUnliftMutableRead a mr)
                return $ fmap (\eb -> MkTupleUpdate (MkFunctionSelector a) eb) ebs
    in MkAnUpdateFunction {..}

functionLiftUpdateFunction ::
       forall a updateA updateB.
       UpdateFunction updateA updateB
    -> UpdateFunction (FunctionUpdate a updateA) (FunctionUpdate a updateB)
functionLiftUpdateFunction (MkRunnable2 trun auf) =
    case transStackRunnerUnliftAllDict trun of
        Dict -> MkRunnable2 trun $ functionLiftAnUpdateFunction auf

functionLiftEditLens ::
       forall a updateA updateB. (Eq a, ApplicableEdit (UpdateEdit updateA))
    => EditLens updateA updateB
    -> EditLens (FunctionUpdate a updateA) (FunctionUpdate a updateB)
functionLiftEditLens (MkRunnable2 (trun :: TransStackRunner tt) (MkAnEditLens auf pe)) =
    case transStackRunnerUnliftAllDict trun of
        Dict -> let
            auf' = functionLiftAnUpdateFunction auf
            pe' :: forall m. MonadIO m
                => [FunctionUpdateEdit a updateB]
                -> MutableRead m (FunctionUpdateReader a updateA)
                -> ApplyStack tt m (Maybe [FunctionUpdateEdit a updateA])
            pe' =
                case transStackDict @MonadIO @tt @m of
                    Dict ->
                        elPutEditsFromPutEdit @tt $ \(MkTupleUpdateEdit (MkFunctionSelector a) editb) mr -> do
                            meditas <- pe [editb] $ functionUnliftMutableRead a mr
                            return $ (fmap $ fmap $ MkTupleUpdateEdit (MkFunctionSelector a)) meditas
            in MkRunnable2 trun $ MkAnEditLens auf' pe'

functionEitherPairEditLens ::
       forall a b update.
       EditLens (PairUpdate (FunctionUpdate a update) (FunctionUpdate b update)) (FunctionUpdate (Either a b) update)
functionEitherPairEditLens = let
    ufGet ::
           ReadFunction (PairUpdateReader (FunctionUpdate a update) (FunctionUpdate b update)) (FunctionUpdateReader (Either a b) update)
    ufGet mr (MkTupleUpdateReader (MkFunctionSelector (Left a)) rt) =
        mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) rt
    ufGet mr (MkTupleUpdateReader (MkFunctionSelector (Right b)) rt) =
        mr $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector b) rt
    ufUpdate ::
           forall m. MonadIO m
        => PairUpdate (FunctionUpdate a update) (FunctionUpdate b update)
        -> MutableRead m (PairUpdateReader (FunctionUpdate a update) (FunctionUpdate b update))
        -> m [FunctionUpdate (Either a b) update]
    ufUpdate (MkTupleUpdate SelectFirst (MkTupleUpdate (MkFunctionSelector a) update)) _ =
        return [MkTupleUpdate (MkFunctionSelector $ Left a) update]
    ufUpdate (MkTupleUpdate SelectSecond (MkTupleUpdate (MkFunctionSelector b) update)) _ =
        return [MkTupleUpdate (MkFunctionSelector $ Right b) update]
    elFunction ::
           AnUpdateFunction '[] (PairUpdate (FunctionUpdate a update) (FunctionUpdate b update)) (FunctionUpdate (Either a b) update)
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [FunctionUpdateEdit (Either a b) update]
        -> MutableRead m (PairUpdateReader (FunctionUpdate a update) (FunctionUpdate b update))
        -> m (Maybe [PairUpdateEdit (FunctionUpdate a update) (FunctionUpdate b update)])
    elPutEdits =
        elPutEditsFromSimplePutEdit @'[] $ \(MkTupleUpdateEdit (MkFunctionSelector eab) edit) ->
            return $
            Just $
            pure $
            case eab of
                Left a -> MkTupleUpdateEdit SelectFirst $ MkTupleUpdateEdit (MkFunctionSelector a) edit
                Right b -> MkTupleUpdateEdit SelectSecond $ MkTupleUpdateEdit (MkFunctionSelector b) edit
    in MkRunnable2 cmEmpty MkAnEditLens {..}

contramapPartialFunctionEditLens ::
       forall update a b.
       (b -> a)
    -> (a -> b -> Bool)
    -> EditLens (FunctionUpdate a update) (PartialUpdate (FunctionUpdate b update))
contramapPartialFunctionEditLens ba matchab = let
    ufGet :: ReadFunction (FunctionUpdateReader a update) (FunctionUpdateReader b update)
    ufGet mr (MkTupleUpdateReader (MkFunctionSelector b) rt) = mr $ MkTupleUpdateReader (MkFunctionSelector $ ba b) rt
    ufUpdate ::
           forall m. MonadIO m
        => FunctionUpdate a update
        -> MutableRead m (FunctionUpdateReader a update)
        -> m [PartialUpdate (FunctionUpdate b update)]
    ufUpdate (MkTupleUpdate (MkFunctionSelector a) _) _ =
        return [UnknownPartialUpdate $ \(MkTupleUpdateReader (MkFunctionSelector b) _) -> matchab a b]
    elFunction :: AnUpdateFunction '[] (FunctionUpdate a update) (PartialUpdate (FunctionUpdate b update))
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [FunctionUpdateEdit b update]
        -> MutableRead m (FunctionUpdateReader a update)
        -> m (Maybe [FunctionUpdateEdit a update])
    elPutEdits =
        elPutEditsFromSimplePutEdit @'[] $ \(MkTupleUpdateEdit (MkFunctionSelector b) edit) ->
            return $ Just $ [MkTupleUpdateEdit (MkFunctionSelector $ ba b) edit]
    in MkRunnable2 cmEmpty MkAnEditLens {..}

applyFunctionAnUpdateFunction ::
       forall a update. (Eq a, IsUpdate update, FullEdit (UpdateEdit update))
    => AnUpdateFunction '[] (PairUpdate (FunctionUpdate a update) (WholeUpdate a)) update
applyFunctionAnUpdateFunction = let
    ufGet :: ReadFunction (PairUpdateReader (FunctionUpdate a update) (WholeUpdate a)) (UpdateReader update)
    ufGet mr rt = do
        a <- mr $ MkTupleUpdateReader SelectSecond ReadWhole
        mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) rt
    ufUpdate ::
           forall m. MonadIO m
        => PairUpdate (FunctionUpdate a update) (WholeUpdate a)
        -> MutableRead m (PairUpdateReader (FunctionUpdate a update) (WholeUpdate a))
        -> m [update]
    ufUpdate (MkTupleUpdate SelectFirst (MkTupleUpdate (MkFunctionSelector ae) update)) mr = do
        a <- mr $ MkTupleUpdateReader SelectSecond ReadWhole
        return $
            if a == ae
                then [update]
                else []
    ufUpdate (MkTupleUpdate SelectSecond (MkWholeReaderUpdate a)) mr = do
        edits <-
            getReplaceEdits $ \rt ->
                mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) rt
        return $ fmap editUpdate edits
    in MkAnUpdateFunction {..}

applyFunctionUpdateFunction ::
       forall a update. (Eq a, IsUpdate update, FullEdit (UpdateEdit update))
    => UpdateFunction (PairUpdate (FunctionUpdate a update) (WholeUpdate a)) update
applyFunctionUpdateFunction = MkRunnable2 cmEmpty applyFunctionAnUpdateFunction

functionEditApply ::
       (Eq p, IsUpdate updateB, FullEdit (UpdateEdit updateB))
    => UpdateFunction updateA (FunctionUpdate p updateB)
    -> UpdateFunction updateA (WholeUpdate p)
    -> UpdateFunction updateA updateB
functionEditApply eff efwp = applyFunctionUpdateFunction . pairCombineUpdateFunctions eff efwp

applyFunctionEditLens ::
       forall a update. (Eq a, IsUpdate update, FullEdit (UpdateEdit update))
    => EditLens (PairUpdate (FunctionUpdate a update) (WholeUpdate a)) update
applyFunctionEditLens = let
    elFunction = applyFunctionAnUpdateFunction
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> MutableRead m (PairUpdateReader (FunctionUpdate a update) (WholeUpdate a))
        -> m (Maybe [PairUpdateEdit (FunctionUpdate a update) (WholeUpdate a)])
    elPutEdits =
        elPutEditsFromPutEdit @'[] $ \edit mr -> do
            a <- mr $ MkTupleUpdateReader SelectSecond ReadWhole
            return $ Just $ pure $ MkTupleUpdateEdit SelectFirst $ MkTupleUpdateEdit (MkFunctionSelector a) edit
    in MkRunnable2 cmEmpty MkAnEditLens {..}

maybeFunctionUpdateFunction ::
       forall a update. UpdateFunction (PairUpdate update (FunctionUpdate a update)) (FunctionUpdate (Maybe a) update)
maybeFunctionUpdateFunction =
    MkRunnable2 cmEmpty $ let
        ufGet ::
               ReadFunction (PairUpdateReader update (FunctionUpdate a update)) (FunctionUpdateReader (Maybe a) update)
        ufGet mr (MkTupleUpdateReader (MkFunctionSelector Nothing) rt) = mr $ MkTupleUpdateReader SelectFirst rt
        ufGet mr (MkTupleUpdateReader (MkFunctionSelector (Just a)) rt) =
            mr $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector a) rt
        ufUpdate ::
               forall m. MonadIO m
            => PairUpdate update (FunctionUpdate a update)
            -> MutableRead m (PairUpdateReader update (FunctionUpdate a update))
            -> m [FunctionUpdate (Maybe a) update]
        ufUpdate (MkTupleUpdate SelectFirst update) _ = return [MkTupleUpdate (MkFunctionSelector Nothing) update]
        ufUpdate (MkTupleUpdate SelectSecond (MkTupleUpdate (MkFunctionSelector a) update)) _ =
            return [MkTupleUpdate (MkFunctionSelector $ Just a) update]
        in MkAnUpdateFunction {..}

functionEditMaybe ::
       UpdateFunction updateA updateB
    -> UpdateFunction updateA (FunctionUpdate p updateB)
    -> UpdateFunction updateA (FunctionUpdate (Maybe p) updateB)
functionEditMaybe efn eff = maybeFunctionUpdateFunction . pairCombineUpdateFunctions efn eff
