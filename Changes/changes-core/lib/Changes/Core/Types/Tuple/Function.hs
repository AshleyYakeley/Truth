module Changes.Core.Types.Tuple.Function
    ( FunctionSelector (..)
    , FunctionUpdateReader
    , FunctionUpdateEdit
    , FunctionUpdate
    , functionChangeLens
    , constFunctionReadFunction
    -- , functionLiftChangeLens
    , contramapPartialFunctionChangeLens
    -- , applyFunctionUpdateFunction
    -- , functionEditApply
    -- , applyFunctionChangeLens
    -- , maybeFunctionUpdateFunction
    -- , functionEditMaybe
    , functionEitherPairChangeLens
    )
where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.Partial
import Changes.Core.Types.Tuple.Pair
import Changes.Core.Types.Tuple.Tuple

data FunctionSelector a (eb :: Type) (et :: Type) where
    MkFunctionSelector :: a -> FunctionSelector a update update

instance Eq a => TestEquality (FunctionSelector a updateB) where
    testEquality (MkFunctionSelector a1) (MkFunctionSelector a2)
        | a1 == a2 = Just Refl
    testEquality _ _ = Nothing

instance SubjectReader (UpdateReader update) => SubjectTupleSelectorRead (FunctionSelector a update) where
    type TupleSubject (FunctionSelector a update) = a -> UpdateSubject update
    tupleReadFromSubject (MkFunctionSelector a) ab = ab a

instance (Eq a, SubjectReader (UpdateReader update)) => SubjectTupleSelector (FunctionSelector a update) where
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

functionChangeLens :: Eq a => a -> ChangeLens (FunctionUpdate a update) update
functionChangeLens a = tupleChangeLens $ MkFunctionSelector a

constFunctionReadFunction :: ReadFunction (UpdateReader update) (FunctionUpdateReader a update)
constFunctionReadFunction mr (MkTupleUpdateReader (MkFunctionSelector _) rt) = mr rt

{-
functionUnliftReadable :: a -> Readable m (FunctionUpdateReader a update) -> Readable m (UpdateReader update)
functionUnliftReadable a mr rt = mr $ MkTupleUpdateReader (MkFunctionSelector a) rt

functionLiftChangeLens ::
       forall a updateA updateB. (Eq a, ApplicableEdit (UpdateEdit updateA))
    => FloatingChangeLens updateA updateB
    -> FloatingChangeLens (FunctionUpdate a updateA) (FunctionUpdate a updateB)
functionLiftChangeLens (MkFloatingChangeLens finit g u pe) = let
    g' :: ReadFunction (FunctionUpdateReader a updateA) (FunctionUpdateReader a updateB)
    g' mr (MkTupleUpdateReader (MkFunctionSelector a) rbt) = g (functionUnliftReadable a mr) rbt
    u' ::
           forall m. MonadIO m
        => FunctionUpdate a updateA
        -> Readable m (FunctionUpdateReader a updateA)
        -> m [FunctionUpdate a updateB]
    u' (MkTupleUpdate (MkFunctionSelector a) update) mr = do
        ebs <- u update (functionUnliftReadable a mr)
        return $ fmap (\eb -> MkTupleUpdate (MkFunctionSelector a) eb) ebs
    pe' :: forall m. MonadIO m
        => [FunctionUpdateEdit a updateB]
        -> Readable m (FunctionUpdateReader a updateA)
        -> m (Maybe [FunctionUpdateEdit a updateA])
    pe' =
        clPutEditsFromPutEdit $ \(MkTupleUpdateEdit (MkFunctionSelector a) editb) mr -> do
            meditas <- pe [editb] $ functionUnliftReadable a mr
            return $ (fmap $ fmap $ MkTupleUpdateEdit (MkFunctionSelector a)) meditas
    in MkFloatingChangeLens finit' g' u' pe'
-}
functionEitherPairChangeLens ::
    forall a b update.
    ChangeLens (PairUpdate (FunctionUpdate a update) (FunctionUpdate b update)) (FunctionUpdate (Either a b) update)
functionEitherPairChangeLens = let
    clRead ::
        ReadFunction (PairUpdateReader (FunctionUpdate a update) (FunctionUpdate b update)) (FunctionUpdateReader (Either a b) update)
    clRead mr (MkTupleUpdateReader (MkFunctionSelector (Left a)) rt) =
        mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) rt
    clRead mr (MkTupleUpdateReader (MkFunctionSelector (Right b)) rt) =
        mr $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector b) rt
    clUpdate ::
        forall m.
        MonadIO m =>
        PairUpdate (FunctionUpdate a update) (FunctionUpdate b update) ->
        Readable m (PairUpdateReader (FunctionUpdate a update) (FunctionUpdate b update)) ->
        m [FunctionUpdate (Either a b) update]
    clUpdate (MkTupleUpdate SelectFirst (MkTupleUpdate (MkFunctionSelector a) update)) _ =
        return [MkTupleUpdate (MkFunctionSelector $ Left a) update]
    clUpdate (MkTupleUpdate SelectSecond (MkTupleUpdate (MkFunctionSelector b) update)) _ =
        return [MkTupleUpdate (MkFunctionSelector $ Right b) update]
    clPutEdits ::
        forall m.
        MonadIO m =>
        [FunctionUpdateEdit (Either a b) update] ->
        Readable m (PairUpdateReader (FunctionUpdate a update) (FunctionUpdate b update)) ->
        m (Maybe [PairUpdateEdit (FunctionUpdate a update) (FunctionUpdate b update)])
    clPutEdits =
        clPutEditsFromSimplePutEdit $ \(MkTupleUpdateEdit (MkFunctionSelector eab) edit) ->
            return
                $ Just
                $ pure
                $ case eab of
                    Left a -> MkTupleUpdateEdit SelectFirst $ MkTupleUpdateEdit (MkFunctionSelector a) edit
                    Right b -> MkTupleUpdateEdit SelectSecond $ MkTupleUpdateEdit (MkFunctionSelector b) edit
    in MkChangeLens{..}

contramapPartialFunctionChangeLens ::
    forall update a b.
    (b -> a) ->
    (a -> b -> Bool) ->
    ChangeLens (FunctionUpdate a update) (PartialUpdate (FunctionUpdate b update))
contramapPartialFunctionChangeLens ba matchab = let
    clRead :: ReadFunction (FunctionUpdateReader a update) (FunctionUpdateReader b update)
    clRead mr (MkTupleUpdateReader (MkFunctionSelector b) rt) = mr $ MkTupleUpdateReader (MkFunctionSelector $ ba b) rt
    clUpdate ::
        forall m.
        MonadIO m =>
        FunctionUpdate a update ->
        Readable m (FunctionUpdateReader a update) ->
        m [PartialUpdate (FunctionUpdate b update)]
    clUpdate (MkTupleUpdate (MkFunctionSelector a) _) _ =
        return [UnknownPartialUpdate $ \(MkTupleUpdateReader (MkFunctionSelector b) _) -> matchab a b]
    clPutEdits ::
        forall m.
        MonadIO m =>
        [FunctionUpdateEdit b update] ->
        Readable m (FunctionUpdateReader a update) ->
        m (Maybe [FunctionUpdateEdit a update])
    clPutEdits =
        clPutEditsFromSimplePutEdit $ \(MkTupleUpdateEdit (MkFunctionSelector b) edit) ->
            return $ Just $ [MkTupleUpdateEdit (MkFunctionSelector $ ba b) edit]
    in MkChangeLens{..}

{-
applyFunctionUpdateFunction ::
       forall a update. (Eq a, IsUpdate update, FullEdit (UpdateEdit update))
    => UpdateFunction (PairUpdate (FunctionUpdate a update) (WholeUpdate a)) update
applyFunctionUpdateFunction = let
    clRead :: ReadFunction (PairUpdateReader (FunctionUpdate a update) (WholeUpdate a)) (UpdateReader update)
    clRead mr rt = do
        a <- mr $ MkTupleUpdateReader SelectSecond ReadWhole
        mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) rt
    clUpdate ::
           forall m. MonadIO m
        => PairUpdate (FunctionUpdate a update) (WholeUpdate a)
        -> Readable m (PairUpdateReader (FunctionUpdate a update) (WholeUpdate a))
        -> m [update]
    clUpdate (MkTupleUpdate SelectFirst (MkTupleUpdate (MkFunctionSelector ae) update)) mr = do
        a <- mr $ MkTupleUpdateReader SelectSecond ReadWhole
        return $
            if a == ae
                then [update]
                else []
    clUpdate (MkTupleUpdate SelectSecond (MkWholeReaderUpdate a)) mr = do
        edits <-
            getReplaceEdits $ \rt ->
                mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) rt
        return $ fmap editUpdate edits
    in MkUpdateFunction {..}

functionEditApply ::
       (Eq p, IsUpdate updateB, FullEdit (UpdateEdit updateB))
    => UpdateFunction updateA (FunctionUpdate p updateB)
    -> UpdateFunction updateA (WholeUpdate p)
    -> UpdateFunction updateA updateB
functionEditApply eff efwp = applyFunctionUpdateFunction . pairCombineChangeLenses eff efwp

applyFunctionChangeLens ::
       forall a update. (Eq a, IsUpdate update, FullEdit (UpdateEdit update))
    => FloatingChangeLens (PairUpdate (FunctionUpdate a update) (WholeUpdate a)) update
applyFunctionChangeLens = let
    elFunction = applyFunctionUpdateFunction
    clPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> Readable m (PairUpdateReader (FunctionUpdate a update) (WholeUpdate a))
        -> m (Maybe [PairUpdateEdit (FunctionUpdate a update) (WholeUpdate a)])
    clPutEdits =
        clPutEditsFromPutEdit $ \edit mr -> do
            a <- mr $ MkTupleUpdateReader SelectSecond ReadWhole
            return $ Just $ pure $ MkTupleUpdateEdit SelectFirst $ MkTupleUpdateEdit (MkFunctionSelector a) edit
    in MkFloatingChangeLens {..}

maybeFunctionUpdateFunction ::
       forall a update. UpdateFunction (PairUpdate update (FunctionUpdate a update)) (FunctionUpdate (Maybe a) update)
maybeFunctionUpdateFunction = let
    clRead :: ReadFunction (PairUpdateReader update (FunctionUpdate a update)) (FunctionUpdateReader (Maybe a) update)
    clRead mr (MkTupleUpdateReader (MkFunctionSelector Nothing) rt) = mr $ MkTupleUpdateReader SelectFirst rt
    clRead mr (MkTupleUpdateReader (MkFunctionSelector (Just a)) rt) =
        mr $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector a) rt
    clUpdate ::
           forall m. MonadIO m
        => PairUpdate update (FunctionUpdate a update)
        -> Readable m (PairUpdateReader update (FunctionUpdate a update))
        -> m [FunctionUpdate (Maybe a) update]
    clUpdate (MkTupleUpdate SelectFirst update) _ = return [MkTupleUpdate (MkFunctionSelector Nothing) update]
    clUpdate (MkTupleUpdate SelectSecond (MkTupleUpdate (MkFunctionSelector a) update)) _ =
        return [MkTupleUpdate (MkFunctionSelector $ Just a) update]
    in MkUpdateFunction {..}

functionEditMaybe ::
       UpdateFunction updateA updateB
    -> UpdateFunction updateA (FunctionUpdate p updateB)
    -> UpdateFunction updateA (FunctionUpdate (Maybe p) updateB)
functionEditMaybe efn eff = maybeFunctionUpdateFunction . pairCombineChangeLenses efn eff
-}
