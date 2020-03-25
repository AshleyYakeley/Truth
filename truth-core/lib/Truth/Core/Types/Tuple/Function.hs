module Truth.Core.Types.Tuple.Function
    ( FunctionSelector(..)
    , FunctionUpdateReader
    , FunctionUpdateEdit
    , FunctionUpdate
    , functionEditLens
    , constFunctionReadFunction
    --, functionLiftEditLens
    , contramapPartialFunctionEditLens
    --, applyFunctionUpdateFunction
    --, functionEditApply
    --, applyFunctionEditLens
    --, maybeFunctionUpdateFunction
    --, functionEditMaybe
    , functionEitherPairEditLens
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Types.Partial
import Truth.Core.Types.Tuple.Pair
import Truth.Core.Types.Tuple.Tuple

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

{-
functionUnliftReadable :: a -> Readable m (FunctionUpdateReader a update) -> Readable m (UpdateReader update)
functionUnliftReadable a mr rt = mr $ MkTupleUpdateReader (MkFunctionSelector a) rt

functionLiftEditLens ::
       forall a updateA updateB. (Eq a, ApplicableEdit (UpdateEdit updateA))
    => FloatingEditLens updateA updateB
    -> FloatingEditLens (FunctionUpdate a updateA) (FunctionUpdate a updateB)
functionLiftEditLens (MkFloatingEditLens init g u pe) = let
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
        elPutEditsFromPutEdit $ \(MkTupleUpdateEdit (MkFunctionSelector a) editb) mr -> do
            meditas <- pe [editb] $ functionUnliftReadable a mr
            return $ (fmap $ fmap $ MkTupleUpdateEdit (MkFunctionSelector a)) meditas
    in MkFloatingEditLens init' g' u' pe'
-}
functionEitherPairEditLens ::
       forall a b update.
       EditLens (PairUpdate (FunctionUpdate a update) (FunctionUpdate b update)) (FunctionUpdate (Either a b) update)
functionEitherPairEditLens = let
    elGet ::
           ReadFunction (PairUpdateReader (FunctionUpdate a update) (FunctionUpdate b update)) (FunctionUpdateReader (Either a b) update)
    elGet mr (MkTupleUpdateReader (MkFunctionSelector (Left a)) rt) =
        mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) rt
    elGet mr (MkTupleUpdateReader (MkFunctionSelector (Right b)) rt) =
        mr $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector b) rt
    elUpdate ::
           forall m. MonadIO m
        => PairUpdate (FunctionUpdate a update) (FunctionUpdate b update)
        -> Readable m (PairUpdateReader (FunctionUpdate a update) (FunctionUpdate b update))
        -> m [FunctionUpdate (Either a b) update]
    elUpdate (MkTupleUpdate SelectFirst (MkTupleUpdate (MkFunctionSelector a) update)) _ =
        return [MkTupleUpdate (MkFunctionSelector $ Left a) update]
    elUpdate (MkTupleUpdate SelectSecond (MkTupleUpdate (MkFunctionSelector b) update)) _ =
        return [MkTupleUpdate (MkFunctionSelector $ Right b) update]
    elPutEdits ::
           forall m. MonadIO m
        => [FunctionUpdateEdit (Either a b) update]
        -> Readable m (PairUpdateReader (FunctionUpdate a update) (FunctionUpdate b update))
        -> m (Maybe [PairUpdateEdit (FunctionUpdate a update) (FunctionUpdate b update)])
    elPutEdits =
        elPutEditsFromSimplePutEdit $ \(MkTupleUpdateEdit (MkFunctionSelector eab) edit) ->
            return $
            Just $
            pure $
            case eab of
                Left a -> MkTupleUpdateEdit SelectFirst $ MkTupleUpdateEdit (MkFunctionSelector a) edit
                Right b -> MkTupleUpdateEdit SelectSecond $ MkTupleUpdateEdit (MkFunctionSelector b) edit
    in MkEditLens {..}

contramapPartialFunctionEditLens ::
       forall update a b.
       (b -> a)
    -> (a -> b -> Bool)
    -> EditLens (FunctionUpdate a update) (PartialUpdate (FunctionUpdate b update))
contramapPartialFunctionEditLens ba matchab = let
    elGet :: ReadFunction (FunctionUpdateReader a update) (FunctionUpdateReader b update)
    elGet mr (MkTupleUpdateReader (MkFunctionSelector b) rt) = mr $ MkTupleUpdateReader (MkFunctionSelector $ ba b) rt
    elUpdate ::
           forall m. MonadIO m
        => FunctionUpdate a update
        -> Readable m (FunctionUpdateReader a update)
        -> m [PartialUpdate (FunctionUpdate b update)]
    elUpdate (MkTupleUpdate (MkFunctionSelector a) _) _ =
        return [UnknownPartialUpdate $ \(MkTupleUpdateReader (MkFunctionSelector b) _) -> matchab a b]
    elPutEdits ::
           forall m. MonadIO m
        => [FunctionUpdateEdit b update]
        -> Readable m (FunctionUpdateReader a update)
        -> m (Maybe [FunctionUpdateEdit a update])
    elPutEdits =
        elPutEditsFromSimplePutEdit $ \(MkTupleUpdateEdit (MkFunctionSelector b) edit) ->
            return $ Just $ [MkTupleUpdateEdit (MkFunctionSelector $ ba b) edit]
    in MkEditLens {..}
{-
applyFunctionUpdateFunction ::
       forall a update. (Eq a, IsUpdate update, FullEdit (UpdateEdit update))
    => UpdateFunction (PairUpdate (FunctionUpdate a update) (WholeUpdate a)) update
applyFunctionUpdateFunction = let
    elGet :: ReadFunction (PairUpdateReader (FunctionUpdate a update) (WholeUpdate a)) (UpdateReader update)
    elGet mr rt = do
        a <- mr $ MkTupleUpdateReader SelectSecond ReadWhole
        mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) rt
    elUpdate ::
           forall m. MonadIO m
        => PairUpdate (FunctionUpdate a update) (WholeUpdate a)
        -> Readable m (PairUpdateReader (FunctionUpdate a update) (WholeUpdate a))
        -> m [update]
    elUpdate (MkTupleUpdate SelectFirst (MkTupleUpdate (MkFunctionSelector ae) update)) mr = do
        a <- mr $ MkTupleUpdateReader SelectSecond ReadWhole
        return $
            if a == ae
                then [update]
                else []
    elUpdate (MkTupleUpdate SelectSecond (MkWholeReaderUpdate a)) mr = do
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
functionEditApply eff efwp = applyFunctionUpdateFunction . pairCombineEditLenses eff efwp

applyFunctionEditLens ::
       forall a update. (Eq a, IsUpdate update, FullEdit (UpdateEdit update))
    => FloatingEditLens (PairUpdate (FunctionUpdate a update) (WholeUpdate a)) update
applyFunctionEditLens = let
    elFunction = applyFunctionUpdateFunction
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> Readable m (PairUpdateReader (FunctionUpdate a update) (WholeUpdate a))
        -> m (Maybe [PairUpdateEdit (FunctionUpdate a update) (WholeUpdate a)])
    elPutEdits =
        elPutEditsFromPutEdit $ \edit mr -> do
            a <- mr $ MkTupleUpdateReader SelectSecond ReadWhole
            return $ Just $ pure $ MkTupleUpdateEdit SelectFirst $ MkTupleUpdateEdit (MkFunctionSelector a) edit
    in MkFloatingEditLens {..}

maybeFunctionUpdateFunction ::
       forall a update. UpdateFunction (PairUpdate update (FunctionUpdate a update)) (FunctionUpdate (Maybe a) update)
maybeFunctionUpdateFunction = let
    elGet :: ReadFunction (PairUpdateReader update (FunctionUpdate a update)) (FunctionUpdateReader (Maybe a) update)
    elGet mr (MkTupleUpdateReader (MkFunctionSelector Nothing) rt) = mr $ MkTupleUpdateReader SelectFirst rt
    elGet mr (MkTupleUpdateReader (MkFunctionSelector (Just a)) rt) =
        mr $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector a) rt
    elUpdate ::
           forall m. MonadIO m
        => PairUpdate update (FunctionUpdate a update)
        -> Readable m (PairUpdateReader update (FunctionUpdate a update))
        -> m [FunctionUpdate (Maybe a) update]
    elUpdate (MkTupleUpdate SelectFirst update) _ = return [MkTupleUpdate (MkFunctionSelector Nothing) update]
    elUpdate (MkTupleUpdate SelectSecond (MkTupleUpdate (MkFunctionSelector a) update)) _ =
        return [MkTupleUpdate (MkFunctionSelector $ Just a) update]
    in MkUpdateFunction {..}

functionEditMaybe ::
       UpdateFunction updateA updateB
    -> UpdateFunction updateA (FunctionUpdate p updateB)
    -> UpdateFunction updateA (FunctionUpdate (Maybe p) updateB)
functionEditMaybe efn eff = maybeFunctionUpdateFunction . pairCombineEditLenses efn eff
-}
