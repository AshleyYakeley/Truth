module Truth.Core.Types.Function where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Pair
import Truth.Core.Types.Tuple
import Truth.Core.Types.Whole

data FunctionSelector a (eb :: Type) (et :: Type) where
    MkFunctionSelector :: a -> FunctionSelector a update update

instance (Eq a) => TestEquality (FunctionSelector a updateB) where
    testEquality (MkFunctionSelector a1) (MkFunctionSelector a2)
        | a1 == a2 = Just Refl
    testEquality _ _ = Nothing

instance (Finite a, SubjectReader (UpdateReader update)) => SubjectTupleSelector (FunctionSelector a update) where
    type TupleSubject (FunctionSelector a update) = a -> UpdateSubject update
    tupleReadFromSubject (MkFunctionSelector a) ab = ab a
    tupleWriteToSubject (MkFunctionSelector a) b _ a'
        | a == a' = b
    tupleWriteToSubject _ _ ab a' = ab a'

instance Finite a => FiniteTupleSelector (FunctionSelector a update) where
    tupleConstruct f = assemble (\a -> f (MkFunctionSelector a))

instance (c (UpdateReader update)) => TupleReaderWitness c (FunctionSelector a update) where
    tupleReaderWitness (MkFunctionSelector _) = Dict

instance (c (UpdateEdit update)) => TupleEditWitness c (FunctionSelector a update) where
    tupleEditWitness (MkFunctionSelector _) = Dict

instance (c update) => TupleUpdateWitness c (FunctionSelector a update) where
    tupleUpdateWitness (MkFunctionSelector _) = Dict

type FunctionUpdateReader a update = TupleUpdateReader (FunctionSelector a update)

type FunctionUpdateEdit a update = TupleUpdateEdit (FunctionSelector a update)

type FunctionUpdate a update = TupleUpdate (FunctionSelector a update)

functionEditLens :: Eq a => a -> EditLens (FunctionUpdate a update) update
functionEditLens a = tupleEditLens $ MkFunctionSelector a

constFunctionReadFunction :: ReadFunction (UpdateReader update) (FunctionUpdateReader a update)
constFunctionReadFunction mr (MkTupleUpdateReader (MkFunctionSelector _) rt) = mr rt

functionLiftUpdateFunction ::
       forall a updateA updateB.
       UpdateFunction updateA updateB
    -> UpdateFunction (FunctionUpdate a updateA) (FunctionUpdate a updateB)
functionLiftUpdateFunction (MkCloseUnlift (unlift :: Unlift t) (MkAnUpdateFunction g u)) =
    MkCloseUnlift unlift $ let
        toMR :: a -> MutableRead m (FunctionUpdateReader a updateA) -> MutableRead m (UpdateReader updateA)
        toMR a mr rat = mr $ MkTupleUpdateReader (MkFunctionSelector a) rat
        ufGet :: ReadFunctionT t (FunctionUpdateReader a updateA) (FunctionUpdateReader a updateB)
        ufGet mr (MkTupleUpdateReader (MkFunctionSelector a) rbt) = g (toMR a mr) rbt
        ufUpdate ::
               forall m. MonadIO m
            => FunctionUpdate a updateA
            -> MutableRead m (FunctionUpdateReader a updateA)
            -> t m [FunctionUpdate a updateB]
        ufUpdate (MkTupleUpdate (MkFunctionSelector a) update) mr =
            withTransConstraintTM @MonadIO $ do
                ebs <- u update (toMR a mr)
                return $ fmap (\eb -> MkTupleUpdate (MkFunctionSelector a) eb) ebs
        in MkAnUpdateFunction {..}

applyFunctionUpdateFunction ::
       forall a update. (Eq a, IsUpdate update, FullEdit (UpdateEdit update))
    => UpdateFunction (PairUpdate (FunctionUpdate a update) (WholeUpdate a)) update
applyFunctionUpdateFunction =
    MkCloseUnlift identityUnlift $ let
        ufGet ::
               ReadFunctionT IdentityT (PairUpdateReader (FunctionUpdate a update) (WholeUpdate a)) (UpdateReader update)
        ufGet mr rt =
            lift $ do
                a <- mr $ MkTupleUpdateReader SelectSecond ReadWhole
                mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) rt
        ufUpdate ::
               forall m. MonadIO m
            => PairUpdate (FunctionUpdate a update) (WholeUpdate a)
            -> MutableRead m (PairUpdateReader (FunctionUpdate a update) (WholeUpdate a))
            -> IdentityT m [update]
        ufUpdate (MkTupleUpdate SelectFirst (MkTupleUpdate (MkFunctionSelector ae) update)) mr =
            lift $ do
                a <- mr $ MkTupleUpdateReader SelectSecond ReadWhole
                return $
                    if a == ae
                        then [update]
                        else []
        ufUpdate (MkTupleUpdate SelectSecond (MkWholeReaderUpdate a)) mr =
            lift $ do
                edits <-
                    getReplaceEdits $ \rt ->
                        mr $ MkTupleUpdateReader SelectFirst $ MkTupleUpdateReader (MkFunctionSelector a) rt
                return $ fmap editUpdate edits
        in MkAnUpdateFunction {..}

functionEditApply ::
       (Eq p, IsUpdate updateB, FullEdit (UpdateEdit updateB))
    => UpdateFunction updateA (FunctionUpdate p updateB)
    -> UpdateFunction updateA (WholeUpdate p)
    -> UpdateFunction updateA updateB
functionEditApply eff efwp = applyFunctionUpdateFunction . pairCombineUpdateFunctions eff efwp

maybeFunctionUpdateFunction ::
       forall a update. UpdateFunction (PairUpdate update (FunctionUpdate a update)) (FunctionUpdate (Maybe a) update)
maybeFunctionUpdateFunction =
    MkCloseUnlift identityUnlift $ let
        ufGet ::
               ReadFunctionT IdentityT (PairUpdateReader update (FunctionUpdate a update)) (FunctionUpdateReader (Maybe a) update)
        ufGet mr (MkTupleUpdateReader (MkFunctionSelector Nothing) rt) = lift $ mr $ MkTupleUpdateReader SelectFirst rt
        ufGet mr (MkTupleUpdateReader (MkFunctionSelector (Just a)) rt) =
            lift $ mr $ MkTupleUpdateReader SelectSecond $ MkTupleUpdateReader (MkFunctionSelector a) rt
        ufUpdate ::
               forall m. MonadIO m
            => PairUpdate update (FunctionUpdate a update)
            -> MutableRead m (PairUpdateReader update (FunctionUpdate a update))
            -> IdentityT m [FunctionUpdate (Maybe a) update]
        ufUpdate (MkTupleUpdate SelectFirst update) _ =
            lift $ return [MkTupleUpdate (MkFunctionSelector Nothing) update]
        ufUpdate (MkTupleUpdate SelectSecond (MkTupleUpdate (MkFunctionSelector a) update)) _ =
            lift $ return [MkTupleUpdate (MkFunctionSelector $ Just a) update]
        in MkAnUpdateFunction {..}

functionEditMaybe ::
       UpdateFunction updateA updateB
    -> UpdateFunction updateA (FunctionUpdate p updateB)
    -> UpdateFunction updateA (FunctionUpdate (Maybe p) updateB)
functionEditMaybe efn eff = maybeFunctionUpdateFunction . pairCombineUpdateFunctions efn eff
