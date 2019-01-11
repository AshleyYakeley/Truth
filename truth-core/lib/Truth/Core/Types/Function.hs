module Truth.Core.Types.Function where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Pair
import Truth.Core.Types.Tuple
import Truth.Core.Types.Whole

data FunctionSelector a (eb :: Type) (et :: Type) where
    MkFunctionSelector :: a -> FunctionSelector a edit edit

instance (Eq a) => TestEquality (FunctionSelector a editb) where
    testEquality (MkFunctionSelector a1) (MkFunctionSelector a2)
        | a1 == a2 = Just Refl
    testEquality _ _ = Nothing

instance (Finite a, SubjectReader (EditReader edit)) => SubjectTupleSelector (FunctionSelector a edit) where
    type TupleSubject (FunctionSelector a edit) = a -> EditSubject edit
    tupleReadFromSubject (MkFunctionSelector a) ab = ab a
    tupleWriteToSubject (MkFunctionSelector a) b _ a'
        | a == a' = b
    tupleWriteToSubject _ _ ab a' = ab a'

instance Finite a => FiniteTupleSelector (FunctionSelector a edit) where
    tupleConstruct f = assemble (\a -> f (MkFunctionSelector a))

instance (c (EditReader edit)) => TupleReaderWitness c (FunctionSelector a edit) where
    tupleReaderWitness (MkFunctionSelector _) = Dict

instance (c edit) => TupleWitness c (FunctionSelector a edit) where
    tupleWitness (MkFunctionSelector _) = Dict

type FunctionEditReader a edit = TupleEditReader (FunctionSelector a edit)

type FunctionEdit a edit = TupleEdit (FunctionSelector a edit)

constFunctionReadFunction :: ReadFunction (EditReader edit) (FunctionEditReader a edit)
constFunctionReadFunction mr (MkTupleEditReader (MkFunctionSelector _) rt) = mr rt

functionLiftEditFunction ::
       forall a edita editb. EditFunction edita editb -> EditFunction (FunctionEdit a edita) (FunctionEdit a editb)
functionLiftEditFunction (MkCloseUnlift (unlift :: Unlift t) (MkAnEditFunction g u)) =
    MkCloseUnlift unlift $ let
        toMR :: a -> MutableRead m (FunctionEditReader a edita) -> MutableRead m (EditReader edita)
        toMR a mr rat = mr $ MkTupleEditReader (MkFunctionSelector a) rat
        efGet :: ReadFunctionT t (FunctionEditReader a edita) (FunctionEditReader a editb)
        efGet mr (MkTupleEditReader (MkFunctionSelector a) rbt) = g (toMR a mr) rbt
        efUpdate ::
               forall m. MonadIO m
            => FunctionEdit a edita
            -> MutableRead m (FunctionEditReader a edita)
            -> t m [FunctionEdit a editb]
        efUpdate (MkTupleEdit (MkFunctionSelector a) edit) mr =
            withTransConstraintTM @MonadIO $ do
                ebs <- u edit (toMR a mr)
                return $ fmap (\eb -> MkTupleEdit (MkFunctionSelector a) eb) ebs
        in MkAnEditFunction {..}

applyFunctionEditFunction ::
       forall a edit. (Eq a, FullEdit edit)
    => EditFunction (PairEdit (FunctionEdit a edit) (WholeEdit a)) edit
applyFunctionEditFunction =
    MkCloseUnlift identityUnlift $ let
        efGet :: ReadFunctionT IdentityT (PairEditReader (FunctionEdit a edit) (WholeEdit a)) (EditReader edit)
        efGet mr rt =
            lift $ do
                a <- mr $ MkTupleEditReader SelectSecond ReadWhole
                mr $ MkTupleEditReader SelectFirst $ MkTupleEditReader (MkFunctionSelector a) rt
        efUpdate ::
               forall m. MonadIO m
            => PairEdit (FunctionEdit a edit) (WholeEdit a)
            -> MutableRead m (PairEditReader (FunctionEdit a edit) (WholeEdit a))
            -> IdentityT m [edit]
        efUpdate (MkTupleEdit SelectFirst (MkTupleEdit (MkFunctionSelector ae) edit)) mr =
            lift $ do
                a <- mr $ MkTupleEditReader SelectSecond ReadWhole
                return $
                    if a == ae
                        then [edit]
                        else []
        efUpdate (MkTupleEdit SelectSecond (MkWholeEdit a)) mr =
            lift $
            getReplaceEdits $ \rt -> mr $ MkTupleEditReader SelectFirst $ MkTupleEditReader (MkFunctionSelector a) rt
        in MkAnEditFunction {..}

functionEditApply ::
       (Eq p, FullEdit editb)
    => EditFunction edita (FunctionEdit p editb)
    -> EditFunction edita (WholeEdit p)
    -> EditFunction edita editb
functionEditApply eff efwp = applyFunctionEditFunction . pairCombineEditFunctions eff efwp

maybeFunctionEditFunction ::
       forall a edit. EditFunction (PairEdit edit (FunctionEdit a edit)) (FunctionEdit (Maybe a) edit)
maybeFunctionEditFunction =
    MkCloseUnlift identityUnlift $ let
        efGet :: ReadFunctionT IdentityT (PairEditReader edit (FunctionEdit a edit)) (FunctionEditReader (Maybe a) edit)
        efGet mr (MkTupleEditReader (MkFunctionSelector Nothing) rt) = lift $ mr $ MkTupleEditReader SelectFirst rt
        efGet mr (MkTupleEditReader (MkFunctionSelector (Just a)) rt) =
            lift $ mr $ MkTupleEditReader SelectSecond $ MkTupleEditReader (MkFunctionSelector a) rt
        efUpdate ::
               forall m. MonadIO m
            => PairEdit edit (FunctionEdit a edit)
            -> MutableRead m (PairEditReader edit (FunctionEdit a edit))
            -> IdentityT m [FunctionEdit (Maybe a) edit]
        efUpdate (MkTupleEdit SelectFirst edit) _ = lift $ return [MkTupleEdit (MkFunctionSelector Nothing) edit]
        efUpdate (MkTupleEdit SelectSecond (MkTupleEdit (MkFunctionSelector a) edit)) _ =
            lift $ return [MkTupleEdit (MkFunctionSelector $ Just a) edit]
        in MkAnEditFunction {..}

functionEditMaybe ::
       EditFunction edita editb
    -> EditFunction edita (FunctionEdit p editb)
    -> EditFunction edita (FunctionEdit (Maybe p) editb)
functionEditMaybe efn eff = maybeFunctionEditFunction . pairCombineEditFunctions efn eff
