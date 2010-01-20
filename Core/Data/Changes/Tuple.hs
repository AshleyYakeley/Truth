{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Changes.Tuple where
{
    import Data.Changes.FixedEditLens;
    import Data.Changes.WholeEdit;
    import Data.Changes.Edit;
    import Data.Witness;
    import Data.ConstFunction;
    import Data.IsTuple;
    import Data.Lens;
    import Control.Category;
    import Control.Monad.Identity;
    import Prelude hiding (id,(.));

    data TupleEdit editlist t where
    {
        MkTupleEdit ::
         forall n editlist t.
         (
            HasListElement n (ListTuple t),
            Edit (ListElement n editlist),
            Subject (ListElement n editlist) ~ ListElement n (ListTuple t)
         ) =>
         Nat n -> ListElement n editlist -> TupleEdit editlist t;
    };

    instance (IsTuple t) => Edit (TupleEdit editlist t) where
    {
        type Subject (TupleEdit editlist t) = t;

        applyEdit (MkTupleEdit el edit) = FunctionConstFunction (
            fromListTuple . modifyListElement el (applyConstFunction (applyEdit edit)) . toListTuple
        );

        invertEdit (MkTupleEdit el edit) t = do
        {
            edit' <- invertEdit edit (getListElement el (toListTuple t));
            return (MkTupleEdit el edit');
        };
    };


    type TupleWholeEdit editlist t = Either (WholeEdit t) (TupleEdit editlist t);

    tupleElementCleanLens' :: (IsTuple t,HasListElement n (ListTuple t),ListElement n (ListTuple t) ~ Subject (ListElement n editlist),Edit (ListElement n editlist)) =>
       Nat n -> CleanLens' Identity (TupleEdit editlist t) (ListElement n editlist);
    tupleElementCleanLens' n = MkCleanLens
    {
        cleanLensUpdate = \(MkTupleEdit n' edit) -> do
        {
            MkEqualType <- matchWitness n n';
            return edit;
        },
        cleanLensSimple = (listElementLens n) . (bijectionLens listTupleBijection),
        cleanLensPutEdit = \edit -> return (MkTupleEdit n edit)
    };

    tupleElementCleanLens :: (IsTuple t,HasListElement n (ListTuple t),ListElement n (ListTuple t) ~ Subject (ListElement n editlist),FullEdit (ListElement n editlist)) =>
       Nat n -> CleanLens' Identity (TupleWholeEdit editlist t) (ListElement n editlist);
    tupleElementCleanLens n = withWholeLens (tupleElementCleanLens' n);
}
