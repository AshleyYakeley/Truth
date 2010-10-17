{-# OPTIONS_GHC -fno-warn-orphans #-}
module Truth.Edit.Tuple where
{
    import Truth.Edit.CleanEditLens;
    import Truth.Edit.EditFunction;
    import Truth.Edit.WholeEdit;
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

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

    tupleElementCleanLens' ::
    (
     IsTuple t,
     HasListElement n (ListTuple t),
     ListElement n (ListTuple t) ~ Subject (ListElement n editlist),
     Edit (ListElement n editlist)
    ) =>
     Nat n -> CleanEditLens' Identity (TupleEdit editlist t) (ListElement n editlist);
    tupleElementCleanLens' n = MkCleanEditLens
    {
        cleanEditLensFunction = MkCleanEditFunction
        {
            cleanEditGet = (getListElement n) . toListTuple,
            cleanEditUpdate = \(MkTupleEdit n' edit) -> do
            {
                MkEqualType <- matchWitness n n';
                return edit;
            }
        },
        cleanEditLensPutEdit = \edit -> return (MkTupleEdit n edit)
    };

    tupleElementCleanLens :: (IsTuple t,HasListElement n (ListTuple t),ListElement n (ListTuple t) ~ Subject (ListElement n editlist),FullEdit (ListElement n editlist)) =>
       Nat n -> CleanEditLens' Identity (TupleWholeEdit editlist t) (ListElement n editlist);
    tupleElementCleanLens n = withWholeLens (tupleElementCleanLens' n);


    type PairEdit edita editb = TupleWholeEdit (edita,(editb,())) (Subject edita,Subject editb);



    data TupleItemEdit n edit t where
    {
        MkTupleItemEdit :: forall n edit t. edit -> TupleItemEdit n edit t;
    };

    instance
    (
        Is Nat n,
        IsTuple t,
        HasListElement n (ListTuple t),
        Subject edit ~ ListElement n (ListTuple t),
        Edit edit
    )
     => Edit (TupleItemEdit n edit t) where
    {
        type Subject (TupleItemEdit n edit t) = t;

        applyEdit (MkTupleItemEdit edit) = FunctionConstFunction (
            fromListTuple . modifyListElement (representative :: Nat n) (applyConstFunction (applyEdit edit)) . toListTuple
        );

        invertEdit (MkTupleItemEdit edit) t = do
        {
            edit' <- invertEdit edit (getListElement (representative :: Nat n) (toListTuple t));
            return (MkTupleItemEdit edit');
        };
    };
}
