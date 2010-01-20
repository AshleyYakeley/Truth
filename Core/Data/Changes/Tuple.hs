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
    import Data.FunctorOne;
    import Data.Bijection;
    import Control.Applicative;
    import Control.Category;
    import Control.Monad.Identity;
    import Data.Maybe;
    import Prelude hiding (id,(.));


    data Zero;

    data Succ n;

    data Nat t where
    {
        ZeroNat :: Nat Zero;
        SuccNat :: Nat t -> Nat (Succ t);
    };

    instance SimpleWitness Nat where
    {
        matchWitness ZeroNat ZeroNat = return MkEqualType;
        matchWitness (SuccNat a) (SuccNat b) = do
        {
            MkEqualType <- matchWitness a b;
            return MkEqualType;
        };
        matchWitness _ _ = Nothing;
    };

    instance Eq1 Nat where
    {
        equals1 a b = isJust (matchWitness a b);
    };

    instance Representative Nat where
    {
        getRepWitness ZeroNat = MkRepWitness;
        getRepWitness (SuccNat n) = case getRepWitness n of
        {
            MkRepWitness -> MkRepWitness;
        };
    };

    instance Is Nat Zero where
    {
        representative = ZeroNat;
    };

    instance (Is Nat n) => Is Nat (Succ n) where
    {
        representative = SuccNat representative;
    };



    class Pick n list where
    {
        type Elem n list :: *;
        getListElement' :: Nat n -> list -> Elem n list;
        putListElement' :: Nat n -> Elem n list -> list -> list;
    };

    instance Pick Zero (a,r) where
    {
        type Elem Zero (a,r) = a;
        getListElement' _ (a,_) = a;
        putListElement' _ a (_,r) = (a,r);
    };

    instance (Pick n r) => Pick (Succ n) (a,r) where
    {
        type Elem (Succ n) (a,r) = Elem n r;
        getListElement' (SuccNat n) (_,r) = getListElement' n r;
        getListElement' _ _ = undefined;    -- hack to overcome dumb warning
        putListElement' (SuccNat n) a (f,r) = (f,putListElement' n a r);
        putListElement' _ _ _ = undefined;    -- hack to overcome dumb warning
    };

    modifyListElement' :: (Pick n t) => Nat n -> (Elem n t -> Elem n t) -> t -> t;
    modifyListElement' n aa t = putListElement' n (aa (getListElement' n t)) t;

    listTupleBijection :: (IsTuple t) => Bijection t (ListTuple t);
    listTupleBijection = MkBijection toListTuple fromListTuple;

    listElementLens :: (Pick n l) =>
       Nat n -> Lens' Identity l (Elem n l);
    listElementLens n = MkLens
    {
        lensGet = getListElement' n,
        lensPutback = \e -> FunctionConstFunction (return . (putListElement' n e))
    };

    bijectionLens :: Bijection a b -> Lens' Identity a b;
    bijectionLens (MkBijection ab ba) = MkLens ab (\b -> ConstConstFunction (return (ba b)));


    data TupleEdit editlist t where
    {
        MkTupleEdit ::
         forall n editlist t. (Pick n (ListTuple t), Edit (Elem n editlist), Subject (Elem n editlist) ~ Elem n (ListTuple t)) => Nat n -> Elem n editlist -> TupleEdit editlist t;
    };

    firstItem :: (Edit edit,ListTuple t ~ (Subject edit,ar)) => edit -> TupleEdit (edit,editr) t;
    firstItem = MkTupleEdit ZeroNat;

    instance (IsTuple t) => Edit (TupleEdit editlist t) where
    {
        type Subject (TupleEdit editlist t) = t;

        applyEdit (MkTupleEdit el edit) = FunctionConstFunction (
            fromListTuple . modifyListElement' el (applyConstFunction (applyEdit edit)) . toListTuple
        );

        invertEdit (MkTupleEdit el edit) t = do
        {
            edit' <- invertEdit edit (getListElement' el (toListTuple t));
            return (MkTupleEdit el edit');
        };
    };

    class (Category k) => CategoryOr k where
    {
	    codiag :: k (Either a a) a;
	    (|||) :: k a c -> k b c -> k (Either a b) c;

	    codiag = id ||| id;
    };

    instance (Applicative m, FunctorOne m) => CategoryOr (Lens' m) where
    {
        ac ||| bc = MkLens
        {
            lensGet = \eab -> case eab of
            {
                Left a -> lensGet ac a;
                Right b -> lensGet bc b;
            },
            lensPutback = \c -> FunctionConstFunction (\eab -> case eab of
            {
                Left a -> fmap Left (applyConstFunction (lensPutback ac c) a);
                Right b -> fmap Right (applyConstFunction (lensPutback bc c) b);
            })
        };
    };


    type TupleWholeEdit editlist t = Either (WholeEdit t) (TupleEdit editlist t);

    tupleElementCleanLens' :: (IsTuple t,Pick n (ListTuple t),Elem n (ListTuple t) ~ Subject (Elem n editlist),Edit (Elem n editlist)) =>
       Nat n -> CleanLens' Identity (TupleEdit editlist t) (Elem n editlist);
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

    tupleElementCleanLens :: (IsTuple t,Pick n (ListTuple t),Elem n (ListTuple t) ~ Subject (Elem n editlist),FullEdit (Elem n editlist)) =>
       Nat n -> CleanLens' Identity (TupleWholeEdit editlist t) (Elem n editlist);
    tupleElementCleanLens n = withWholeLens (tupleElementCleanLens' n);
}
