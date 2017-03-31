{-# OPTIONS -Wno-orphans #-}
module Control.Applicative.Free where
{
    import Data.KindCategory;
    import Data.Free;


    data FreeApplicative f a = FreePure a | forall t. FreeApply (f t) (FreeApplicative f (t -> a));

    instance Functor (FreeApplicative f) where
    {
        fmap ab (FreePure a) = FreePure (ab a);
        fmap ab (FreeApply ft fta) = FreeApply ft (fmap (\ta -> ab . ta) fta);
    };

    instance Applicative (FreeApplicative f) where
    {
        pure = FreePure;
        (FreePure ab) <*> fa = fmap ab fa;
        (FreeApply ft ftab) <*> fa = FreeApply ft ((fmap (\tab a t -> tab t a) ftab) <*> fa);
    };

    toFreeApplicative :: f a -> FreeApplicative f a;
    toFreeApplicative fa = FreeApply fa (FreePure id);

    fromFreeApplicative :: forall f r. (forall a. a -> f a) -> (forall a b. f (a -> b) -> f a -> f b) -> FreeApplicative f r -> f r;
    fromFreeApplicative pure' apply' = fFA where
    {
        fFA :: forall r'. FreeApplicative f r' -> f r';
        fFA (FreePure a) = pure' a;
        fFA (FreeApply ft fta) = apply' (fFA fta) ft;
    };

    instance HasFree Applicative f where
    {
        type Free Applicative f = FreeApplicative f;
        toFree = MkNestedMorphism toFreeApplicative
    };
}
