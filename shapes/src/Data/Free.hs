module Data.Free where
{
    import Shapes.Import;
    import Data.KindCategory;


    class c (Free c t) => HasFree (c :: k -> Constraint) (t :: k) where
    {
        type Free c t :: k;
        toFree :: KindMorphism k t (Free c t);
    };
}
