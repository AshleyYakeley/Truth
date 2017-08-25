module Data.FiniteSet where
{
    import Prelude;
    import Data.Semigroup;
    import qualified Data.List as List;
    import Data.MonoTraversable;
    import Data.Containers;
    import Data.KeyContainer;

    newtype FiniteSet a = MkFiniteSet {unFiniteSet :: [a]} deriving (Functor, {- Applicative,Monad,Alternative,MonadPlus, -} Semigroup,Monoid,MonoFunctor,MonoFoldable,GrowingAppend,Foldable);
    type instance Element (FiniteSet a) = a;

    instance Traversable FiniteSet where
    {
        traverse afb (MkFiniteSet aa) = fmap MkFiniteSet $ traverse afb aa;
    };

    instance MonoTraversable (FiniteSet a);

    instance Eq a => SetContainer (FiniteSet a) where
    {
        type ContainerKey (FiniteSet a) = a;
        member key (MkFiniteSet aa) = elem key aa;
        notMember key set = not $ member key set;
        union (MkFiniteSet a) (MkFiniteSet b) = MkFiniteSet $ List.union a b;
        difference (MkFiniteSet a) (MkFiniteSet b) = MkFiniteSet $ a List.\\ b;
        intersection (MkFiniteSet a) (MkFiniteSet b) = MkFiniteSet $ List.intersect a b;
        keys (MkFiniteSet a) = a;
    };

    instance Eq a => KeyContainer (FiniteSet a) where
    {
        elementKey _ a = a;
        lookupElement key = List.find (\k -> k == key);
        insertElement e (MkFiniteSet []) = MkFiniteSet [e];
        insertElement e (MkFiniteSet (a:aa)) | e == a = MkFiniteSet $ e:aa;
        insertElement e (MkFiniteSet (a:aa)) = MkFiniteSet $ a : (unFiniteSet $ insertElement e $ MkFiniteSet aa);
        deleteElement _ (MkFiniteSet []) = MkFiniteSet [];
        deleteElement k (MkFiniteSet (k':aa)) | k == k' = MkFiniteSet $ aa;
        deleteElement k (MkFiniteSet (a:aa)) = MkFiniteSet $ a : (unFiniteSet $ deleteElement k $ MkFiniteSet aa);
        fromElementList = MkFiniteSet;
    };
}
