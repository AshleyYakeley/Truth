module Data.Witness.All where
{
    import Prelude;
    import Data.Kind;
    import Data.Witness.Any;
    import Data.KindCategory;
    import Data.Functor.Const;
    import Data.Functor.Identity;

    newtype All (w :: * -> *) = MkAll {getAll :: forall t. w t -> t};
    newtype AllF (w :: k -> *) (f :: k -> *) = MkAllF {getAllF :: forall (t :: k). w t -> f t};

    allFToAll :: AllF w Identity -> All w;
    allFToAll (MkAllF wtit) = MkAll $ \wt -> runIdentity $ wtit wt;

    class WitnessConstraint (c :: k -> Constraint) (w :: k -> *) where
    {
        witnessConstraint :: forall (t :: k). w t -> ConstraintWitness (c t);
    };

    class FiniteWitness (w :: k -> *) where
    {
        assembleWitnessF :: Applicative m => (forall t. w t -> m (f t)) -> m (AllF w f);
    };

    allWitnesses :: FiniteWitness w => [AnyWitness w];
    allWitnesses = getConst $ assembleWitnessF $ \wt -> Const [MkAnyWitness wt];

    assembleWitness :: (FiniteWitness w,Applicative m) => (forall t. w t -> m t) -> m (All w);
    assembleWitness wtmt = fmap allFToAll $ assembleWitnessF $ \wt -> fmap Identity $ wtmt wt;

    mapAnyWitness :: (forall t. w1 t -> w2 t) -> AnyWitness w1 -> AnyWitness w2;
    mapAnyWitness f (MkAnyWitness wt) = MkAnyWitness $ f wt;
}
