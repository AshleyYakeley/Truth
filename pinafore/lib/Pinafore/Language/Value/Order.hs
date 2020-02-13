module Pinafore.Language.Value.Order where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.FiniteSetRef
import Pinafore.Language.Value.Morphism
import Pinafore.Language.Value.Ref
import Shapes
import Truth.Core

data PinaforeOrder a =
    forall t. MkPinaforeOrder (PinaforeFunctionMorphism PinaforeEntityUpdate (Know a) t)
                              (t -> t -> Ordering)

instance Semigroup (PinaforeOrder a) where
    MkPinaforeOrder fa oa <> MkPinaforeOrder fb ob =
        MkPinaforeOrder (liftA2 (,) fa fb) $ \(a1, b1) (a2, b2) ->
            case oa a1 a2 of
                EQ -> ob b1 b2
                cmp -> cmp

instance Monoid (PinaforeOrder a) where
    mempty = MkPinaforeOrder (pure ()) $ compare @()
    mappend = (<>)

instance Contravariant (PinaforeOrder) where
    contramap ba (MkPinaforeOrder ef o) = MkPinaforeOrder (ef . (arr $ fmap ba)) o

instance HasVariance 'Contravariance (PinaforeOrder) where
    varianceRepresentational = Nothing

ordOrder ::
       forall a. Ord a
    => PinaforeOrder a
ordOrder = MkPinaforeOrder id compare

orderOn :: forall a b. PinaforeMorphism '( a, TopType) '( BottomType, b) -> PinaforeOrder b -> PinaforeOrder a
orderOn f (MkPinaforeOrder ef o) = MkPinaforeOrder (ef . pinaforeMorphismFunction f) o

noOrder :: forall . PinaforeOrder TopType
noOrder = mempty

orders :: forall a. [PinaforeOrder a] -> PinaforeOrder a
orders = mconcat

rev :: forall a. PinaforeOrder a -> PinaforeOrder a
rev (MkPinaforeOrder ef o) = MkPinaforeOrder ef $ \a b -> o b a

qOrderSet ::
       forall a. (?pinafore :: PinaforeContext)
    => PinaforeOrder a
    -> PinaforeReadOnlyValue (FiniteSet a)
    -> PinaforeReadOnlyValue (Know [a])
qOrderSet (MkPinaforeOrder (ofunc :: PinaforeFunctionMorphism PinaforeEntityUpdate (Know a) t) oord) pset = let
    cmp :: (a, t) -> (a, t) -> Ordering
    cmp (_, t1) (_, t2) = oord t1 t2
    ofuncpair :: PinaforeFunctionMorphism PinaforeEntityUpdate a (a, t)
    ofuncpair =
        proc a -> do
            kt <- ofunc -< Known a
            returnA -< (a, kt)
    upairs :: PinaforeReadOnlyValue (FiniteSet (a, t))
    upairs = applyPinaforeFunction pinaforeSubEntity (cfmap ofuncpair) pset
    sortpoints :: FiniteSet (a, t) -> [a]
    sortpoints (MkFiniteSet pairs) = fmap fst $ sortBy cmp pairs
    --in unWholeUpdateFunction $ fmap (Known . sortpoints) $ MkWholeUpdateFunction upairs
    in eaMapReadOnlyWhole (Known . sortpoints) upairs

pinaforeOrderCompare ::
       forall a b. (?pinafore :: PinaforeContext)
    => (Ordering -> b)
    -> PinaforeOrder a
    -> PinaforeImmutableReference a
    -> PinaforeImmutableReference a
    -> PinaforeImmutableReference b
pinaforeOrderCompare ob (MkPinaforeOrder ef o) fv1 fv2 =
    (\v1 v2 -> ob $ o v1 v2) <$> (applyImmutableReference pinaforeSubEntity (fmap Known ef) fv1) <*>
    (applyImmutableReference pinaforeSubEntity (fmap Known ef) fv2)

pinaforeSetGetOrdered ::
       forall a. (?pinafore :: PinaforeContext)
    => PinaforeOrder a
    -> PinaforeFiniteSetRef '( BottomType, a)
    -> PinaforeRef '( TopType, [a])
pinaforeSetGetOrdered order set = pinaforeReadOnlyValueToRef $ qOrderSet order $ pinaforeFiniteSetRefFunctionValue set

pinaforeUpdateOrder :: PinaforeOrder a -> UpdateOrder (ContextUpdate PinaforeEntityUpdate (WholeUpdate (Know a)))
pinaforeUpdateOrder (MkPinaforeOrder m cmp) =
    MkUpdateOrder cmp $ editLensToFloating $ pinaforeFunctionMorphismUpdateFunction m
