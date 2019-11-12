module Pinafore.Language.Value.Order where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.FiniteSetRef
import Pinafore.Language.Value.Morphism
import Pinafore.Language.Value.Ref
import Shapes
import Truth.Core

data PinaforeOrder baseupdate a =
    forall t. MkPinaforeOrder (PinaforeFunctionMorphism baseupdate (Know a) t)
                              (t -> t -> Ordering)

instance Semigroup (PinaforeOrder baseupdate a) where
    MkPinaforeOrder fa oa <> MkPinaforeOrder fb ob =
        MkPinaforeOrder (liftA2 (,) fa fb) $ \(a1, b1) (a2, b2) ->
            case oa a1 a2 of
                EQ -> ob b1 b2
                cmp -> cmp

instance Monoid (PinaforeOrder baseupdate a) where
    mempty = MkPinaforeOrder (pure ()) $ compare @()
    mappend = (<>)

instance Contravariant (PinaforeOrder baseupdate) where
    contramap ba (MkPinaforeOrder ef o) = MkPinaforeOrder (ef . (arr $ fmap ba)) o

instance HasVariance 'Contravariance (PinaforeOrder baseupdate) where
    varianceRepresentational = Nothing

ordOrder ::
       forall baseupdate a. Ord a
    => PinaforeOrder baseupdate a
ordOrder = MkPinaforeOrder id compare

orderOn ::
       forall baseupdate a b.
       PinaforeMorphism baseupdate '( a, TopType) '( BottomType, b)
    -> PinaforeOrder baseupdate b
    -> PinaforeOrder baseupdate a
orderOn f (MkPinaforeOrder ef o) = MkPinaforeOrder (ef . pinaforeMorphismFunction f) o

noOrder :: forall baseupdate. PinaforeOrder baseupdate TopType
noOrder = mempty

orders :: forall baseupdate a. [PinaforeOrder baseupdate a] -> PinaforeOrder baseupdate a
orders = mconcat

rev :: forall baseupdate a. PinaforeOrder baseupdate a -> PinaforeOrder baseupdate a
rev (MkPinaforeOrder ef o) = MkPinaforeOrder ef $ \a b -> o b a

qOrderSet ::
       forall baseupdate a.
       PinaforeOrder baseupdate a
    -> PinaforeFunctionValue baseupdate (FiniteSet a)
    -> PinaforeFunctionValue baseupdate (Know [a])
qOrderSet (MkPinaforeOrder (ofunc :: PinaforeFunctionMorphism baseupdate (Know a) t) oord) pset = let
    cmp :: (a, t) -> (a, t) -> Ordering
    cmp (_, t1) (_, t2) = oord t1 t2
    ofuncpair :: PinaforeFunctionMorphism baseupdate a (a, t)
    ofuncpair =
        proc a -> do
            kt <- ofunc -< Known a
            returnA -< (a, kt)
    upairs :: PinaforeFunctionValue baseupdate (FiniteSet (a, t))
    upairs = applyPinaforeFunction (cfmap ofuncpair) pset
    sortpoints :: FiniteSet (a, t) -> [a]
    sortpoints (MkFiniteSet pairs) = fmap fst $ sortBy cmp pairs
    in case upairs of
           MkRunnable2 trun@(MkTransStackRunner _) upairs' ->
               MkRunnable2 trun $ unWholeAnUpdateFunction $ fmap (Known . sortpoints) $ MkWholeAnUpdateFunction upairs'

pinaforeOrderCompare ::
       forall baseupdate a b.
       (Ordering -> b)
    -> PinaforeOrder baseupdate a
    -> PinaforeImmutableReference baseupdate a
    -> PinaforeImmutableReference baseupdate a
    -> PinaforeImmutableReference baseupdate b
pinaforeOrderCompare ob (MkPinaforeOrder ef o) fv1 fv2 =
    (\v1 v2 -> ob $ o v1 v2) <$> (applyImmutableReference (fmap Known ef) fv1) <*>
    (applyImmutableReference (fmap Known ef) fv2)

pinaforeSetGetOrdered ::
       forall baseupdate a.
       PinaforeOrder baseupdate a
    -> PinaforeFiniteSetRef baseupdate '( BottomType, a)
    -> PinaforeRef baseupdate '( TopType, [a])
pinaforeSetGetOrdered order set = pinaforeFunctionToRef $ qOrderSet order $ pinaforeFiniteSetRefFunctionValue set
