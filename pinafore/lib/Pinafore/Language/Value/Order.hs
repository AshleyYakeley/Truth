module Pinafore.Language.Value.Order where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.Morphism
import Pinafore.Language.Value.Ref
import Pinafore.Language.Value.SetRef
import Shapes
import Truth.Core

data PinaforeOrder baseedit a =
    forall t. MkPinaforeOrder (PinaforeFunctionMorphism baseedit (Know a) t)
                              (t -> t -> Ordering)

instance Semigroup (PinaforeOrder baseedit a) where
    MkPinaforeOrder fa oa <> MkPinaforeOrder fb ob =
        MkPinaforeOrder (liftA2 (,) fa fb) $ \(a1, b1) (a2, b2) ->
            case oa a1 a2 of
                EQ -> ob b1 b2
                cmp -> cmp

instance Monoid (PinaforeOrder baseedit a) where
    mempty = MkPinaforeOrder (pure ()) $ compare @()
    mappend = (<>)

instance Contravariant (PinaforeOrder baseedit) where
    contramap ba (MkPinaforeOrder ef o) = MkPinaforeOrder (ef . (arr $ fmap ba)) o

instance HasVariance 'Contravariance (PinaforeOrder baseedit) where
    varianceRepresentational = Nothing

ordOrder ::
       forall baseedit a. Ord a
    => PinaforeOrder baseedit a
ordOrder = MkPinaforeOrder id compare

orderOn ::
       forall baseedit a b.
       PinaforeMorphism baseedit '( a, TopType) '( BottomType, b)
    -> PinaforeOrder baseedit b
    -> PinaforeOrder baseedit a
orderOn f (MkPinaforeOrder ef o) = MkPinaforeOrder (ef . pinaforeMorphismFunction f) o

orders :: forall baseedit a. [PinaforeOrder baseedit a] -> PinaforeOrder baseedit a
orders = mconcat

rev :: forall baseedit a. PinaforeOrder baseedit a -> PinaforeOrder baseedit a
rev (MkPinaforeOrder ef o) = MkPinaforeOrder ef $ \a b -> o b a

qOrderSet ::
       forall baseedit a.
       PinaforeOrder baseedit a
    -> PinaforeFunctionValue baseedit (FiniteSet a)
    -> PinaforeFunctionValue baseedit (Know [a])
qOrderSet (MkPinaforeOrder (ofunc :: PinaforeFunctionMorphism baseedit (Know a) t) oord) pset = let
    cmp :: (a, t) -> (a, t) -> Ordering
    cmp (_, t1) (_, t2) = oord t1 t2
    ofuncpair :: PinaforeFunctionMorphism baseedit a (a, t)
    ofuncpair =
        proc a -> do
            kt <- ofunc -< Known a
            returnA -< (a, kt)
    upairs :: PinaforeFunctionValue baseedit (FiniteSet (a, t))
    upairs = applyPinaforeFunction (cfmap ofuncpair) pset
    sortpoints :: FiniteSet (a, t) -> [a]
    sortpoints (MkFiniteSet pairs) = fmap fst $ sortBy cmp pairs
    in unWholeEditFunction $ fmap (Known . sortpoints) $ MkWholeEditFunction upairs

pinaforeOrderCompare ::
       forall baseedit a b.
       (Ordering -> b)
    -> PinaforeOrder baseedit a
    -> PinaforeImmutableReference baseedit a
    -> PinaforeImmutableReference baseedit a
    -> PinaforeImmutableReference baseedit b
pinaforeOrderCompare ob (MkPinaforeOrder ef o) fv1 fv2 =
    (\v1 v2 -> ob $ o v1 v2) <$> (applyImmutableReference (fmap Known ef) fv1) <*>
    (applyImmutableReference (fmap Known ef) fv2)

pinaforeSetGetOrdered ::
       forall baseedit a.
       PinaforeOrder baseedit a
    -> PinaforeSetRef baseedit '( BottomType, a)
    -> PinaforeRef baseedit '( TopType, [a])
pinaforeSetGetOrdered order set = pinaforeFunctionToRef $ qOrderSet order $ pinaforeSetRefFunctionValue set
