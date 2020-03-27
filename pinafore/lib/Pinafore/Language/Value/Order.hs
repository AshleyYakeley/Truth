module Pinafore.Language.Value.Order where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.FiniteSetRef
import Pinafore.Language.Value.Morphism
import Pinafore.Language.Value.Ref
import Shapes
import Truth.Core

data LangOrder baseupdate a =
    forall t. MkLangOrder (PinaforeFunctionMorphism baseupdate (Know a) t)
                          (t -> t -> Ordering)

instance Semigroup (LangOrder baseupdate a) where
    MkLangOrder fa oa <> MkLangOrder fb ob =
        MkLangOrder (liftA2 (,) fa fb) $ \(a1, b1) (a2, b2) ->
            case oa a1 a2 of
                EQ -> ob b1 b2
                cmp -> cmp

instance Monoid (LangOrder baseupdate a) where
    mempty = MkLangOrder (pure ()) $ compare @()
    mappend = (<>)

instance Contravariant (LangOrder baseupdate) where
    contramap ba (MkLangOrder ef o) = MkLangOrder (ef . (arr $ fmap ba)) o

instance HasVariance 'Contravariance (LangOrder baseupdate) where
    varianceRepresentational = Nothing

ordOrder ::
       forall baseupdate a. Ord a
    => LangOrder baseupdate a
ordOrder = MkLangOrder id compare

orderOn ::
       forall baseupdate a b.
       LangMorphism baseupdate '( a, TopType) '( BottomType, b)
    -> LangOrder baseupdate b
    -> LangOrder baseupdate a
orderOn f (MkLangOrder ef o) = MkLangOrder (ef . langMorphismFunction f) o

noOrder :: forall baseupdate. LangOrder baseupdate TopType
noOrder = mempty

orders :: forall baseupdate a. [LangOrder baseupdate a] -> LangOrder baseupdate a
orders = mconcat

rev :: forall baseupdate a. LangOrder baseupdate a -> LangOrder baseupdate a
rev (MkLangOrder ef o) = MkLangOrder ef $ \a b -> o b a

qOrderSet ::
       forall baseupdate a. (?pinafore :: PinaforeContext baseupdate, HasPinaforeEntityUpdate baseupdate)
    => LangOrder baseupdate a
    -> PinaforeROWRef (FiniteSet a)
    -> PinaforeROWRef (Know [a])
qOrderSet (MkLangOrder (ofunc :: PinaforeFunctionMorphism baseupdate (Know a) t) oord) pset = let
    cmp :: (a, t) -> (a, t) -> Ordering
    cmp (_, t1) (_, t2) = oord t1 t2
    ofuncpair :: PinaforeFunctionMorphism baseupdate a (a, t)
    ofuncpair =
        proc a -> do
            kt <- ofunc -< Known a
            returnA -< (a, kt)
    upairs :: PinaforeROWRef (FiniteSet (a, t))
    upairs = applyPinaforeFunction pinaforeBase (cfmap ofuncpair) pset
    sortpoints :: FiniteSet (a, t) -> [a]
    sortpoints (MkFiniteSet pairs) = fmap fst $ sortBy cmp pairs
    --in unWholeUpdateFunction $ fmap (Known . sortpoints) $ MkWholeUpdateFunction upairs
    in eaMapReadOnlyWhole (Known . sortpoints) upairs

langOrderCompare ::
       forall baseupdate a b. (?pinafore :: PinaforeContext baseupdate, HasPinaforeEntityUpdate baseupdate)
    => (Ordering -> b)
    -> LangOrder baseupdate a
    -> PinaforeImmutableRef a
    -> PinaforeImmutableRef a
    -> PinaforeImmutableRef b
langOrderCompare ob (MkLangOrder ef o) fv1 fv2 =
    (\v1 v2 -> ob $ o v1 v2) <$> (applyImmutableRef pinaforeBase (fmap Known ef) fv1) <*>
    (applyImmutableRef pinaforeBase (fmap Known ef) fv2)

pinaforeSetGetOrdered ::
       forall baseupdate a. (?pinafore :: PinaforeContext baseupdate, HasPinaforeEntityUpdate baseupdate)
    => LangOrder baseupdate a
    -> LangFiniteSetRef '( BottomType, a)
    -> LangRef '( TopType, [a])
pinaforeSetGetOrdered order set = pinaforeROWRefToRef $ qOrderSet order $ langFiniteSetRefFunctionValue set

pinaforeUpdateOrder :: LangOrder baseupdate a -> UpdateOrder (ContextUpdate baseupdate (WholeUpdate (Know a)))
pinaforeUpdateOrder (MkLangOrder m cmp) =
    MkUpdateOrder cmp $ changeLensToFloating $ pinaforeFunctionMorphismUpdateFunction m
