module Pinafore.Language.Value.Order where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.FiniteSetRef
import Pinafore.Language.Value.Morphism
import Pinafore.Language.Value.Ref
import Shapes

data LangOrder a =
    forall t. MkLangOrder (PinaforeFunctionMorphism PinaforeStorageUpdate (Know a) t)
                          (t -> t -> Ordering)

instance Semigroup (LangOrder a) where
    MkLangOrder fa oa <> MkLangOrder fb ob =
        MkLangOrder (liftA2 (,) fa fb) $ \(a1, b1) (a2, b2) ->
            case oa a1 a2 of
                EQ -> ob b1 b2
                cmp -> cmp

instance Monoid (LangOrder a) where
    mempty = MkLangOrder (pure ()) $ compare @()
    mappend = (<>)

instance Contravariant (LangOrder) where
    contramap ba (MkLangOrder ef o) = MkLangOrder (ef . (arr $ fmap ba)) o

instance HasVariance 'Contravariance (LangOrder) where
    varianceRepresentational = Nothing

ordOrder ::
       forall a. Ord a
    => LangOrder a
ordOrder = MkLangOrder id compare

orderOn :: forall a b. LangMorphism '( a, TopType) '( BottomType, b) -> LangOrder b -> LangOrder a
orderOn f (MkLangOrder ef o) = MkLangOrder (ef . langMorphismFunction f) o

noOrder :: LangOrder TopType
noOrder = mempty

orders :: forall a. [LangOrder a] -> LangOrder a
orders = mconcat

rev :: forall a. LangOrder a -> LangOrder a
rev (MkLangOrder ef o) = MkLangOrder ef $ \a b -> o b a

qOrderSet ::
       forall a. (?pinafore :: PinaforeContext)
    => LangOrder a
    -> PinaforeROWRef (FiniteSet a)
    -> PinaforeROWRef (Know [a])
qOrderSet (MkLangOrder (ofunc :: PinaforeFunctionMorphism PinaforeStorageUpdate (Know a) t) oord) pset = let
    cmp :: (a, t) -> (a, t) -> Ordering
    cmp (_, t1) (_, t2) = oord t1 t2
    ofuncpair :: PinaforeFunctionMorphism PinaforeStorageUpdate a (a, t)
    ofuncpair =
        proc a -> do
            kt <- ofunc -< Known a
            returnA -< (a, kt)
    upairs :: PinaforeROWRef (FiniteSet (a, t))
    upairs = applyPinaforeFunction pinaforeEntityModel (cfmap ofuncpair) pset
    sortpoints :: FiniteSet (a, t) -> [a]
    sortpoints (MkFiniteSet pairs) = fmap fst $ sortBy cmp pairs
    --in unWholeUpdateFunction $ fmap (Known . sortpoints) $ MkWholeUpdateFunction upairs
    in eaMapReadOnlyWhole (Known . sortpoints) upairs

langOrderCompare ::
       forall a b. (?pinafore :: PinaforeContext)
    => (Ordering -> b)
    -> LangOrder a
    -> PinaforeImmutableRef a
    -> PinaforeImmutableRef a
    -> PinaforeImmutableRef b
langOrderCompare ob (MkLangOrder ef o) fv1 fv2 =
    (\v1 v2 -> ob $ o v1 v2) <$> (applyImmutableRef pinaforeEntityModel (fmap Known ef) fv1) <*>
    (applyImmutableRef pinaforeEntityModel (fmap Known ef) fv2)

pinaforeSetGetOrdered ::
       forall a. (?pinafore :: PinaforeContext)
    => LangOrder a
    -> LangFiniteSetRef '( BottomType, a)
    -> LangRef '( TopType, [a])
pinaforeSetGetOrdered order set = pinaforeROWRefToRef $ qOrderSet order $ langFiniteSetRefFunctionValue set

pinaforeUpdateOrder :: LangOrder a -> UpdateOrder (ContextUpdate PinaforeStorageUpdate (WholeUpdate (Know a)))
pinaforeUpdateOrder (MkLangOrder m cmp) =
    MkUpdateOrder cmp $ changeLensToFloating $ pinaforeFunctionMorphismUpdateFunction m
