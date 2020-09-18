module Pinafore.Language.Value.RefOrder where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.FiniteSetRef
import Pinafore.Language.Value.Morphism
import Pinafore.Language.Value.WholeRef
import Shapes

data LangRefOrder a =
    forall t. MkLangRefOrder (PinaforeFunctionMorphism PinaforeStorageUpdate (Know a) t)
                             (t -> t -> Ordering)

instance Semigroup (LangRefOrder a) where
    MkLangRefOrder fa oa <> MkLangRefOrder fb ob =
        MkLangRefOrder (liftA2 (,) fa fb) $ \(a1, b1) (a2, b2) ->
            case oa a1 a2 of
                EQ -> ob b1 b2
                cmp -> cmp

instance Monoid (LangRefOrder a) where
    mempty = MkLangRefOrder (pure ()) $ compare @()
    mappend = (<>)

instance Contravariant (LangRefOrder) where
    contramap ba (MkLangRefOrder ef o) = MkLangRefOrder (ef . (arr $ fmap ba)) o

instance HasVariance 'Contravariance (LangRefOrder) where
    varianceRepresentational = Nothing

pureRefOrder :: forall a. (a -> a -> Ordering) -> LangRefOrder a
pureRefOrder cmp = MkLangRefOrder id $ knowOrder cmp

refOrderOn :: forall a b. LangMorphism '( a, TopType) '( BottomType, b) -> LangRefOrder b -> LangRefOrder a
refOrderOn f (MkLangRefOrder ef o) = MkLangRefOrder (ef . langMorphismFunction f) o

noRefOrder :: LangRefOrder TopType
noRefOrder = mempty

refOrders :: forall a. [LangRefOrder a] -> LangRefOrder a
refOrders = mconcat

rev :: forall a. LangRefOrder a -> LangRefOrder a
rev (MkLangRefOrder ef o) = MkLangRefOrder ef $ \a b -> o b a

qOrderSet ::
       forall a. (?pinafore :: PinaforeContext)
    => LangRefOrder a
    -> PinaforeROWRef (FiniteSet a)
    -> PinaforeROWRef (Know [a])
qOrderSet (MkLangRefOrder (ofunc :: PinaforeFunctionMorphism PinaforeStorageUpdate (Know a) t) oord) pset = let
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
    in eaMapReadOnlyWhole (Known . sortpoints) upairs

langRefOrderCompare ::
       forall a. (?pinafore :: PinaforeContext)
    => LangRefOrder a
    -> PinaforeImmutableWholeRef a
    -> PinaforeImmutableWholeRef a
    -> PinaforeImmutableWholeRef Ordering
langRefOrderCompare (MkLangRefOrder ef o) fv1 fv2 =
    o <$> (applyImmutableRef pinaforeEntityModel (fmap Known ef) fv1) <*>
    (applyImmutableRef pinaforeEntityModel (fmap Known ef) fv2)

pinaforeSetGetOrdered ::
       forall a. (?pinafore :: PinaforeContext)
    => LangRefOrder a
    -> LangFiniteSetRef '( BottomType, a)
    -> LangWholeRef '( TopType, [a])
pinaforeSetGetOrdered order set = pinaforeROWRefToWholeRef $ qOrderSet order $ langFiniteSetRefFunctionValue set

pinaforeUpdateOrder :: LangRefOrder a -> UpdateOrder (ContextUpdate PinaforeStorageUpdate (WholeUpdate (Know a)))
pinaforeUpdateOrder (MkLangRefOrder m cmp) =
    MkUpdateOrder cmp $ changeLensToFloating $ pinaforeFunctionMorphismContextChangeLens m
