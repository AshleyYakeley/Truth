module Pinafore.Base.ModelOrder where

import Changes.Core
import Pinafore.Base.FunctionMorphism
import Pinafore.Base.ImmutableWholeRef
import Pinafore.Base.Know
import Pinafore.Base.ModelBased
import Pinafore.Base.ModelMorphism
import Pinafore.Base.Morphism
import Pinafore.Base.Order
import Pinafore.Base.Ref
import Shapes

data RefOrder a update =
    forall t. MkRefOrder (PinaforeFunctionMorphism update (Know a) t)
                         (Order t)

instance EditContraFunctor (RefOrder a) where
    eaContraMap lens (MkRefOrder fm order) = MkRefOrder (mapPinaforeFunctionMorphismBase lens fm) order

instance Semigroup (RefOrder a update) where
    MkRefOrder fa oa <> MkRefOrder fb ob =
        MkRefOrder (liftA2 (,) fa fb) $ \(a1, b1) (a2, b2) -> joinOrderings (oa a1 a2) (ob b1 b2)

instance Monoid (RefOrder a update) where
    mempty = MkRefOrder (pure ()) $ compare @()
    mappend = (<>)

instance CatFunctor (CatDual (->)) (NestedMorphism (->)) RefOrder where
    cfmap (MkCatDual f) = MkNestedMorphism $ \(MkRefOrder ef o) -> MkRefOrder (ef . (arr $ fmap f)) o

reverseRefOrder :: RefOrder a update -> RefOrder a update
reverseRefOrder (MkRefOrder ef o) = MkRefOrder ef $ reverseOrder o

refOrderOn :: PinaforeLensMorphism ap aq bp bq update -> RefOrder bq update -> RefOrder ap update
refOrderOn pm (MkRefOrder ef o) = MkRefOrder (ef . lensFunctionMorphism pm) o

type ModelRefOrder a = ModelBased (RefOrder a)

pureModelRefOrder :: Order a -> ModelRefOrder a
pureModelRefOrder cmp = pureModelBased $ MkRefOrder id $ knowOrder cmp

mapModelRefOrder :: (b -> a) -> ModelRefOrder a -> ModelRefOrder b
mapModelRefOrder f = mapModelBased $ ccontramap1 f

reverseModelRefOrder :: ModelRefOrder a -> ModelRefOrder a
reverseModelRefOrder = mapModelBased reverseRefOrder

modelRefOrderOn :: ModelMorphism ap aq bp bq -> ModelRefOrder bq -> ModelRefOrder ap
modelRefOrderOn = combineModelBased refOrderOn

modelRefOrderSet :: ModelRefOrder a -> PinaforeROWRef (FiniteSet a) -> PinaforeROWRef (Know [a])
modelRefOrderSet ro pset =
    modelBasedModel ro $ \model (MkRefOrder (ofunc :: PinaforeFunctionMorphism update (Know a) t) oord) -> let
        cmp :: Order (a, t)
        cmp (_, t1) (_, t2) = oord t1 t2
        ofuncpair :: PinaforeFunctionMorphism update a (a, t)
        ofuncpair =
            proc a -> do
                kt <- ofunc -< Known a
                returnA -< (a, kt)
        upairs :: PinaforeROWRef (FiniteSet (a, t))
        upairs = applyPinaforeFunction model (cfmap ofuncpair) pset
        sortpoints :: FiniteSet (a, t) -> [a]
        sortpoints (MkFiniteSet pairs) = fmap fst $ sortBy cmp pairs
        in eaMapReadOnlyWhole (Known . sortpoints) upairs

modelRefOrderCompare ::
       forall a.
       ModelRefOrder a
    -> PinaforeImmutableWholeRef a
    -> PinaforeImmutableWholeRef a
    -> PinaforeImmutableWholeRef Ordering
modelRefOrderCompare ro fv1 fv2 =
    modelBasedModel ro $ \model (MkRefOrder ef o) ->
        o <$> (applyImmutableRef model (fmap Known ef) fv1) <*> (applyImmutableRef model (fmap Known ef) fv2)

modelRefUpdateOrder ::
       ModelRefOrder a
    -> (forall update. Model update -> UpdateOrder (ContextUpdate update (WholeUpdate (Know a))) -> r)
    -> r
modelRefUpdateOrder ro call =
    modelBasedModel ro $ \model (MkRefOrder m cmp) ->
        call model $ mkUpdateOrder cmp $ pinaforeFunctionMorphismContextChangeLens m
