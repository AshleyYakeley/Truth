module Pinafore.Base.ModelOrder where

import Changes.Core
import Pinafore.Base.FunctionMorphism
import Pinafore.Base.ImmutableWholeModel
import Pinafore.Base.Know
import Pinafore.Base.ModelBased
import Pinafore.Base.ModelMorphism
import Pinafore.Base.Morphism
import Pinafore.Base.Order
import Pinafore.Base.Ref
import Shapes

data ModelOrder a update =
    forall t. MkModelOrder (PinaforeFunctionMorphism update (Know a) t)
                           (Order t)

instance EditContraFunctor (ModelOrder a) where
    eaContraMap lens (MkModelOrder fm order) = MkModelOrder (mapPinaforeFunctionMorphismBase lens fm) order

instance Semigroup (ModelOrder a update) where
    MkModelOrder fa oa <> MkModelOrder fb ob =
        MkModelOrder (liftA2 (,) fa fb) $ \(a1, b1) (a2, b2) -> joinOrderings (oa a1 a2) (ob b1 b2)

instance Monoid (ModelOrder a update) where
    mempty = MkModelOrder (pure ()) $ compare @()
    mappend = (<>)

instance CatFunctor (CatDual (->)) (NestedMorphism (->)) ModelOrder where
    cfmap (MkCatDual f) = MkNestedMorphism $ \(MkModelOrder ef o) -> MkModelOrder (ef . (arr $ fmap f)) o

reverseModelOrder :: ModelOrder a update -> ModelOrder a update
reverseModelOrder (MkModelOrder ef o) = MkModelOrder ef $ reverseOrder o

modelOrderOn :: PinaforeLensMorphism ap aq bp bq update -> ModelOrder bq update -> ModelOrder ap update
modelOrderOn pm (MkModelOrder ef o) = MkModelOrder (ef . lensFunctionMorphism pm) o

type ModelModelOrder a = ModelBased (ModelOrder a)

pureModelModelOrder :: Order a -> ModelModelOrder a
pureModelModelOrder cmp = pureModelBased $ MkModelOrder id $ knowOrder cmp

mapModelModelOrder :: (b -> a) -> ModelModelOrder a -> ModelModelOrder b
mapModelModelOrder f = mapModelBased $ ccontramap1 f

reverseModelModelOrder :: ModelModelOrder a -> ModelModelOrder a
reverseModelModelOrder = mapModelBased reverseModelOrder

modelModelOrderOn :: ModelMorphism ap aq bp bq -> ModelModelOrder bq -> ModelModelOrder ap
modelModelOrderOn = combineModelBased modelOrderOn

modelModelOrderSet :: ModelModelOrder a -> PinaforeROWModel (FiniteSet a) -> PinaforeROWModel (Know [a])
modelModelOrderSet ro pset =
    modelBasedModel ro $ \model (MkModelOrder (ofunc :: PinaforeFunctionMorphism update (Know a) t) oord) -> let
        cmp :: Order (a, t)
        cmp (_, t1) (_, t2) = oord t1 t2
        ofuncpair :: PinaforeFunctionMorphism update a (a, t)
        ofuncpair =
            proc a -> do
                kt <- ofunc -< Known a
                returnA -< (a, kt)
        upairs :: PinaforeROWModel (FiniteSet (a, t))
        upairs = applyPinaforeFunction model (cfmap ofuncpair) pset
        sortpoints :: FiniteSet (a, t) -> [a]
        sortpoints (MkFiniteSet pairs) = fmap fst $ sortBy cmp pairs
        in eaMapReadOnlyWhole (Known . sortpoints) upairs

modelModelOrderCompare ::
       forall a.
       ModelModelOrder a
    -> PinaforeImmutableWholeModel a
    -> PinaforeImmutableWholeModel a
    -> PinaforeImmutableWholeModel Ordering
modelModelOrderCompare ro fv1 fv2 =
    modelBasedModel ro $ \model (MkModelOrder ef o) ->
        o <$> (applyImmutableModel model (fmap Known ef) fv1) <*> (applyImmutableModel model (fmap Known ef) fv2)

modelRefUpdateOrder ::
       ModelModelOrder a
    -> (forall update. Model update -> UpdateOrder (ContextUpdate update (WholeUpdate (Know a))) -> r)
    -> r
modelRefUpdateOrder ro call =
    modelBasedModel ro $ \model (MkModelOrder m cmp) ->
        call model $ mkUpdateOrder cmp $ pinaforeFunctionMorphismContextChangeLens m
