module Pinafore.Base.Model.ModelOrder where

import Changes.Core
import Pinafore.Base.Know
import Pinafore.Base.Model.FunctionMorphism
import Pinafore.Base.Model.ImmutableWholeModel
import Pinafore.Base.Model.Model
import Pinafore.Base.Model.ModelBased
import Pinafore.Base.Model.ModelMorphism
import Pinafore.Base.Model.Morphism
import Pinafore.Base.Order
import Shapes

data ModelOrder a update =
    forall t. MkModelOrder (StorageFunctionMorphism update (Know a) t)
                           (Order t)

instance EditContraFunctor (ModelOrder a) where
    eaContraMap lens (MkModelOrder fm order) = MkModelOrder (mapStorageFunctionMorphismBase lens fm) order

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

modelOrderOn :: StorageLensMorphism ap aq bp bq update -> ModelOrder bq update -> ModelOrder ap update
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

modelModelOrderSet :: ModelModelOrder a -> WROWModel (FiniteSet a) -> WROWModel (Know [a])
modelModelOrderSet ro pset =
    modelBasedModel ro $ \model (MkModelOrder (ofunc :: StorageFunctionMorphism update (Know a) t) oord) -> let
        cmp :: Order (a, t)
        cmp (_, t1) (_, t2) = oord t1 t2
        ofuncpair :: StorageFunctionMorphism update a (a, t)
        ofuncpair =
            proc a -> do
                kt <- ofunc -< Known a
                returnA -< (a, kt)
        upairs :: WROWModel (FiniteSet (a, t))
        upairs = applyStorageFunction model (cfmap ofuncpair) pset
        sortpoints :: FiniteSet (a, t) -> [a]
        sortpoints (MkFiniteSet pairs) = fmap fst $ sortBy cmp pairs
        in eaMapReadOnlyWhole (Known . sortpoints) upairs

modelModelOrderCompare ::
       forall a. ModelModelOrder a -> ImmutableWholeModel a -> ImmutableWholeModel a -> ImmutableWholeModel Ordering
modelModelOrderCompare ro fv1 fv2 =
    modelBasedModel ro $ \model (MkModelOrder ef o) ->
        o <$> (applyImmutableModel model (fmap Known ef) fv1) <*> (applyImmutableModel model (fmap Known ef) fv2)

modelRefUpdateOrder ::
       ModelModelOrder a
    -> (forall update. Model update -> UpdateOrder (ContextUpdate update (WholeUpdate (Know a))) -> r)
    -> r
modelRefUpdateOrder ro call =
    modelBasedModel ro $ \model (MkModelOrder m cmp) ->
        call model $ mkUpdateOrder cmp $ storageFunctionMorphismContextChangeLens m
