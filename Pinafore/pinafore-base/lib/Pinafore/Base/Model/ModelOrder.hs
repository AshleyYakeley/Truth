module Pinafore.Base.Model.ModelOrder where

import Changes.Core
import Shapes

import Pinafore.Base.Know
import Pinafore.Base.Model.FunctionAttribute
import Pinafore.Base.Model.ImmutableWholeModel
import Pinafore.Base.Model.LensAttribute
import Pinafore.Base.Model.Model
import Pinafore.Base.Model.ModelBased
import Pinafore.Base.Model.ModelProperty

data ModelOrder a update
    = forall t. MkModelOrder
        (StorageFunctionAttribute update (Know a) t)
        (Order t)

instance EditContraFunctor (ModelOrder a) where
    eaContraMap lens (MkModelOrder fm order) = MkModelOrder (mapStorageFunctionAttributeBase lens fm) order

instance Semigroup (ModelOrder a update) where
    MkModelOrder fa oa <> MkModelOrder fb ob = MkModelOrder (liftA2 (,) fa fb) $ oa <***> ob

instance Monoid (ModelOrder a update) where
    mempty = MkModelOrder (pure ()) rUnit

instance CatFunctor (CatDual (->)) (NestedMorphism (->)) ModelOrder where
    cfmap (MkCatDual f) = MkNestedMorphism $ \(MkModelOrder ef o) -> MkModelOrder (ef . (arr $ fmap f)) o

reverseModelOrder :: ModelOrder a update -> ModelOrder a update
reverseModelOrder (MkModelOrder ef o) = MkModelOrder ef $ reverseOrder o

modelOrderOn :: StorageLensAttribute ap aq bp bq update -> ModelOrder bq update -> ModelOrder ap update
modelOrderOn pm (MkModelOrder ef o) = MkModelOrder (ef . lensFunctionAttribute pm) o

type ModelModelOrder a = ModelBased (ModelOrder a)

pureModelModelOrder :: Order a -> ModelModelOrder a
pureModelModelOrder (MkOrder cmp) = pureModelBased $ MkModelOrder id $ MkOrder $ knowCompare cmp

mapModelModelOrder :: (b -> a) -> ModelModelOrder a -> ModelModelOrder b
mapModelModelOrder f = mapModelBased $ ccontramap1 f

reverseModelModelOrder :: ModelModelOrder a -> ModelModelOrder a
reverseModelModelOrder = mapModelBased reverseModelOrder

modelModelOrderOn :: ModelAttribute ap aq bp bq -> ModelModelOrder bq -> ModelModelOrder ap
modelModelOrderOn = combineModelBased modelOrderOn

modelModelOrderSet :: forall a. ModelModelOrder a -> WROWModel (ListSet a) -> WROWModel (Know [a])
modelModelOrderSet ro pset =
    modelBasedModel ro $ \model (MkModelOrder (ofunc :: StorageFunctionAttribute update (Know a) t) (oord :: Order t)) -> let
        upairs :: WROWModel (ListMap a t)
        upairs = applyStorageFunction model (liftListSetStorageFunctionAttribute $ ofunc . arr Known) pset
        sortpoints :: ListMap a t -> [a]
        sortpoints pairs = fmap fst $ orderSort (contramap snd oord) $ listMapToList pairs
        in eaMapReadOnlyWhole (Known . sortpoints) upairs

modelModelOrderCompare ::
    forall a. ModelModelOrder a -> ImmutableWholeModel a -> ImmutableWholeModel a -> ImmutableWholeModel Ordering
modelModelOrderCompare ro fv1 fv2 =
    modelBasedModel ro $ \model (MkModelOrder ef o) ->
        compareOrder o
            <$> (applyImmutableModel model (fmap Known ef) fv1)
            <*> (applyImmutableModel model (fmap Known ef) fv2)

modelRefUpdateOrder ::
    ModelModelOrder a ->
    (forall update. Model update -> UpdateOrder (ContextUpdate update (WholeUpdate (Know a))) -> r) ->
    r
modelRefUpdateOrder ro call =
    modelBasedModel ro $ \model (MkModelOrder m o) ->
        call model $ mkUpdateOrder (compareOrder o) $ storageFunctionAttributeContextChangeLens m
