module Pinafore.Base.ModelBased
    ( ModelBased
    , modelBasedModel
    , pureModelBased
    , storageModelBased
    , otherModelBased
    , mapModelBased
    , combineModelBased
    ) where

import Changes.Core
import Pinafore.Base.Edit
import Shapes

type ModelBaseType :: Type -> Type
data ModelBaseType update where
    StorageModelBaseType :: ModelBaseType QStorageUpdate
    OtherModelBaseType :: ModelBaseType update

instance TestEquality ModelBaseType where
    testEquality StorageModelBaseType StorageModelBaseType = Just Refl
    testEquality _ _ = Nothing

-- The purpose of this mechanism is optimisation, to reduce mapping of morphisms etc. in the common case when they're both on the same model.
-- The simpler correct but unoptimised eqivalent of this type would be
--     data ModelBased f = forall update. MkModelBased (Model update) (f update)
type ModelBased :: (Type -> Type) -> Type
data ModelBased f
    = NullModelBased (forall update. f update)
    | forall update. ProvidedModelBased (ModelBaseType update)
                                        (Model update)
                                        (f update)

modelBasedModel :: ModelBased f -> (forall update. Model update -> f update -> r) -> r
modelBasedModel (NullModelBased f) call = call unitModel f
modelBasedModel (ProvidedModelBased _ model f) call = call model f

pureModelBased :: (forall update. f update) -> ModelBased f
pureModelBased = NullModelBased

storageModelBased :: Model QStorageUpdate -> f QStorageUpdate -> ModelBased f
storageModelBased = ProvidedModelBased StorageModelBaseType

otherModelBased :: Model update -> f update -> ModelBased f
otherModelBased = ProvidedModelBased OtherModelBaseType

mapModelBased :: (forall update. f update -> g update) -> ModelBased f -> ModelBased g
mapModelBased mp (NullModelBased f) = NullModelBased $ mp f
mapModelBased mp (ProvidedModelBased mbt model f) = ProvidedModelBased mbt model $ mp f

combineModelBased ::
       forall f g h. (EditContraFunctor f, EditContraFunctor g)
    => (forall update. f update -> g update -> h update)
    -> ModelBased f
    -> ModelBased g
    -> ModelBased h
combineModelBased combine (NullModelBased f) mbg = mapModelBased (combine f) mbg
combineModelBased combine mbf (NullModelBased g) = mapModelBased (\f -> combine f g) mbf
combineModelBased combine (ProvidedModelBased t1 model1 f1) (ProvidedModelBased t2 model2 f2) =
    case testEquality t1 t2 of
        Just Refl -> ProvidedModelBased t1 model1 $ combine f1 f2
        Nothing -> let
            model12 = pairModels model1 model2
            f1' = eaContraMap (tupleChangeLens SelectFirst) f1
            f2' = eaContraMap (tupleChangeLens SelectSecond) f2
            in ProvidedModelBased OtherModelBaseType model12 $ combine f1' f2'

instance (EditContraFunctor f, forall update. Semigroup (f update)) => Semigroup (ModelBased f) where
    (<>) = combineModelBased (<>)

instance (EditContraFunctor f, forall update. Monoid (f update)) => Monoid (ModelBased f) where
    mempty = pureModelBased mempty
