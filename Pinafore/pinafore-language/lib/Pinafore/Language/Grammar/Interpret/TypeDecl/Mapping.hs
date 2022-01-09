module Pinafore.Language.Grammar.Interpret.TypeDecl.Mapping where

import Pinafore.Language.Type
import Shapes

type Mapping :: Symbol -> Type -> Type
newtype Mapping n t =
    MkMapping (Kleisli Endo (UVarT n -> UVarT n) t)
    deriving (Semigroup, Monoid, IsoVariant, Summish, Productish)

varMapping :: forall (n :: Symbol). Mapping n (UVarT n)
varMapping = MkMapping $ Kleisli $ \ab -> Endo ab

runMapping :: Mapping n t -> (UVarT n -> UVarT n) -> t -> t
runMapping (MkMapping (Kleisli f)) ab = appEndo $ f ab

mapMapping :: ((p -> p) -> (q -> q)) -> Mapping n p -> Mapping n q
mapMapping ff (MkMapping (Kleisli f)) = MkMapping $ Kleisli $ \tt -> Endo $ ff $ appEndo $ f tt

joinMapping :: ((p -> p) -> (q -> q) -> (t -> t)) -> Mapping n p -> Mapping n q -> Mapping n t
joinMapping ff (MkMapping (Kleisli fp)) (MkMapping (Kleisli fq)) =
    MkMapping $ Kleisli $ \tt -> Endo $ ff (appEndo $ fp tt) (appEndo $ fq tt)
