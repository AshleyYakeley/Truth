module Pinafore.Language.SpecialForm where

import Language.Expression.Common
import Pinafore.Base
import Shapes

type Annotation :: Type -> Type -> Type
data Annotation ts t where
    AnnotAnchor :: Annotation ts Anchor
    AnnotPositiveType :: Annotation ts (Some (TSPosWitness ts))
    AnnotNegativeType :: Annotation ts (Some (TSNegWitness ts))

type SpecialForm :: Type -> (Type -> Type) -> Type
data SpecialForm ts m =
    forall lt. MkSpecialForm (ListType (Annotation ts) lt)
                             (ListProduct lt -> m (TSValue ts))
