module Pinafore.Language.SpecialForm where

import Language.Expression.Common
import Pinafore.Base
import Pinafore.Language.Type.Entity
import Pinafore.Language.Type.OpenEntity
import Shapes

type Annotation :: Type -> Type -> Type
data Annotation ts t where
    AnnotAnchor :: Annotation ts Anchor
    AnnotConcreteEntityType :: Annotation ts (AnyW ConcreteEntityType)
    AnnotOpenEntityType :: Annotation ts (AnyW OpenEntityType)
    AnnotPositiveType :: Annotation ts (AnyW (TSPosWitness ts))
    AnnotNegativeType :: Annotation ts (AnyW (TSNegWitness ts))

type SpecialForm :: Type -> (Type -> Type) -> Type
data SpecialForm ts m =
    forall lt. MkSpecialForm (ListType (Annotation ts) lt)
                             (HList lt -> m (TSValue ts))