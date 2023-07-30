module Pinafore.Language.SpecialForm where

import Data.Shim
import Language.Expression.Common
import Pinafore.Base
import Pinafore.Language.Type.Ground
import Shapes

type QAnnotation :: Type -> Type
data QAnnotation t where
    AnnotAnchor :: QAnnotation Anchor
    AnnotNonpolarType :: QAnnotation (Some (TSNonpolarWitness QTypeSystem))
    AnnotPositiveType :: QAnnotation (Some (QType 'Positive))
    AnnotNegativeType :: QAnnotation (Some (QType 'Negative))
