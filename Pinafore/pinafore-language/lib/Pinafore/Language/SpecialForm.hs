module Pinafore.Language.SpecialForm where

import Import
import Pinafore.Language.Type.Ground

type QAnnotation :: Type -> Type
data QAnnotation t where
    AnnotAnchor :: QAnnotation Anchor
    AnnotNonpolarType :: QAnnotation (Some (TSNonpolarWitness QTypeSystem))
    AnnotPositiveType :: QAnnotation (Some (QType 'Positive))
    AnnotNegativeType :: QAnnotation (Some (QType 'Negative))
