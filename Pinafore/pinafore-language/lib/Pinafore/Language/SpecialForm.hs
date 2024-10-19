module Pinafore.Language.SpecialForm where

import Import
import Pinafore.Language.Type.Ground

type QAnnotation :: Type -> Type
data QAnnotation t where
    AnnotAnchor :: QAnnotation Anchor
    AnnotType :: QAnnotation (Some QNonpolarType)
