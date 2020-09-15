module Pinafore.Language.Type.OpenEntity where

import Pinafore.Base
import Pinafore.Language.Name
import Pinafore.Language.Type.Identified
import Pinafore.Language.Type.Show
import Shapes

type OpenEntityType :: BigNat -> Type
data OpenEntityType tid =
    MkOpenEntityType Name
                     (TypeIDType tid)

instance TestEquality OpenEntityType where
    testEquality (MkOpenEntityType _ t1) (MkOpenEntityType _ t2) = do
        Refl <- testEquality t1 t2
        return Refl

instance ExprShow (OpenEntityType tid) where
    exprShowPrec (MkOpenEntityType n _) = exprShowPrec n

type OpenEntity :: BigNat -> Type
newtype OpenEntity tid = MkOpenEntity
    { unNamedEntity :: Entity
    } deriving (Eq, Random)
