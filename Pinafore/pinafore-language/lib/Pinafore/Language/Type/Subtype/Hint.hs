module Pinafore.Language.Type.Subtype.Hint where

import Pinafore.Language.Name
import Shapes

data QSubtypeHint = MkQSubtypeHint
    { dhDomain :: [Name]
    , dhMap :: Name -> Name
    }

instance Eq QSubtypeHint where
    MkQSubtypeHint da ma == MkQSubtypeHint db mb =
        if da == db
            then fmap ma da == fmap mb db
            else error $ "inconsistent data hints: " <> show (da, db)

instance Semigroup QSubtypeHint where
    MkQSubtypeHint da ma <> MkQSubtypeHint _ mb = MkQSubtypeHint da $ mb . ma
