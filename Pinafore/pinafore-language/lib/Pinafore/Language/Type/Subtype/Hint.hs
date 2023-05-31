module Pinafore.Language.Type.Subtype.Hint
    ( QSubtypeHint
    , mkQSubtypeHint
    ) where

import Pinafore.Language.Name
import Shapes

data QSubtypeHint = MkQSubtypeHint
    { dhDomain :: [Name]
    , dhMap :: Name -> Name
    }

dhCodomain :: QSubtypeHint -> [Name]
dhCodomain MkQSubtypeHint {..} = fmap dhMap dhDomain

instance Eq QSubtypeHint where
    hintA == hintB =
        if dhDomain hintA == dhDomain hintB
            then dhCodomain hintA == dhCodomain hintB
            else error $ "inconsistent data hints: " <> show (dhDomain hintA, dhDomain hintB)

instance Semigroup QSubtypeHint where
    MkQSubtypeHint da ma <> MkQSubtypeHint _ mb = MkQSubtypeHint da $ mb . ma

mkFunc :: [(Name, Name)] -> Name -> Name
mkFunc [] n = error $ "broken subtype hint: " <> show n
mkFunc ((a, b):_) n
    | a == n = b
mkFunc (_:nn) n = mkFunc nn n

mkQSubtypeHint :: [(Name, Name)] -> QSubtypeHint
mkQSubtypeHint nn = MkQSubtypeHint (sort $ fmap fst nn) $ mkFunc nn
