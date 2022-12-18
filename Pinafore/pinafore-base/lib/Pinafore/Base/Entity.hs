module Pinafore.Base.Entity where

import Control.DeepSeq
import Pinafore.Base.Anchor
import Shapes

newtype Entity =
    MkEntity Anchor
    deriving newtype (Eq, Ord, Random, HasSerializer, NFData, Hashable)

instance Show Entity where
    show (MkEntity anchor) = show anchor

newEntity :: MonadIO m => m Entity
newEntity = liftIO randomIO

hashToEntity :: (forall r. (forall t. HasSerializer t => t -> r) -> [r]) -> Entity
hashToEntity f = MkEntity $ hashToAnchor f

checkEntity :: String -> Entity -> Entity
checkEntity s (MkEntity anchor) = MkEntity $ checkAnchor s anchor
