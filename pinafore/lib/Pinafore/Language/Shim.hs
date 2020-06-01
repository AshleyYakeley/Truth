module Pinafore.Language.Shim where

import Data.Shim
import Language.Expression.Dolan
import Shapes

type PinaforeShim :: PolyMapKind
type PinaforeShim = JMShim

type PinaforeShimWit :: forall k. (Polarity -> k -> Type) -> Polarity -> k -> Type
type PinaforeShimWit (wit :: Polarity -> k -> Type) polarity = PShimWit (PinaforeShim k) wit polarity
