module Pinafore.Language.Convert.HasType where

import Pinafore.Language.Shim
import Pinafore.Language.Type
import Shapes

type ToPinaforeType = ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive)

type FromPinaforeType = FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative)
