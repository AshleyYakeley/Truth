{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert.TH where

import Pinafore.Language.Shim
import Pinafore.Language.Type
import Shapes

-- Literal
literalInstances :: _ -> _
literalInstances t =
    [d|
  
  instance ToPolarShimWit (PinaforePolyShim Type)
             (PinaforeSingularType 'Positive)
             $( t )
           where
          toPolarShimWit
            = mkPolarShimWit $
                GroundDolanSingularType
                  (EntityPinaforeGroundType NilListType $
                     LiteralEntityGroundType representative)
                  NilDolanArguments
  
  instance ToPolarShimWit (PinaforePolyShim Type)
             (PinaforeType 'Positive)
             $( t )
           where
          toPolarShimWit = singleDolanShimWit toJMShimWit
  
  instance FromPolarShimWit (PinaforePolyShim Type)
             (PinaforeSingularType 'Negative)
             $( t )
           where
          fromPolarShimWit
            = mkPolarShimWit $
                GroundDolanSingularType
                  (EntityPinaforeGroundType NilListType $
                     LiteralEntityGroundType representative)
                  NilDolanArguments
  
  instance FromPolarShimWit (PinaforePolyShim Type)
             (PinaforeType 'Negative)
             $( t )
           where
          fromPolarShimWit = singleDolanShimWit fromJMShimWit
  |]
