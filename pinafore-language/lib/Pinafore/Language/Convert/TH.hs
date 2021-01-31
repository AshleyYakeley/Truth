{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert.TH where

import Pinafore.Language.Shim
import Pinafore.Language.Type
import Shapes

-- Literal
literalInstances :: _ -> _
literalInstances t =
    [d|
  
  instance ToShimWit (PinaforePolyShim Type)
             (PinaforeSingularType 'Positive)
             $( t )
           where
          toShimWit
            = mkShimWit $
                GroundDolanSingularType
                  (EntityPinaforeGroundType NilListType $
                     LiteralEntityGroundType representative)
                  NilDolanArguments
  
  instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive)
             $( t )
           where
          toShimWit = singleDolanShimWit toJMShimWit
  
  instance FromShimWit (PinaforePolyShim Type)
             (PinaforeSingularType 'Negative)
             $( t )
           where
          fromShimWit
            = mkShimWit $
                GroundDolanSingularType
                  (EntityPinaforeGroundType NilListType $
                     LiteralEntityGroundType representative)
                  NilDolanArguments
  
  instance FromShimWit (PinaforePolyShim Type)
             (PinaforeType 'Negative)
             $( t )
           where
          fromShimWit = singleDolanShimWit fromJMShimWit
  |]
