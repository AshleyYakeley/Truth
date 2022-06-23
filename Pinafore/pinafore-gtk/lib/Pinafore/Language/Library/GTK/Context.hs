{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.Context where

import Changes.UI.GTK
import Pinafore.Language.API
import Shapes

contextGroundType :: PinaforeGroundType '[] GTKContext
contextGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily GTKContext)|]) "Context"

instance HasPinaforeGroundType '[] GTKContext where
    pinaforeGroundType = contextGroundType
