{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.Context where

import Changes.Core
import Changes.UI.GTK
import Pinafore.Base
import Pinafore.Language.API
import Shapes

contextGroundType :: PinaforeGroundType '[] GTKContext
contextGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily GTKContext)|]) "Context"

instance HasPinaforeGroundType '[] GTKContext where
    pinaforeGroundType = contextGroundType

gvRunAction :: (?pinafore :: PinaforeContext) => (View --> IO) -> PinaforeAction () -> GView 'Locked ()
gvRunAction unlift pa = gvRunUnlockedIO $ unlift $ runPinaforeAction pa
