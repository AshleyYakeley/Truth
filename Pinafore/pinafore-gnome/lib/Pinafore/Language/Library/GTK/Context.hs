{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.Context where

import Changes.Core
import Changes.World.GNOME.GTK
import Pinafore.Base
import Pinafore.Language.API
import Shapes

data OtherContext = MkOtherContext
    { ocClipboard :: Model (WholeUpdate (Maybe Clip))
    }

data LangContext = MkLangContext
    { lcGTKContext :: GTKContext
    , lcOtherContext :: OtherContext
    }

contextGroundType :: PinaforeGroundType '[] LangContext
contextGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangContext)|]) "Context"

instance HasPinaforeGroundType '[] LangContext where
    pinaforeGroundType = contextGroundType

gvRunAction :: (?pinafore :: PinaforeContext) => (View --> IO) -> PinaforeAction () -> GView 'Locked ()
gvRunAction unlift pa = gvRunUnlockedIO $ unlift $ runPinaforeAction pa

gvRunActionDefault :: (?pinafore :: PinaforeContext) => (View --> IO) -> a -> PinaforeAction a -> GView 'Locked a
gvRunActionDefault unlift a pa = fmap (fromKnow a) $ gvRunUnlockedIO $ unlift $ unliftPinaforeAction pa
