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

contextGroundType :: QGroundType '[] LangContext
contextGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangContext)|]) "Context.GTK."

instance HasQGroundType '[] LangContext where
    qGroundType = contextGroundType

gvRunAction :: (View --> IO) -> Action () -> GView 'Locked ()
gvRunAction unlift pa = gvRunUnlockedIO $ unlift $ runAction pa

gvRunActionDefault :: (View --> IO) -> a -> Action a -> GView 'Locked a
gvRunActionDefault unlift a pa = fmap (fromKnow a) $ gvRunUnlockedIO $ unlift $ unliftAction pa
