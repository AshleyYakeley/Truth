{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.Context where

import Changes.Core
import Changes.World.GNOME.GTK
import Pinafore.API
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

viewUnlift :: (View --> IO) -> View --> View
viewUnlift unlift va = liftIO $ unlift va

viewRunAction :: (View --> IO) -> Action () -> View ()
viewRunAction unlift pa = viewUnlift unlift $ runAction pa

viewRunActionDefault :: (View --> IO) -> a -> Action a -> View a
viewRunActionDefault unlift a pa = viewUnlift unlift $ fmap (fromKnow a) $ unliftAction pa
