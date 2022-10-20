module Pinafore.Language.Library.GTK.Element.Context where

import Changes.Core
import Changes.World.GNOME.GTK
import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Context
import Shapes

data SelectionModel =
    TextSelectionModel LangTextModel

data ElementContext = MkElementContext
    { ecUnlift :: View --> IO
    , ecAccelGroup :: AccelGroup
    , ecOtherContext :: OtherContext
    , ecSelectNotify :: SelectNotify SelectionModel
    }

-- LangElement
newtype LangElement = MkLangElement
    { unLangElement :: ElementContext -> GView 'Locked Widget
    }

elementGroundType :: QGroundType '[] LangElement
elementGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangElement)|]) "Element"

instance HasQGroundType '[] LangElement where
    qGroundType = elementGroundType
