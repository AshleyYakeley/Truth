module Pinafore.Language.Library.GTK.Widget.Context where

import Changes.Core
import Changes.World.GNOME.GTK
import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Context
import Shapes

data SelectionModel =
    TextSelectionModel LangTextModel

data WidgetContext = MkWidgetContext
    { wcUnlift :: View --> IO
    , wcAccelGroup :: AccelGroup
    , wcOtherContext :: OtherContext
    , wcSelectNotify :: SelectNotify SelectionModel
    }

-- LangWidget
newtype LangWidget = MkLangWidget
    { unLangWidget :: WidgetContext -> GView 'Unlocked Widget
    }

widgetGroundType :: QGroundType '[] LangWidget
widgetGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangWidget)|]) "Widget.GTK."

instance HasQGroundType '[] LangWidget where
    qGroundType = widgetGroundType
