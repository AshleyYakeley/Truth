module Pinafore.Language.Library.GTK.Element.Drawing
    ( drawingStuff
    ) where

import Changes.Core
import Changes.World.GNOME.GTK
import GI.Gdk as GI
import Pinafore.Base
import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Context
import Pinafore.Language.Library.GTK.Element.Context
import Pinafore.Language.Library.Media
import Shapes

newtype LangHandler =
    MkLangHandler (EventButton -> Action Bool)

instance Semigroup LangHandler where
    MkLangHandler evta <> MkLangHandler evtb =
        MkLangHandler $ \et -> do
            sa <- evta et
            if sa
                then return True
                else evtb et

instance Monoid LangHandler where
    mempty = MkLangHandler $ \_ -> return False

handlerGroundType :: QGroundType '[] LangHandler
handlerGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangHandler)|]) "Handler.GTK."

instance HasQGroundType '[] LangHandler where
    qGroundType = handlerGroundType

runLangHandler :: ElementContext -> LangHandler -> UIEvents
runLangHandler ec (MkLangHandler action) = MkUIEvents $ \eb -> gvRunActionDefault (ecUnlift ec) True $ action eb

langOnClick :: Action () -> LangHandler
langOnClick action =
    MkLangHandler $ \event -> do
        click <- GI.get event #type
        case click of
            EventTypeButtonPress -> do
                action
                return True
            _ -> return False

handlerFallThrough :: LangHandler -> LangHandler
handlerFallThrough (MkLangHandler uie) = MkLangHandler $ \evt -> fmap (\_ -> False) $ uie evt

uiDraw :: ImmutableWholeModel ((Int32, Int32) -> LangDrawing LangHandler) -> LangElement
uiDraw model =
    MkLangElement $ \ec ->
        createCairo $
        unWModel $
        immutableWholeModelValue mempty $
        fmap (\d p -> fmap (\f pp -> runLangHandler ec $ mconcat $ f pp) $ unLangDrawing (d p)) model

drawingStuff :: BindDocTree ()
drawingStuff =
    headingBDT
        "Drawing"
        ""
        [ typeBDT "Handler" "Response to button-clicked events" (MkSomeGroundType handlerGroundType) []
        , valBDT "concatHandler" "Collect handlers." $ mconcat @LangHandler
        , valBDT "onClick" "Action to perform on click" langOnClick
        , valBDT "fallThrough" "Run the handler, but fall through to run handlers underneath." handlerFallThrough
        , valBDT "draw" "Drawable element" uiDraw
        ]
