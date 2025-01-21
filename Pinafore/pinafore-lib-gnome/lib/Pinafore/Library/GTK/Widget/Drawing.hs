module Pinafore.Library.GTK.Widget.Drawing
    ( drawingStuff
    )
where

import Changes.Core
import Changes.World.GNOME.GTK
import GI.Gdk as GI
import Pinafore.API
import Pinafore.Library.Media
import Shapes

import Pinafore.Library.GTK.Context
import Pinafore.Library.GTK.Widget.Context

newtype LangHandler
    = MkLangHandler (EventButton -> Action Bool)

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

runLangHandler :: WidgetContext -> LangHandler -> UIEvents
runLangHandler ec (MkLangHandler action) =
    MkUIEvents $ \eb -> gvRunUnlocked $ gvLiftView $ viewRunActionDefault (wcUnlift ec) True $ action eb

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

uiDraw :: ImmutableWholeModel ((Natural, Natural) -> LangDrawing LangHandler) -> LangWidget
uiDraw model =
    MkLangWidget $ \ec ->
        createCairo
            $ unWModel
            $ immutableWholeModelValue mempty
            $ fmap
                ( \d (w, h) ->
                    fmap (\f pp -> runLangHandler ec $ mconcat $ f pp)
                        $ unLangDrawing (d (toNaturalForce w, toNaturalForce h))
                )
                model

drawingStuff :: LibraryStuff
drawingStuff =
    headingBDS
        "Drawing"
        ""
        [ typeBDS "Handler" "Response to button-clicked events" (MkSomeGroundType handlerGroundType) []
        , namespaceBDS "Handler"
            $ monoidEntries @LangHandler
            <> [ valBDS "onClick" "Action to perform on click" langOnClick
               , valBDS "fallThrough" "Run the handler, but fall through to run handlers underneath." handlerFallThrough
               ]
        , namespaceBDS "Widget" [valBDS "draw" "Drawable widget" uiDraw]
        ]
