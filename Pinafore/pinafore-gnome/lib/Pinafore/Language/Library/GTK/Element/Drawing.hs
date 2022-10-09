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
    MkLangHandler (EventButton -> PinaforeAction Bool)

instance Semigroup LangHandler where
    MkLangHandler evta <> MkLangHandler evtb =
        MkLangHandler $ \et -> do
            sa <- evta et
            if sa
                then return True
                else evtb et

instance Monoid LangHandler where
    mempty = MkLangHandler $ \_ -> return False

handlerGroundType :: PinaforeGroundType '[] LangHandler
handlerGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangHandler)|]) "Handler"

instance HasPinaforeGroundType '[] LangHandler where
    pinaforeGroundType = handlerGroundType

runLangHandler :: (?pinafore :: PinaforeContext) => ElementContext -> LangHandler -> UIEvents
runLangHandler ec (MkLangHandler action) = MkUIEvents $ \eb -> gvRunActionDefault (ecUnlift ec) True $ action eb

langOnClick :: PinaforeAction () -> LangHandler
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

uiDraw ::
       (?pinafore :: PinaforeContext)
    => PinaforeImmutableWholeModel ((Int32, Int32) -> LangDrawing LangHandler)
    -> LangElement
uiDraw model =
    MkLangElement $ \ec ->
        createCairo $
        unWModel $
        pinaforeImmutableModelValue mempty $
        fmap (\d p -> fmap (\f pp -> runLangHandler ec $ mconcat $ f pp) $ unLangDrawing (d p)) model

drawingStuff :: DocTreeEntry BindDoc
drawingStuff =
    docTreeEntry
        "Drawing"
        ""
        [ mkTypeEntry "Handler" "Response to button-clicked events" $ MkSomeGroundType handlerGroundType
        , mkValEntry "concatHandler" "Collect handlers." $ mconcat @LangHandler
        , mkValEntry "onClick" "Action to perform on click" langOnClick
        , mkValEntry "fallThrough" "Run the handler, but fall through to run handlers underneath." handlerFallThrough
        , mkValEntry "draw" "Drawable element" uiDraw
        ]
