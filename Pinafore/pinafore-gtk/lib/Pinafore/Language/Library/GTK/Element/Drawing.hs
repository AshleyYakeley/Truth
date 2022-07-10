module Pinafore.Language.Library.GTK.Element.Drawing
    ( drawingStuff
    ) where

import Changes.Core
import Changes.UI.GTK
import Data.Shim
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

uiDraw ::
       (?pinafore :: PinaforeContext)
    => PinaforeImmutableWholeRef ((Int32, Int32) -> LangDrawing LangHandler)
    -> LangElement
uiDraw ref =
    MkLangElement $ \ec ->
        createCairo $
        unWModel $
        pinaforeImmutableRefValue mempty $ fmap (\d p -> fmap (fmap $ runLangHandler ec) $ unLangDrawing (d p)) ref

drawingStuff :: DocTreeEntry BindDoc
drawingStuff =
    docTreeEntry
        "Drawing"
        ""
        [ mkTypeEntry "Handler" "A user interface element is something that goes inside a window." $
          MkBoundType handlerGroundType
        , hasSubtypeRelationEntry @[LangHandler] @LangHandler "Monoidal relationship" $ functionToShim "mconcat" mconcat
        , mkValEntry "onClick" "Action to perform on click" langOnClick
        , mkValEntry "draw" "Drawable element" uiDraw
        ]
