{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.Window
    ( windowStuff
    ) where

import Changes.Core
import Changes.UI.GTK
import Changes.World.MIME
import Data.Shim
import Pinafore.Base
import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Context
import Pinafore.Language.Library.GTK.Element
import Shapes

-- LangWindow
data LangWindow = MkLangWindow
    { lwContext :: GTKContext
    , lwClose :: GView 'Locked ()
    , lwWindow :: UIWindow
    }

windowGroundType :: PinaforeGroundType '[] LangWindow
windowGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangWindow)|]) "Window"

instance HasPinaforeGroundType '[] LangWindow where
    pinaforeGroundType = windowGroundType

-- UIWindow
instance HasPinaforeType 'Negative UIWindow where
    pinaforeType = mapNegShimWit (functionToShim "lwWindow" lwWindow) pinaforeType

createLangWindow :: GTKContext -> WindowSpec -> View LangWindow
createLangWindow gtkc uiw = do
    (lwWindow, wclose) <- runGView gtkc $ gvRunLocked $ gvGetCloser $ createWindow uiw
    let lwContext = gtkc
    let lwClose = wclose
    return $ MkLangWindow {..}

uiWindowClose :: LangWindow -> View ()
uiWindowClose MkLangWindow {..} = runGView lwContext $ gvRunLocked lwClose

openWindow ::
       (?pinafore :: PinaforeContext)
    => GTKContext
    -> (Int32, Int32)
    -> PinaforeImmutableWholeRef Text
    -> LangElement
    -> PinaforeAction LangWindow
openWindow gtkc wsSize title (MkLangElement element) =
    actionLiftView $
    mfix $ \w ->
        liftIOWithUnlift $ \unlift ->
            unlift $
            createLangWindow gtkc $ let
                wsPosition = WindowPositionCenter
                wsCloseBoxAction :: GView 'Locked ()
                wsCloseBoxAction = lwClose w
                wsTitle :: Model (ROWUpdate Text)
                wsTitle = unWModel $ eaMapReadOnlyWhole (fromKnow mempty) $ immutableRefToReadOnlyRef title
                wsContent :: AccelGroup -> GView 'Locked Widget
                wsContent ag = element MkElementContext {ecUnlift = unlift, ecAccelGroup = ag}
                in MkWindowSpec {..}

exitUI :: GTKContext -> View ()
exitUI gtkc = runGView gtkc $ gvExitUI

showWindow :: LangWindow -> View ()
showWindow MkLangWindow {..} = runGView lwContext $ gvRunLocked $ uiWindowShow lwWindow

hideWindow :: LangWindow -> View ()
hideWindow MkLangWindow {..} = runGView lwContext $ gvRunLocked $ uiWindowHide lwWindow

run :: forall a. (GTKContext -> PinaforeAction a) -> PinaforeAction a
run call = actionTunnelView $ \unlift -> runGTKView $ \gtkc -> unlift $ call gtkc

literalConv :: Bijection (Maybe (Text, LazyByteString)) (Maybe Literal)
literalConv =
    MkIsomorphism
        { isoForwards =
              \mtb -> do
                  (mimetype, b) <- mtb
                  case splitWhen ((==) '/') mimetype of
                      [t, s] -> return $ MkMIMELiteral (MkMIMEContentType t s []) $ toStrict b
                      _ -> Nothing
        , isoBackwards =
              \ml -> do
                  l <- ml
                  case l of
                      MkMIMELiteral (MkMIMEContentType t s _) b -> return (t <> "/" <> s, fromStrict b)
                      _ -> Nothing
        }

langChooseFile :: GTKContext -> (Maybe (Text, Text) -> Bool) -> PinaforeAction (LangWholeRef '( Literal, Literal))
langChooseFile gtkc test = do
    f <- actionLiftViewKnow $ fmap maybeToKnow $ runGView gtkc $ gvRunLocked $ chooseFile test
    fref <- liftIO $ giFileReference f
    (model :: Model (MaybeUpdate (PairUpdate (WholeUpdate Text) ByteStringUpdate)), ()) <-
        actionLiftLifeCycle $ makeSharedModel $ reflectingPremodel fref
    return $ pinaforeRefToWholeRef $ eaMap (bijectionWholeChangeLens $ invert knowMaybe . literalConv) $ MkWModel model

windowStuff :: DocTreeEntry BindDoc
windowStuff =
    docTreeEntry
        "Window"
        ""
        [ mkTypeEntry "Context" "Context for GTK" $ MkBoundType contextGroundType
        , mkValEntry
              "run"
              "Call the provided function with a GTK context, after which run the GTK event loop until all windows are closed." $
          run @A
        , mkValEntry "chooseFile" "Run a dialog to choose a file." langChooseFile
        , mkTypeEntry "Window" "A user interface window." $ MkBoundType windowGroundType
        , mkValEntry "openWindow" "Open a new window with this size, title and element." openWindow
        , mkValEntry "closeWindow" "Close a window." uiWindowClose
        , mkValEntry "showWindow" "Show a window." showWindow
        , mkValEntry "hideWindow" "Hide a window." hideWindow
        , mkValEntry "exit" "Exit the user interface." exitUI
        ]
