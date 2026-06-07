module Pinafore.Library.GTK.Widget.Drawing
    ( drawingStuff
    )
where

import Changes.Core
import Changes.World.GNOME.GTK
import Data.Shim
import Pinafore.API
import Pinafore.Library.Media
import Shapes

import Pinafore.Library.GTK.Context
import Pinafore.Library.GTK.Widget

newtype LangHandler a
    = MkLangHandler ((View --> IO) -> InputHandler a)
    deriving newtype (Semigroup, Monoid)

instance Contravariant LangHandler where
    contramap f (MkLangHandler h) = MkLangHandler $ \unlift -> contramap f $ h unlift

instance ContraFilterable LangHandler where
    contramapMaybe f (MkLangHandler h) = MkLangHandler $ \unlift -> contramapMaybe f $ h unlift

instance MaybeRepresentational LangHandler where
    maybeRepresentational = Nothing

instance HasVariance LangHandler where
    type VarianceOf LangHandler = 'Contravariance

instance HasQGroundType '[ContraCCRVariance] LangHandler where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangHandler)|]) "Handler.GTK."

langOnClick :: LangHandler (Action ())
langOnClick = MkLangHandler $ \unlift ->
    clickAction $ \_ _ action -> Just $ gvRunUnlocked $ gvLiftView $ viewRunAction unlift action

handlerFallThrough :: LangHandler A -> LangHandler A
handlerFallThrough (MkLangHandler uie) = MkLangHandler $ \evt -> inputFallThrough $ uie evt

uiDraw :: ImmutableWholeModel ((Natural, Natural) -> LangDrawing A) -> LangHandler A -> LangWidget
uiDraw model (MkLangHandler handler) =
    MkLangWidget $ \wc ->
        createCairo
            ( unWModel
                $ immutableWholeModelValue mempty
                $ fmap
                    ( \d (w, h) ->
                        unLangDrawing
                            $ d (toNaturalForce w, toNaturalForce h)
                    )
                    model
            )
            $ contramapMaybe listToMaybe
            $ handler
            $ wcUnlift wc

cofilterHandler :: (B -> Maybe A) -> LangHandler A -> LangHandler B
cofilterHandler = contramapMaybe

drawingStuff :: LibraryStuff
drawingStuff =
    headingBDS
        "Drawing"
        ""
        [ typeBDS "Handler" "Response to button-clicked events" (qSomeGroundType @_ @LangHandler) []
        , namespaceBDS "Handler"
            $ monoidEntries @(LangHandler A)
            <> [ valBDS "cofilter" "" cofilterHandler
               , valBDS "onClick" "Action to perform on click" langOnClick
               , valBDS "fallThrough" "Run the handler, but fall through to run handlers underneath." handlerFallThrough
               ]
        , namespaceBDS "Widget" [valBDS "draw" "Drawable widget" uiDraw]
        ]
