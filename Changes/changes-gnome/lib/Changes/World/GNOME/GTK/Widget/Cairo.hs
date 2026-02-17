module Changes.World.GNOME.GTK.Widget.Cairo
    ( InputType (..)
    , InputActions
    , inputAction
    , inputFallThrough
    , createCairo
    )
where

import Data.IORef
import Graphics.Cairo.Functional

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI
import GI.Cairo.Render.Connector qualified as GI




data InputType loc evt where
    ClickInputType :: InputType loc (Int32,[GI.ModifierType],loc)

instance TestEquality (InputType loc) where
    testEquality ClickInputType ClickInputType = Just Refl

newtype InputActions loc = MkInputActions (forall evt. InputType loc evt -> Maybe (evt -> GView 'Locked Bool))

instance Semigroup (InputActions loc) where
    MkInputActions evta <> MkInputActions evtb =
        MkInputActions $ \et -> case (evta et,evtb et) of
            (Nothing,Nothing) -> Nothing
            (fa,fb) -> Just $ \d -> do
                sa <- fromMaybe (\_ -> return False) fa d
                if sa
                    then return True
                    else fromMaybe (\_ -> return False) fb d

instance Monoid (InputActions loc) where
    mempty = MkInputActions $ \_ -> Nothing

inputAction :: forall loc evt. InputType loc evt -> (evt -> GView 'Locked ()) -> InputActions loc
inputAction gtype action = MkInputActions $ \gt -> do
    Refl <- testEquality gt gtype
    return $ \evt -> do
        action evt
        return True

inputFallThrough :: InputActions loc -> InputActions loc
inputFallThrough (MkInputActions ga) = MkInputActions $ (fmap $ fmap $ fmap $ fmap $ \_ -> False) ga

createCairo :: forall a. Model (ROWUpdate ((Int32, Int32) -> Drawing (PixelPoint -> a))) -> InputActions a -> GView 'Unlocked GI.Widget
createCairo model (MkInputActions inputActions) = do
    drawingRef :: IORef ((Int32, Int32) -> Drawing (PixelPoint -> a)) <- gvLiftIONoUI $ newIORef $ \_ -> pure $ \_ -> error "createCairo: unset"
    gvRunLockedThen $ do
        (drawingArea, widget) <- gvNewWidget GI.DrawingArea []
        let
            getDrawing :: IO (Drawing (PixelPoint -> a))
            getDrawing = do
                pdrawing <- readIORef drawingRef
                w <- #getAllocatedWidth drawingArea
                h <- #getAllocatedHeight drawingArea
                return $ pdrawing (w, h)
        GI.drawingAreaSetDrawFunc drawingArea $ Just $ \_ context _ _ -> do
            drawing <- getDrawing
            GI.renderWithContext (drawingRender drawing) context
        for_ (inputActions ClickInputType) $ \action -> do
            evc <- gvNew GI.GestureClick []
            #addController widget evc
            _ <-
                gvOnSignal evc #released $ \nPress h v -> do
                    mevent <- #getCurrentEvent evc
                    for_ mevent $ \event -> do
                        drawing <- liftIO getDrawing
                        modifiers <- #getModifierState event
                        _ <- action (nPress,modifiers,drawingPoint drawing (h, v))
                        return ()
            return ()
        return $ do
            gvBindReadOnlyWholeModel model $ \pdrawing ->
                gvLiftIONoUI $ do
                    writeIORef drawingRef pdrawing
                    #queueDraw drawingArea
            return widget
