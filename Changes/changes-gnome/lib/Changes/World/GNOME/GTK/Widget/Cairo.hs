module Changes.World.GNOME.GTK.Widget.Cairo
    ( InputType (..)
    , InputHandler
    , inputAction
    , clickAction
    , inputFallThrough
    , createCairo
    )
where

import Data.IORef
import GI.Cairo.Render.Connector qualified as GI
import Graphics.Cairo.Functional

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

data InputType loc evt where
    ClickInputType :: InputType loc (Word32, Set (GI.ModifierType), loc)

instance TestEquality (InputType loc) where
    testEquality ClickInputType ClickInputType = Just Refl

mapInputType :: (locA -> locB) -> InputType locA evtA -> (forall evtB. InputType locB evtB -> (evtA -> evtB) -> r) -> r
mapInputType locF itype call = case itype of
    ClickInputType -> call ClickInputType $ \(nPress, mods, loc) -> (nPress, mods, locF loc)

mapInputTypeF :: Functor f => (locA -> f locB) -> InputType locA evtA -> (forall evtB. InputType locB evtB -> (evtA -> f evtB) -> r) -> r
mapInputTypeF locF itype call = case itype of
    ClickInputType -> call ClickInputType $ \(nPress, mods, locA) -> fmap (\locB -> (nPress, mods, locB)) $ locF locA

newtype InputHandler loc = MkInputHandler (forall evt. InputType loc evt -> Maybe (evt -> GView 'Locked Bool))

instance Contravariant InputHandler where
    contramap ba (MkInputHandler iactionsA) = MkInputHandler $ \itypeB -> mapInputType ba itypeB $ \itypeA evtF ->
        fmap (ccontramap1 evtF) $ iactionsA itypeA

instance ContraFilterable InputHandler where
    contramapMaybe f (MkInputHandler iactionsB) = MkInputHandler $ \itypeA -> mapInputTypeF f itypeA $ \itypeB evtF ->
        fmap (\actionB evtA -> maybe (return False) actionB $ evtF evtA) $ iactionsB itypeB

instance Semigroup (InputHandler loc) where
    MkInputHandler evta <> MkInputHandler evtb =
        MkInputHandler $ \et -> case (evta et, evtb et) of
            (Nothing, Nothing) -> Nothing
            (fa, fb) -> Just $ \d -> do
                sa <- fromMaybe (\_ -> return False) fa d
                if sa
                    then return True
                    else fromMaybe (\_ -> return False) fb d

instance Monoid (InputHandler loc) where
    mempty = MkInputHandler $ \_ -> Nothing

inputAction :: forall loc evt. InputType loc evt -> (evt -> Maybe (GView 'Locked ())) -> InputHandler loc
inputAction gtype evtAction = MkInputHandler $ \gt -> do
    Refl <- testEquality gt gtype
    return $ \evt -> case evtAction evt of
        Just action -> do
            action
            return True
        Nothing -> return False

clickAction :: (Word32 -> Set (GI.ModifierType) -> loc -> Maybe (GView 'Locked ())) -> InputHandler loc
clickAction locAction = inputAction ClickInputType $ \(nClicks, mods, loc) ->
    locAction nClicks mods loc

inputFallThrough :: forall loc. InputHandler loc -> InputHandler loc
inputFallThrough (MkInputHandler ga) = MkInputHandler $ (fmap $ fmap $ fmap $ fmap $ \_ -> False) ga

createCairo :: forall a. Model (ROWUpdate ((Int32, Int32) -> Drawing (PixelPoint -> a))) -> InputHandler a -> GView 'Unlocked GI.Widget
createCairo model (MkInputHandler inputActions) = do
    drawingRef :: IORef ((Int32, Int32) -> Drawing (PixelPoint -> a)) <-
        gvLiftIOTrustMeNoUI $ newIORef $ \_ -> pure $ \_ -> error "createCairo: unset"
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
            _ <-
                gvOnSignal () evc #released $ \nPress h v -> do
                    drawing <- liftIO getDrawing
                    modifiers <- #getCurrentEventState evc
                    _ <- action (fromIntegral nPress, setFromList modifiers, drawingPoint drawing (h, v))
                    return ()
            evcTransferred <- gvDuplicateUnbound GI.GestureClick evc
            #addController widget evcTransferred
            return ()
        return $ do
            gvBindReadOnlyWholeModel model $ \pdrawing ->
                gvLiftIOTrustMeNoUI $ do
                    writeIORef drawingRef pdrawing
                    #queueDraw drawingArea
            return widget
