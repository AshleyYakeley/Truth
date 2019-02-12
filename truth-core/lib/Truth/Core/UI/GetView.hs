module Truth.Core.UI.GetView where

import Data.IORef
import Truth.Core.Import
import Truth.Core.UI.CreateView
import Truth.Core.UI.Specifier.Lens
import Truth.Core.UI.Specifier.SelectionLens
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.Specifier.WithAspect

newtype GetView w = MkGetView
    { getUIView :: forall sel edit.
                           (forall sel' edit'. UISpec sel' edit' -> CreateView sel' edit' w) -> UISpec sel edit -> Maybe (CreateView sel edit w)
    }

instance Semigroup (GetView w) where
    (MkGetView p) <> (MkGetView q) =
        MkGetView $ \getview uispec ->
            case p getview uispec of
                Just view -> Just view
                Nothing -> q getview uispec

instance Monoid (GetView w) where
    mempty = MkGetView $ \_ _ -> Nothing
    mappend = (<>)

lensGetView :: GetView w
lensGetView =
    MkGetView $ \getview speca ->
        (do
             MkUILens lens specb <- isUISpec speca
             return $ cvMapEdit lens $ getview specb) <|>
        (do
             MkUISetSelectionMap lens specb <- isUISpec speca
             return $ cvMapSelection lens $ getview specb) <|>
        (do
             MkUINoSelection specb <- isUISpec speca
             return $ cvNoAspect $ getview specb) <|>
        (do
             MkUIWithAspect specf <- isUISpec speca
             return $ do
                 selref <- liftIO $ newIORef $ return Nothing
                 let
                     getsel :: Aspect _
                     getsel = do
                         asp <- readIORef selref
                         asp
                     updatesetsel :: (Aspect _ -> IO ()) -> (Aspect _ -> IO ())
                     updatesetsel setsel asp = do
                         writeIORef selref asp
                         setsel asp
                 (firstAspect, w) <- cvAccessAspect updatesetsel $ getview $ specf getsel
                 liftIO $ writeIORef selref firstAspect
                 return w)
