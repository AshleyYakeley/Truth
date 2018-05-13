module Truth.Core.UI.GetView where

import Truth.Core.Import
import Truth.Core.UI.CreateView
import Truth.Core.UI.Specifier.Lens
import Truth.Core.UI.Specifier.SelectionLens
import Truth.Core.UI.Specifier.Specifier

newtype GetView w = MkGetView
    { getUIView :: forall seledit edit.
                           (forall seledit' edit'. UISpec seledit' edit' -> CreateView seledit' edit' w) -> UISpec seledit edit -> Maybe (CreateView seledit edit w)
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
             MkUISetSelectionLens lens specb <- isUISpec speca
             return $ cvMapSetSelectionEdit lens $ getview specb) <|>
        (do
             MkUINoSelectionLens specb <- isUISpec speca
             return $ cvNoAspect $ getview specb)
