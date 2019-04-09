module Truth.Core.UI.GetView where

import Truth.Core.Import
import Truth.Core.UI.CreateView
import Truth.Core.UI.Specifier.Map
import Truth.Core.UI.Specifier.Selection
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
             MkMapUISpec lens specb <- isUISpec speca
             return $ cvMapEdit lens $ getview specb) <|>
        (do
             MkMapSelectionUISpec lens specb <- isUISpec speca
             return $ cvMapSelection lens $ getview specb) <|>
        (do
             MkNoSelectionUISpec specb <- isUISpec speca
             return $ cvNoAspect $ getview specb) <|>
        (do
             MkWithAspectUISpec specf <- isUISpec speca
             return $ cvWithAspect (getview . specf))
