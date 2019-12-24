module Truth.Core.UI.GetView where

import Truth.Core.Import
import Truth.Core.UI.CreateView
import Truth.Core.UI.Specifier.Map
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.Specifier.WithAspect

newtype GetView w = MkGetView
    { getUIView :: forall sel. (forall sel'. UISpec sel' -> CreateView sel' w) -> UISpec sel -> Maybe (CreateView sel w)
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
             MkMapUISpec cv specb <- isUISpec speca
             return $ cv $ getview specb) <|>
        (do
             MkWithAspectUISpec specf <- isUISpec speca
             return $ cvWithAspect (getview . specf))
