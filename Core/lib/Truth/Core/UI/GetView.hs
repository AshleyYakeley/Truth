module Truth.Core.UI.GetView where
{
    import Truth.Core.Import;
    import Truth.Core.Edit;
    import Truth.Core.UI.Specifier;
    import Truth.Core.UI.View;
    import Truth.Core.UI.Lens;


    newtype GetView w = MkGetView {getUIView :: forall edit. Edit edit => (forall edit'. Edit edit' => UISpec edit' -> CreateView edit' w) -> UISpec edit -> Maybe (CreateView edit w)};

    instance Semigroup (GetView w) where
    {
        (MkGetView p) <> (MkGetView q) = MkGetView $ \getview uispec -> case p getview uispec of
        {
            Just view -> Just view;
            Nothing -> q getview uispec;
        }
    };

    instance Monoid (GetView w) where
    {
        mempty = MkGetView $ \_ _ -> Nothing;
        mappend = (<>);
    };

    lensGetView :: GetView w;
    lensGetView = MkGetView $ \getview speca -> do
    {
        MkUILens lens specb <- isUISpec speca;
        return $ mapCreateViewEdit lens $ getview specb;
    };
}
