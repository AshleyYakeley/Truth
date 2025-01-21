module Data.Text.Markup where

import Shapes.Import

data Markup a
    = MkMarkup
        [(Text, MarkupElement a)]
        Text

data MarkupElement a
    = MkMarkupElement
        a
        (Markup a)

instance Semigroup (Markup a) where
    MkMarkup aa at <> MkMarkup [] bt = MkMarkup aa (at <> bt)
    MkMarkup aa at <> MkMarkup ((bbt, bbx) : bbr) bt = MkMarkup (aa <> ((at <> bbt, bbx) : bbr)) bt

instance Monoid (Markup a) where
    mempty = MkMarkup mempty mempty

instance Functor Markup where
    fmap ab (MkMarkup ee t) = MkMarkup ((fmap $ fmap $ fmap ab) ee) t

instance Functor MarkupElement where
    fmap ab (MkMarkupElement a m) = MkMarkupElement (ab a) (fmap ab m)

markupElementStrip :: MarkupElement a -> Text
markupElementStrip (MkMarkupElement _ m) = markupStrip m

markupStrip :: Markup a -> Text
markupStrip (MkMarkup ee et) = concatmap (\(t, e) -> t <> markupElementStrip e) ee <> et

markupElementSerialise ::
    forall a b.
    Monoid b =>
    (a -> b -> b) ->
    (Char -> b) ->
    MarkupElement a ->
    b
markupElementSerialise abb cb (MkMarkupElement a m) = abb a $ markupSerialise abb cb m

markupSerialise ::
    forall a b.
    Monoid b =>
    (a -> b -> b) ->
    (Char -> b) ->
    Markup a ->
    b
markupSerialise abb cb (MkMarkup ee et) = let
    tb :: Text -> b
    tb t = concatmap cb $ otoList t
    in concatmap (\(t, e) -> tb t <> markupElementSerialise abb cb e) ee <> tb et

plainMarkup :: Text -> Markup a
plainMarkup t = MkMarkup mempty t

elementMarkup :: MarkupElement a -> Markup a
elementMarkup e = MkMarkup [(mempty, e)] mempty

tagMarkup :: a -> Markup a -> Markup a
tagMarkup a m = elementMarkup $ MkMarkupElement a m

markupMapM ::
    forall m a b.
    Monad m =>
    (a -> Markup b -> m (Markup b)) ->
    Markup a ->
    m (Markup b)
markupMapM f (MkMarkup ee et) = do
    ee' <-
        for ee $ \(t, MkMarkupElement a e) -> do
            e' <- markupMapM f e
            e'' <- f a e'
            return $ plainMarkup t <> e''
    return $ mconcat ee' <> plainMarkup et

markupMap :: forall a b. (a -> Markup b -> Markup b) -> Markup a -> Markup b
markupMap f ma = runIdentity $ markupMapM (\a mb -> Identity $ f a mb) ma
