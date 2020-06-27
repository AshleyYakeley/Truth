module Control.Applicative.Compose where

import Shapes.Import

newtype ComposeA inner outer a = MkComposeA
    { getComposeA :: outer (inner a)
    }

instance (Functor inner, Functor outer) => Functor (ComposeA inner outer) where
    fmap ab (MkComposeA oia) = MkComposeA $ fmap (fmap ab) oia

instance (Applicative inner, Applicative outer) => Applicative (ComposeA inner outer) where
    pure a = MkComposeA $ pure $ pure a
    MkComposeA mab <*> MkComposeA ma = MkComposeA $ liftA2 (<*>) mab ma

instance (Alternative inner, Applicative outer) => Alternative (ComposeA inner outer) where
    empty = MkComposeA $ pure empty
    MkComposeA oia <|> MkComposeA oib = MkComposeA $ liftA2 (<|>) oia oib

liftAOuter :: (Functor outer, Applicative inner) => outer a -> ComposeA inner outer a
liftAOuter ma = MkComposeA $ fmap pure ma

liftAInner :: Applicative outer => inner a -> ComposeA inner outer a
liftAInner na = MkComposeA $ pure na
