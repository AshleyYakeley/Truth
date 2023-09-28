module Control.Applicative.Wrapped where

import Shapes.Import

-- | An 'Applicative' that wraps a 'Monad'.
class (Monad (WAInnerM s), Applicative s) => WrappedApplicative s where
    type WAInnerM s :: Type -> Type
    wexec :: forall a. WAInnerM s (s a) -> s a
    whoist :: (forall a. WAInnerM s a -> WAInnerM s a) -> s b -> s b

wbind :: WrappedApplicative s => WAInnerM s a -> (a -> s b) -> s b
wbind ma asb = wexec $ fmap asb ma

wdo :: WrappedApplicative s => WAInnerM s () -> s a -> s a
wdo mu sa = wbind mu $ \() -> sa

wlift :: WrappedApplicative s => WAInnerM s a -> s a
wlift ma = wbind ma pure
