module Control.Applicative.Wrapped where

import Shapes

class (Monad (WAWrapper s), Applicative s) => WrappedApplicative s where
    type WAWrapper s :: Type -> Type
    wexec :: forall a. WAWrapper s (s a) -> s a

wbind :: WrappedApplicative s => WAWrapper s a -> (a -> s b) -> s b
wbind ma asb = wexec $ fmap asb ma

wlift :: WrappedApplicative s => WAWrapper s a -> s a
wlift ma = wbind ma pure
