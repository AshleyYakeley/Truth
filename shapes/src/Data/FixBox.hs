module Data.FixBox
    ( FixBox
    , mkFixBox
    , boxFix
    ) where

import Shapes.Import

data FixBox m =
    forall t. MkFixBox (t -> WMFunction m m)
                       (m t)

instance Applicative m => Semigroup (FixBox m) where
    MkFixBox twmma mta <> MkFixBox twmmb mtb = MkFixBox (\(~(a, b)) -> twmmb b . twmma a) $ liftA2 (,) mta mtb

instance Applicative m => Monoid (FixBox m) where
    mempty = MkFixBox (\_ -> id) $ pure ()

mkFixBox :: (t -> WMFunction m m) -> m t -> FixBox m
mkFixBox = MkFixBox

boxFix :: MonadFix m => [FixBox m] -> MFunction m m
boxFix boxes ma =
    case mconcat boxes of
        MkFixBox twmm mt -> do
            (_, a) <- mfix $ \(~(t, _)) -> runWMFunction (twmm t) $ liftA2 (,) mt ma
            return a
