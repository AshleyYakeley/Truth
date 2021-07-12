module Data.FixBox
    ( FixBox
    , mkFixBox
    , boxFix
    , boxesFix
    , boxSeq
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

boxFix :: MonadFix m => FixBox m -> MFunction m m
boxFix (MkFixBox twmm mt) ma = do
    (_, a) <- mfix $ \(~(t, _)) -> runWMFunction (twmm t) $ liftA2 (,) mt ma
    return a

boxesFix :: MonadFix m => [FixBox m] -> MFunction m m
boxesFix boxes = boxFix $ mconcat boxes

boxSeq :: Monad m => FixBox m -> MFunction m m
boxSeq (MkFixBox twmm mt) ma = do
    t <- mt
    runWMFunction (twmm t) ma
