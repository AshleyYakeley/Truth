module Data.PurityFunction where

import Shapes.Import

data PurityFunction m a b
    = ImpureFunction (a -> m b)
    | PureFunction (a -> b)

applyPurityFunction :: Applicative m => PurityFunction m a b -> a -> m b
applyPurityFunction (ImpureFunction amb) a = amb a
applyPurityFunction (PureFunction ab) a = pure $ ab a

instance Functor m => Functor (PurityFunction m t) where
    fmap ab (ImpureFunction mf) = ImpureFunction $ fmap (fmap ab) mf
    fmap ab (PureFunction f) = PureFunction $ fmap ab f

instance Applicative m => Applicative (PurityFunction m t) where
    pure a = PureFunction $ pure a
    PureFunction tab <*> PureFunction ta = PureFunction $ tab <*> ta
    tab <*> ta = ImpureFunction $ \t -> applyPurityFunction tab t <*> applyPurityFunction ta t

instance Alternative (PurityFunction Maybe t) where
    empty = ImpureFunction $ \_ -> empty
    PureFunction ab <|> _ = PureFunction ab
    ImpureFunction amb1 <|> PureFunction ab2 =
        PureFunction $ \a ->
            case amb1 a of
                Just b -> b
                Nothing -> ab2 a
    ImpureFunction amb1 <|> ImpureFunction amb2 = ImpureFunction $ \a -> amb1 a <|> amb2 a

instance Monad m => Category (PurityFunction m) where
    id = PureFunction id
    PureFunction bc . PureFunction ab = PureFunction $ bc . ab
    bc . ab = ImpureFunction $ \a -> applyPurityFunction ab a >>= applyPurityFunction bc

instance Monad m => Arrow (PurityFunction m) where
    arr = PureFunction
    first (PureFunction ab) = PureFunction $ first ab
    first (ImpureFunction amb) = ImpureFunction $ \(a, c) -> fmap (\b -> (b, c)) $ amb a
    second (PureFunction ab) = PureFunction $ second ab
    second (ImpureFunction amb) = ImpureFunction $ \(c, a) -> fmap (\b -> (c, b)) $ amb a
