module Data.Recursion.AppRec
    ( AppRec
    , liftAppRec
    , AppKnot
    , appKnotRec
    , appKnotResult
    , pureAppKnot
    , knotAppRec
    ) where

import Shapes.Import

newtype AppRec f a =
    MkAppRec (forall t. IOWitness t -> f (t -> a))

instance Functor f => Functor (AppRec f) where
    fmap ab (MkAppRec tfta) = MkAppRec $ \t -> fmap (fmap ab) $ tfta t

instance Applicative f => Applicative (AppRec f) where
    pure a = MkAppRec $ \_ -> pure $ pure a
    MkAppRec tftab <*> MkAppRec tfta = MkAppRec $ \t -> liftA2 (<*>) (tftab t) (tfta t)

liftAppRec :: Functor f => f a -> AppRec f a
liftAppRec fa = MkAppRec $ \_ -> fmap (\a _ -> a) fa

runAppRec :: Functor f => IOWitness a -> AppRec f a -> f a
runAppRec wit (MkAppRec tfta) = fmap fix $ tfta wit

-- | deliberately not an instance of Functor/Applicative
data AppKnot f a = MkAppKnot
    { appKnotRec :: AppRec f a
    , appKnotResult :: f a
    }

-- | probably shouldn't be exposed
liftAppKnot :: Functor f => f a -> AppKnot f a
liftAppKnot fa = MkAppKnot (liftAppRec fa) fa

pureAppKnot :: Applicative f => a -> AppKnot f a
pureAppKnot a = liftAppKnot $ pure a

knotAppRec ::
       forall f a. Applicative f
    => IOWitness a
    -> AppRec f a
    -> AppKnot f a
knotAppRec wit ar@(MkAppRec tfta) = let
    tfta' :: forall t. IOWitness t -> f (t -> a)
    tfta' wit' =
        case testEquality wit wit' of
            Just Refl -> pure id
            Nothing -> tfta wit'
    in MkAppKnot (MkAppRec tfta') (runAppRec wit ar)
