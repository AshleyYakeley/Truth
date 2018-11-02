module Pinafore.Base.Know
    ( Know
    , pattern Known
    , pattern Unknown
    , fromKnow
    , isKnown
    , knowBool
    , maybeToKnow
    , knowToMaybe
    , knowMaybe
    , catKnowns
    , uiUnknownValue
    ) where

import Language.Expression.Dolan
import Shapes
import Truth.Core

newtype Know a =
    MkKnow (Maybe a)
    deriving (Eq, Functor, Foldable, Applicative, Alternative, Monad, MonadPlus)

pattern Known :: a -> Know a

pattern Known a = MkKnow (Just a)

pattern Unknown :: Know a

pattern Unknown = MkKnow Nothing

{-# COMPLETE Known, Unknown #-}

instance Ord a => Ord (Know a) where
    compare Unknown Unknown = EQ
    compare Unknown (Known _) = LT
    compare (Known _) Unknown = GT
    compare (Known a) (Known b) = compare a b

instance Traversable Know where
    traverse afb (Known a) = fmap Known $ afb a
    traverse _ Unknown = pure Unknown

instance Show a => Show (Know a) where
    show Unknown = "Unknown"
    show (Known a) = "Known " <> show a

instance HasDolanVary '[ 'Covariance] Know where
    dolanVary = ConsDolanKindVary fmap $ NilDolanKindVary

fromKnow :: a -> Know a -> a
fromKnow _ (Known v) = v
fromKnow v Unknown = v

isKnown :: Know a -> Bool
isKnown (Known _) = True
isKnown Unknown = False

knowBool :: Bijection (Know ()) Bool
knowBool =
    MkBijection isKnown $ \b ->
        if b
            then Known ()
            else Unknown

maybeToKnow :: Maybe a -> Know a
maybeToKnow = MkKnow

knowToMaybe :: Know a -> Maybe a
knowToMaybe (MkKnow ma) = ma

knowMaybe :: Bijection (Know a) (Maybe a)
knowMaybe = MkBijection knowToMaybe maybeToKnow

catKnowns :: Filterable f => f (Know a) -> f a
catKnowns = catMaybes . fmap knowToMaybe

uiUnknownValue :: Eq a => a -> UISpec seledit (WholeEdit a) -> UISpec seledit (WholeEdit (Know a))
uiUnknownValue def ui = uiLens (bijectionWholeEditLens knowMaybe) $ uiNothingValue def ui
