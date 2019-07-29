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
    , knowMaybeLens
    , catKnowns
    , uiUnknownValue
    ) where

import Language.Expression.Dolan
import Shapes
import Truth.Core

newtype Know a =
    MkKnow (Maybe a)
    deriving (Eq, Functor, Foldable, Applicative, Alternative, Monad, MonadFix, MonadPlus, MonadOne)

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

instance RepresentationalRole Know where
    representationalCoercion MkCoercion = MkCoercion

instance HasDolanVary '[ 'Covariance] Know where
    dolanVary = ConsDolanVarianceMap (Just Dict) cfmap $ NilDolanVarianceMap

fromKnow :: a -> Know a -> a
fromKnow _ (Known v) = v
fromKnow v Unknown = v

isKnown :: Know a -> Bool
isKnown (Known _) = True
isKnown Unknown = False

knowBool :: Bijection (Know ()) Bool
knowBool =
    MkIsomorphism isKnown $ \b ->
        if b
            then Known ()
            else Unknown

maybeToKnow :: Maybe a -> Know a
maybeToKnow = MkKnow

knowToMaybe :: Know a -> Maybe a
knowToMaybe (MkKnow ma) = ma

knowMaybe :: Bijection (Know a) (Maybe a)
knowMaybe = MkIsomorphism knowToMaybe maybeToKnow

knowMaybeLens :: Lens' Maybe a (Know a)
knowMaybeLens = MkLens Known $ \ka _ -> knowToMaybe ka

catKnowns :: Filterable f => f (Know a) -> f a
catKnowns = catMaybes . fmap knowToMaybe

uiUnknownValue :: Eq a => a -> UISpec sel (WholeEdit a) -> UISpec sel (WholeEdit (Know a))
uiUnknownValue def ui = mapEditUISpec (bijectionWholeEditLens knowMaybe) $ mapMaybeNothingUISpec def ui
