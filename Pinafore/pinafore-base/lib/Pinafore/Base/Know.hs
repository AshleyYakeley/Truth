module Pinafore.Base.Know
    ( Know
    , pattern Known
    , pattern Unknown
    , fromKnow
    , isKnown
    , knowBool
    , knowCompare
    , maybeToKnow
    , knowToMaybe
    , knowMaybe
    , knowMaybeLens
    , catKnowns
    , unknownValueBijection
    , unknownValueChangeLens
    , biFromKnowWhole
    , biToKnowWhole
    )
where

import Changes.Core
import Shapes

newtype Know a
    = MkKnow (Maybe a)
    deriving newtype
        ( Eq
        , Functor
        , Foldable
        , Applicative
        , Alternative
        , Monad
        , MonadFix
        , MonadPlus
        , MonadException
        , MonadInner
        )

pattern Known :: a -> Know a
pattern Known a = MkKnow (Just a)

pattern Unknown :: Know a
pattern Unknown = MkKnow Nothing

{-# COMPLETE Known, Unknown #-}

knowCompare :: (a -> a -> Ordering) -> Know a -> Know a -> Ordering
knowCompare _ Unknown Unknown = EQ
knowCompare _ Unknown (Known _) = LT
knowCompare _ (Known _) Unknown = GT
knowCompare cmp (Known a) (Known b) = cmp a b

instance Ord a => Ord (Know a) where
    compare = knowCompare compare

instance Traversable Know where
    traverse afb (Known a) = fmap Known $ afb a
    traverse _ Unknown = pure Unknown

instance Show a => Show (Know a) where
    show Unknown = "Unknown"
    show (Known a) = "Known " <> show a

instance RepresentationalRole Know where
    representationalCoercion MkCoercion = MkCoercion

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

instance Invariant Know where
    invmap ab _ = fmap ab

instance InjectiveFilterable Know

instance Filterable Know where
    mapMaybe amb = \case
        Known a | Just b <- amb a -> Known b
        _ -> Unknown

-- | not really a bijection
unknownValueBijection :: a -> Bijection (Know a) a
unknownValueBijection def = let
    isoForwards (Known a) = a
    isoForwards Unknown = def
    isoBackwards = Known
    in MkIsomorphism{..}

unknownValueChangeLens :: a -> ChangeLens (WholeUpdate (Know a)) (WholeUpdate a)
unknownValueChangeLens def = toChangeLens $ unknownValueBijection def

biFromKnowWhole ::
    forall p q.
    Monoid q =>
    ChangeLens (BiWholeUpdate (Know p) (Know q)) (BiWholeUpdate p q)
biFromKnowWhole = mapBiWholeChangeLens Known $ fromKnow mempty

biToKnowWhole ::
    forall p q.
    Monoid p =>
    ChangeLens (BiWholeUpdate p q) (BiWholeUpdate (Know p) (Know q))
biToKnowWhole = mapBiWholeChangeLens (fromKnow mempty) Known
