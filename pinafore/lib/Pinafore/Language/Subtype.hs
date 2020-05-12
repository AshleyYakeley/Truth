module Pinafore.Language.Subtype
    ( SubtypeMatch(..)
    , SubtypeEntry(..)
    , getSubtypeShim
    , equalSubtypeMatch
    ) where

import Data.Shim
import Shapes

newtype SubtypeMatch cat w = MkSubtypeMatch
    { unSubtypeMatch :: forall a b. (InKind a, InKind b) => w a -> w b -> Maybe (cat a b)
    }

data SubtypeEntry cat w =
    forall a b. (InKind a, InKind b) =>
                    MkSubtypeEntry (w a)
                                   (w b)
                                   (cat a b)

getImmediateSupertypes ::
       forall cat w t. (InCategory cat, InKind t)
    => SubtypeMatch cat w
    -> [SubtypeEntry cat w]
    -> ShimWit cat w 'Positive t
    -> [ShimWit cat w 'Positive t]
getImmediateSupertypes statmap st (MkShimWit ta0 conv0) = do
    MkSubtypeEntry ta tb conv <- st
    case unSubtypeMatch statmap ta0 ta of
        Just sconv -> return $ MkShimWit tb $ conv <.> sconv <.> conv0
        Nothing -> empty

eqShimWit :: forall cat w t. SubtypeMatch cat w -> ShimWit cat w 'Positive t -> ShimWit cat w 'Positive t -> Bool
eqShimWit statmap (MkShimWit ta _) (MkShimWit tb _) =
    isJust $ do
        _ <- unSubtypeMatch statmap ta tb
        _ <- unSubtypeMatch statmap tb ta
        return ()

expandSupertypes ::
       forall cat w t. (InCategory cat, InKind t)
    => SubtypeMatch cat w
    -> [SubtypeEntry cat w]
    -> [ShimWit cat w 'Positive t]
    -> [ShimWit cat w 'Positive t]
expandSupertypes statmap st aa = nubBy (eqShimWit statmap) $ mconcat $ aa : fmap (getImmediateSupertypes statmap st) aa

contains ::
       forall cat w a b. (InCategory cat, InKind a, InKind b)
    => SubtypeMatch cat w
    -> [ShimWit cat w 'Positive a]
    -> w b
    -> Maybe (cat a b)
contains _ [] _ = Nothing
contains statmap (MkShimWit ta conv:_) tb
    | Just sconv <- unSubtypeMatch statmap ta tb = Just $ sconv <.> conv
contains statmap (_:aa) tb = contains statmap aa tb

isSupertype ::
       forall cat w a b. (InCategory cat, InKind a, InKind b)
    => SubtypeMatch cat w
    -> [SubtypeEntry cat w]
    -> [ShimWit cat w 'Positive a]
    -> w b
    -> Maybe (cat a b)
isSupertype statmap _st aa a
    | Just conv <- contains statmap aa a = Just conv
isSupertype statmap subtypes aa a = let
    aa' = expandSupertypes statmap subtypes aa
    in if length aa' > length aa
           then isSupertype statmap subtypes aa' a
           else Nothing

getSubtypeShim ::
       forall cat w. InCategory cat
    => [SubtypeEntry cat w]
    -> SubtypeMatch cat w
    -> SubtypeMatch cat w
getSubtypeShim subtypes statmap = MkSubtypeMatch $ \ta tb -> isSupertype statmap subtypes [mkShimWit ta] tb

equalSubtypeMatch :: (InCategory cat, TestEquality w) => SubtypeMatch cat w
equalSubtypeMatch =
    MkSubtypeMatch $ \wa wb -> do
        Refl <- testEquality wa wb
        return cid
