module Pinafore.Language.Subtype
    ( SubtypeMatch(..)
    , SubtypeEntry(..)
    , getSubtypeShim
    , equalSubtypeMatch
    ) where

import Data.Shim
import Shapes

newtype SubtypeMatch shim w = MkSubtypeMatch
    { unSubtypeMatch :: forall a b. (InKind a, InKind b) => w a -> w b -> Maybe (shim a b)
    }

data SubtypeEntry shim w =
    forall a b. (InKind a, InKind b) =>
                    MkSubtypeEntry (w a)
                                   (w b)
                                   (shim a b)

getImmediateSupertypes ::
       forall shim w t. (InCategory shim, InKind t)
    => SubtypeMatch shim w
    -> [SubtypeEntry shim w]
    -> ShimWit shim w 'Positive t
    -> [ShimWit shim w 'Positive t]
getImmediateSupertypes statmap st t0 =
    unPosShimWit t0 $ \ta0 conv0 -> do
        MkSubtypeEntry ta tb conv <- st
        case unSubtypeMatch statmap ta0 ta of
            Just sconv -> return $ mkPosShimWit tb $ conv <.> sconv <.> conv0
            Nothing -> empty

eqShimWit :: forall shim w t. SubtypeMatch shim w -> ShimWit shim w 'Positive t -> ShimWit shim w 'Positive t -> Bool
eqShimWit statmap (MkShimWit ta _) (MkShimWit tb _) =
    isJust $ do
        _ <- unSubtypeMatch statmap ta tb
        _ <- unSubtypeMatch statmap tb ta
        return ()

expandSupertypes ::
       forall shim w t. (InCategory shim, InKind t)
    => SubtypeMatch shim w
    -> [SubtypeEntry shim w]
    -> [ShimWit shim w 'Positive t]
    -> [ShimWit shim w 'Positive t]
expandSupertypes statmap st aa = nubBy (eqShimWit statmap) $ mconcat $ aa : fmap (getImmediateSupertypes statmap st) aa

contains ::
       forall shim w a b. (InCategory shim, InKind a, InKind b)
    => SubtypeMatch shim w
    -> [ShimWit shim w 'Positive a]
    -> w b
    -> Maybe (shim a b)
contains _ [] _ = Nothing
contains statmap (MkShimWit ta (MkPolarMap conv):_) tb
    | Just sconv <- unSubtypeMatch statmap ta tb = Just $ sconv <.> conv
contains statmap (_:aa) tb = contains statmap aa tb

isSupertype ::
       forall shim w a b. (InCategory shim, InKind a, InKind b)
    => SubtypeMatch shim w
    -> [SubtypeEntry shim w]
    -> [ShimWit shim w 'Positive a]
    -> w b
    -> Maybe (shim a b)
isSupertype statmap _st aa a
    | Just conv <- contains statmap aa a = Just conv
isSupertype statmap subtypes aa a = let
    aa' = expandSupertypes statmap subtypes aa
    in if length aa' > length aa
           then isSupertype statmap subtypes aa' a
           else Nothing

getSubtypeShim ::
       forall shim w. InCategory shim
    => [SubtypeEntry shim w]
    -> SubtypeMatch shim w
    -> SubtypeMatch shim w
getSubtypeShim subtypes statmap = MkSubtypeMatch $ \ta tb -> isSupertype statmap subtypes [mkShimWit ta] tb

equalSubtypeMatch :: (InCategory shim, TestEquality w) => SubtypeMatch shim w
equalSubtypeMatch =
    MkSubtypeMatch $ \wa wb -> do
        Refl <- testEquality wa wb
        return cid
