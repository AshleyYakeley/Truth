{-# OPTIONS -fno-warn-orphans #-}
module Pinafore.Language.Convert.JoinMeet where

import Language.Expression.Dolan

import Import
import Pinafore.Language.Convert.HasType

-- top, bottom, join, meet
instance forall (pshim :: PolyShimKind). CCRVariancesShim pshim => HasQType pshim 'Positive BottomType where
    qType = nilDolanShimWit

instance forall (pshim :: PolyShimKind). CCRVariancesShim pshim => HasQType pshim 'Negative TopType where
    qType = nilDolanShimWit

instance
    forall (pshim :: PolyShimKind) a b.
    ( CCRVariancesShim pshim
    , JoinMeetIsoShim (pshim Type)
    , HasQType pshim 'Positive a
    , HasQType pshim 'Positive b
    ) =>
    HasQType pshim 'Positive (JoinType a b)
    where
    qType = joinMeetShimWit qType qType

instance
    forall (pshim :: PolyShimKind) a b.
    ( CCRVariancesShim pshim
    , JoinMeetIsoShim (pshim Type)
    , HasQType pshim 'Negative a
    , HasQType pshim 'Negative b
    ) =>
    HasQType pshim 'Negative (MeetType a b)
    where
    qType = joinMeetShimWit qType qType
