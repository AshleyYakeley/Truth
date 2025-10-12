module Pinafore.Language.Convert.FromQIsoShim where

import Language.Expression.Dolan

import Import
import Pinafore.Language.Type.Ground

class (CCRVariancesShim pshim, JoinMeetIsoShim (pshim Type)) => FromQIsoShim (pshim :: PolyShimKind) where
    fromQShims :: forall a b. QShim a b -> QShim b a -> pshim Type a b

instance FromQIsoShim QPolyIsoShim where
    fromQShims ab ba = MkPolyMapT $ MkIsomorphism ab ba

instance FromQIsoShim QPolyShim where
    fromQShims ab _ = ab

instance FromQIsoShim (PolyMapT CatDual QPolyShim) where
    fromQShims _ ba = MkPolyMapT $ MkCatDual ba

fromQShimsPolar ::
    forall (pshim :: PolyShimKind) polarity a b.
    (FromQIsoShim pshim, Is PolarityType polarity) =>
    QShim a b ->
    QShim b a ->
    PolarShim (pshim Type) polarity a b
fromQShimsPolar ab ba =
    case polarityType @polarity of
        PositiveType -> MkPolarShim $ fromQShims ab ba
        NegativeType -> MkPolarShim $ fromQShims ba ab

mapQIsoShimWit ::
    forall (pshim :: PolyShimKind) polarity a b.
    (FromQIsoShim pshim, Is PolarityType polarity) =>
    QShim a b ->
    QShim b a ->
    PShimWit (pshim Type) QType polarity b ->
    PShimWit (pshim Type) QType polarity a
mapQIsoShimWit ab ba = mapShimWit $ fromQShimsPolar ab ba
