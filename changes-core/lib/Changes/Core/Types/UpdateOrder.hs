module Changes.Core.Types.UpdateOrder where

import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Types.EditApplicative
import Changes.Core.Types.ReadOnly
import Changes.Core.Types.Whole

data UpdateOrder update =
    forall o. MkUpdateOrder (o -> o -> Ordering)
                            (FloatingChangeLens update (ROWUpdate o))

instance Semigroup (UpdateOrder update) where
    MkUpdateOrder oa lensa <> MkUpdateOrder ob lensb = let
        oab (a1, b1) (a2, b2) =
            case oa a1 a2 of
                EQ -> ob b1 b2
                cmp -> cmp
        in MkUpdateOrder oab $ eaPairReadOnlyWhole lensa lensb

instance Monoid (UpdateOrder update) where
    mempty = MkUpdateOrder (\_ _ -> EQ) $ eaPure ()
    mappend = (<>)

mapUpdateOrder :: FloatingChangeLens updateB updateA -> UpdateOrder updateA -> UpdateOrder updateB
mapUpdateOrder lens (MkUpdateOrder cmp flens) = MkUpdateOrder cmp $ flens . lens

mapReadOnlyUpdateOrder ::
       FloatingChangeLens updateB (ReadOnlyUpdate updateA) -> UpdateOrder updateA -> UpdateOrder updateB
mapReadOnlyUpdateOrder lens (MkUpdateOrder cmp flens) = MkUpdateOrder cmp $ liftReadOnlyFloatingChangeLens flens . lens

reverseUpdateOrder :: UpdateOrder update -> UpdateOrder update
reverseUpdateOrder (MkUpdateOrder cmp lens) = MkUpdateOrder (\a b -> cmp b a) lens

pureUpdateOrder :: (a -> a -> Ordering) -> UpdateOrder (ROWUpdate a)
pureUpdateOrder cmp = MkUpdateOrder cmp id

ordUpdateOrder :: Ord a => UpdateOrder (ROWUpdate a)
ordUpdateOrder = pureUpdateOrder compare
