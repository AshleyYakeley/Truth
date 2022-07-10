module Pinafore.Language.Type.GetDynamicSupertype
    ( getGreatestDynamicSupertype
    , getOptGreatestDynamicSupertype
    , getOptGreatestDynamicSupertypeSW
    ) where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.Shim
import Pinafore.Language.Type.DynamicSupertype
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Subtype ()
import Pinafore.Language.Type.Type
import Shapes

getOptSingleGreatestDynamicSupertype ::
       PinaforeSingularType 'Negative t -> Maybe (PinaforeSingularShimWit 'Negative (Maybe t))
getOptSingleGreatestDynamicSupertype (GroundedDolanSingularType gt args) =
    getPolyGreatestDynamicSupertype (pgtGreatestDynamicSupertype gt) args
getOptSingleGreatestDynamicSupertype _ = Nothing

getSingleGreatestDynamicSupertype :: PinaforeSingularType 'Negative t -> PinaforeSingularShimWit 'Negative (Maybe t)
getSingleGreatestDynamicSupertype t =
    case getOptSingleGreatestDynamicSupertype t of
        Just t' -> t'
        Nothing -> mapPolarShimWit (MkPolarMap $ functionToShim "Just" Just) $ mkShimWit t

pfmap ::
       (HasVariance f, VarianceOf f ~ 'Covariance)
    => PolarMap (PinaforePolyShim Type) 'Negative a b
    -> PolarMap (PinaforePolyShim Type) 'Negative (f a) (f b)
pfmap (MkPolarMap mp) = MkPolarMap $ cfmap mp

zip2 ::
       PinaforeType 'Negative b
    -> PolarMap (PinaforePolyShim Type) 'Negative (Maybe (MeetType a b)) (MeetType (Maybe a) (Maybe b))
zip2 NilDolanType = polar1 . pfmap iPolarL1
zip2 _ = MkPolarMap $ functionToShim "zip2" $ \(BothMeetType a b) -> liftA2 BothMeetType a b

getGreatestDynamicSupertype :: PinaforeType 'Negative t -> PinaforeShimWit 'Negative (Maybe t)
getGreatestDynamicSupertype NilDolanType =
    mapPolarShimWit (MkPolarMap $ functionToShim "Just" Just) $ mkShimWit NilDolanType
getGreatestDynamicSupertype (ConsDolanType t1 tr) =
    case getSingleGreatestDynamicSupertype t1 of
        MkShimWit dt1 conv1 ->
            case getGreatestDynamicSupertype tr of
                MkShimWit dtr convr -> MkShimWit (ConsDolanType dt1 dtr) $ iPolarPair conv1 convr . zip2 tr

getOptGreatestDynamicSupertype :: PinaforeType 'Negative t -> Maybe (PinaforeShimWit 'Negative (Maybe t))
getOptGreatestDynamicSupertype (ConsDolanType t1 NilDolanType) = do
    t1' <- getOptSingleGreatestDynamicSupertype t1
    return $ singleDolanShimWit $ mapShimWit (pfmap iPolarL1) t1'
getOptGreatestDynamicSupertype t = Just $ getGreatestDynamicSupertype t

getOptGreatestDynamicSupertypeSW :: PinaforeShimWit 'Negative t -> Maybe (PinaforeShimWit 'Negative (Maybe t))
getOptGreatestDynamicSupertypeSW (MkShimWit t conv) = do
    t' <- getOptGreatestDynamicSupertype t
    return $ mapShimWit (pfmap conv) t'
