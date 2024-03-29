module Pinafore.Language.Type.GetDynamicSupertype
    ( getGreatestDynamicSupertype
    , getOptGreatestDynamicSupertype
    , getOptGreatestDynamicSupertypeSW
    ) where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.Interpreter ()
import Pinafore.Language.Shim
import Pinafore.Language.Type.DynamicSupertype
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Subtype ()
import Shapes

pfmap ::
       (HasVariance f, VarianceOf f ~ 'Covariance)
    => PolarShim (QPolyShim Type) 'Negative a b
    -> PolarShim (QPolyShim Type) 'Negative (f a) (f b)
pfmap (MkPolarShim mp) = MkPolarShim $ cfmap mp

zip2 :: PolarShim (QPolyShim Type) 'Negative (Maybe (MeetType a b)) (MeetType (Maybe a) (Maybe b))
zip2 = MkPolarShim $ functionToShim "zip2" $ \(BothMeetType a b) -> liftA2 BothMeetType a b

getOptSingleGreatestDynamicSupertype :: QSingularType 'Negative t -> Maybe (QShimWit 'Negative (Maybe t))
getOptSingleGreatestDynamicSupertype (GroundedDolanSingularType (MkDolanGroundedType gt args)) = do
    dt <- getPolyGreatestDynamicSupertype (qgtGreatestDynamicSupertype gt) args
    return $ shimWitToDolan dt
getOptSingleGreatestDynamicSupertype (RecursiveDolanSingularType var t) =
    case unrollRecursiveType var t of
        MkShimWit t' iconv -> let
            t'' = getGreatestDynamicSupertype t'
            in return $ mapShimWit (pfmap $ polarPolyIsoForwards iconv) t''
getOptSingleGreatestDynamicSupertype _ = Nothing

getSingleGreatestDynamicSupertype :: QSingularType 'Negative t -> QShimWit 'Negative (Maybe t)
getSingleGreatestDynamicSupertype t =
    case getOptSingleGreatestDynamicSupertype t of
        Just t' -> t'
        Nothing -> mapPolarShimWit (MkPolarShim $ functionToShim "Just" Just) $ typeToDolan t

getGreatestDynamicSupertype :: QType 'Negative t -> QShimWit 'Negative (Maybe t)
getGreatestDynamicSupertype NilDolanType =
    mapPolarShimWit (MkPolarShim $ functionToShim "Just" Just) $ mkShimWit NilDolanType
getGreatestDynamicSupertype (ConsDolanType t1 NilDolanType) =
    mapShimWit (pfmap iPolarL1) $ getSingleGreatestDynamicSupertype t1
getGreatestDynamicSupertype (ConsDolanType t1 tr) =
    mapShimWit zip2 $ joinMeetShimWit (getSingleGreatestDynamicSupertype t1) (getGreatestDynamicSupertype tr)

getOptGreatestDynamicSupertype :: QType 'Negative t -> Maybe (QShimWit 'Negative (Maybe t))
getOptGreatestDynamicSupertype (ConsDolanType t1 NilDolanType) = do
    t1' <- getOptSingleGreatestDynamicSupertype t1
    return $ shimWitToDolan $ mapShimWit (pfmap iPolarL1) t1'
getOptGreatestDynamicSupertype t = Just $ getGreatestDynamicSupertype t

getOptGreatestDynamicSupertypeSW :: QShimWit 'Negative t -> Maybe (QShimWit 'Negative (Maybe t))
getOptGreatestDynamicSupertypeSW (MkShimWit t conv) = do
    t' <- getOptGreatestDynamicSupertype t
    return $ mapShimWit (pfmap conv) t'
