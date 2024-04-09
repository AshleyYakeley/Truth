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

getOptSingleGreatestDynamicSupertype :: QSingularType 'Negative t -> Interpreter (Maybe (QShimWit 'Negative (Maybe t)))
getOptSingleGreatestDynamicSupertype (GroundedDolanSingularType (MkDolanGroundedType gt args)) = do
    mdt <- getPolyGreatestDynamicSupertype (qgtGreatestDynamicSupertype gt) args
    return $ fmap shimWitToDolan mdt
getOptSingleGreatestDynamicSupertype (RecursiveDolanSingularType var t) =
    case unrollRecursiveType var t of
        MkShimWit t' iconv -> do
            t'' <- getGreatestDynamicSupertype t'
            return $ Just $ mapShimWit (pfmap $ polarPolyIsoForwards iconv) t''
getOptSingleGreatestDynamicSupertype _ = return Nothing

getSingleGreatestDynamicSupertype :: QSingularType 'Negative t -> Interpreter (QShimWit 'Negative (Maybe t))
getSingleGreatestDynamicSupertype t = do
    mt' <- getOptSingleGreatestDynamicSupertype t
    return $
        case mt' of
            Just t' -> t'
            Nothing -> mapPolarShimWit (MkPolarShim $ functionToShim "Just" Just) $ typeToDolan t

getGreatestDynamicSupertype :: QType 'Negative t -> Interpreter (QShimWit 'Negative (Maybe t))
getGreatestDynamicSupertype NilDolanType =
    return $ mapPolarShimWit (MkPolarShim $ functionToShim "Just" Just) $ mkShimWit NilDolanType
getGreatestDynamicSupertype (ConsDolanType t1 NilDolanType) = do
    st <- getSingleGreatestDynamicSupertype t1
    return $ mapShimWit (pfmap iPolarL1) st
getGreatestDynamicSupertype (ConsDolanType t1 tr) = do
    t1' <- getSingleGreatestDynamicSupertype t1
    tr' <- getGreatestDynamicSupertype tr
    return $ mapShimWit zip2 $ joinMeetShimWit t1' tr'

getOptGreatestDynamicSupertype :: QType 'Negative t -> Interpreter (Maybe (QShimWit 'Negative (Maybe t)))
getOptGreatestDynamicSupertype (ConsDolanType t1 NilDolanType) = do
    mt1' <- getOptSingleGreatestDynamicSupertype t1
    return $ do
        t1' <- mt1'
        return $ shimWitToDolan $ mapShimWit (pfmap iPolarL1) t1'
getOptGreatestDynamicSupertype t = fmap Just $ getGreatestDynamicSupertype t

getOptGreatestDynamicSupertypeSW :: QShimWit 'Negative t -> Interpreter (Maybe (QShimWit 'Negative (Maybe t)))
getOptGreatestDynamicSupertypeSW (MkShimWit t conv) = do
    t' <- getOptGreatestDynamicSupertype t
    return $ fmap (mapShimWit (pfmap conv)) t'
