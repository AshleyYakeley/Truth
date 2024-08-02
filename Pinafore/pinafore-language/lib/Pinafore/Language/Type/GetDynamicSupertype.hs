module Pinafore.Language.Type.GetDynamicSupertype
    ( QExprShim
    , QExprShimWit
    , QExprGroundedShimWit
    , getGreatestDynamicSupertype
    , getGroundedShimWitGreatestDynamicSupertype
    , getOptGreatestDynamicSupertype
    , getOptGreatestDynamicSupertypeSW
    ) where

import Import
import Pinafore.Language.Interpreter ()
import Pinafore.Language.Type.DynamicSupertype
import Pinafore.Language.Type.Ground

type QExprShim = DolanExprShim QGroundType

type QExprShimWit polarity = PShimWit QExprShim QType polarity

type QExprGroundedShimWit polarity = PShimWit QExprShim QGroundedType polarity

pfmap :: (HasVariance f, VarianceOf f ~ 'Covariance) => QShim a b -> QShim (f a) (f b)
pfmap = cfmap

zip2 :: PolarShim QExprShim 'Negative (Maybe (MeetType a b)) (MeetType (Maybe a) (Maybe b))
zip2 = MkPolarShim $ pureComposeShim $ functionToShim "zip2" $ \(BothMeetType a b) -> liftA2 BothMeetType a b

getOptGroundedGreatestDynamicSupertype :: QGroundedType 'Negative t -> Maybe (QExprGroundedShimWit 'Negative (Maybe t))
getOptGroundedGreatestDynamicSupertype (MkDolanGroundedType gt args) =
    case qgtGreatestDynamicSupertype gt of
        NullPolyGreatestDynamicSupertype -> Nothing
        MkPolyGreatestDynamicSupertype ft -> Just $ ft args

getGroundedGreatestDynamicSupertype :: QGroundedType 'Negative t -> QExprGroundedShimWit 'Negative (Maybe t)
getGroundedGreatestDynamicSupertype gnt =
    fromMaybe (MkShimWit gnt $ MkPolarShim $ pureComposeShim $ functionToShim "Just" Just) $
    getOptGroundedGreatestDynamicSupertype gnt

getGroundedShimWitGreatestDynamicSupertype :: QGroundedShimWit 'Negative t -> QExprGroundedShimWit 'Negative (Maybe t)
getGroundedShimWitGreatestDynamicSupertype (MkShimWit gnt (MkPolarShim conv)) =
    mapShimWit (MkPolarShim $ pureComposeShim $ cfmap conv) $ getGroundedGreatestDynamicSupertype gnt

getOptSingleGreatestDynamicSupertype ::
       QSingularType 'Negative t -> Interpreter (Maybe (QExprShimWit 'Negative (Maybe t)))
getOptSingleGreatestDynamicSupertype (GroundedDolanSingularType gnt) =
    return $ do
        gnt' <- getOptGroundedGreatestDynamicSupertype gnt
        return $ shimWitToDolan gnt'
getOptSingleGreatestDynamicSupertype (RecursiveDolanSingularType var t) =
    case unrollRecursiveType var t of
        MkShimWit t' iconv -> do
            t'' <- getGreatestDynamicSupertype t'
            return $ Just $ mapShimWit (MkPolarShim $ pureComposeShim $ cfmap $ polarPolyIsoNegative iconv) t''
getOptSingleGreatestDynamicSupertype _ = return Nothing

getSingleGreatestDynamicSupertype :: QSingularType 'Negative t -> Interpreter (QExprShimWit 'Negative (Maybe t))
getSingleGreatestDynamicSupertype t = do
    mt' <- getOptSingleGreatestDynamicSupertype t
    return $
        case mt' of
            Just t' -> t'
            Nothing -> mapShimWit (MkPolarShim $ pureComposeShim $ functionToShim "Just" Just) $ typeToDolan t

getGreatestDynamicSupertype :: QType 'Negative t -> Interpreter (QExprShimWit 'Negative (Maybe t))
getGreatestDynamicSupertype NilDolanType =
    return $ mapPolarShimWit (MkPolarShim $ pureComposeShim $ functionToShim "Just" Just) $ mkShimWit NilDolanType
getGreatestDynamicSupertype (ConsDolanType t1 NilDolanType) = do
    st <- getSingleGreatestDynamicSupertype t1
    return $ mapShimWit (MkPolarShim $ pureComposeShim $ pfmap iMeetR1) st
getGreatestDynamicSupertype (ConsDolanType t1 tr) = do
    t1' <- getSingleGreatestDynamicSupertype t1
    tr' <- getGreatestDynamicSupertype tr
    return $ mapShimWit zip2 $ joinMeetShimWit t1' tr'

getOptGreatestDynamicSupertype :: QType 'Negative t -> Interpreter (Maybe (QExprShimWit 'Negative (Maybe t)))
getOptGreatestDynamicSupertype (ConsDolanType t1 NilDolanType) = do
    mt1' <- getOptSingleGreatestDynamicSupertype t1
    return $ do
        t1' <- mt1'
        return $ shimWitToDolan $ mapShimWit (MkPolarShim $ pureComposeShim $ pfmap iMeetR1) t1'
getOptGreatestDynamicSupertype t = fmap Just $ getGreatestDynamicSupertype t

getOptGreatestDynamicSupertypeSW :: QShimWit 'Negative t -> Interpreter (Maybe (QExprShimWit 'Negative (Maybe t)))
getOptGreatestDynamicSupertypeSW (MkShimWit t (MkPolarShim conv)) = do
    t' <- getOptGreatestDynamicSupertype t
    return $ fmap (mapShimWit (MkPolarShim $ pureComposeShim $ cfmap conv)) t'
