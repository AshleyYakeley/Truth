module Language.Expression.Dolan.Simplify.DuplicateGroundTypes
    ( mergeDuplicateGroundTypes
    ) where

import Data.Shim
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Solver
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

mergeInSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> DolanTypeCheckM ground (DolanSingularShimWit ground polarity t)
mergeInSingularType = mapDolanSingularTypeM mergeInType

mergeTypeType ::
       forall (ground :: GroundTypeKind) polarity ta tb. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity ta
    -> DolanType ground polarity tb
    -> DolanTypeCheckM ground (DolanShimWit ground polarity (JoinMeetType polarity ta tb))
mergeTypeType ta tb =
    case polarityType @polarity of
        PositiveType -> chainPolarShimWitM mergeInType $ joinMeetShimWit (mkPolarShimWit ta) (mkPolarShimWit tb)
        NegativeType -> chainPolarShimWitM mergeInType $ joinMeetShimWit (mkPolarShimWit ta) (mkPolarShimWit tb)

mergeSSSubtype ::
       forall (ground :: GroundTypeKind) polarity ta tb. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity ta
    -> DolanSingularType ground polarity tb
    -> DolanTypeCheckM ground (DolanSingularShimWit ground polarity (JoinMeetType polarity ta tb))
mergeSSSubtype ta tb = do
    conv <- subtypeSingularType ta tb
    return $ MkShimWit tb $ polarF conv id

mergeSSEquality ::
       forall (ground :: GroundTypeKind) polarity ta tb. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity ta
    -> DolanSingularType ground polarity tb
    -> DolanTypeCheckM ground (DolanSingularShimWit ground polarity (JoinMeetType polarity ta tb))
mergeSSEquality (GroundedDolanSingularType (MkDolanGroundedType gt1 args1)) (GroundedDolanSingularType (MkDolanGroundedType gt2 args2))
    | Just (Refl, HRefl) <- groundTypeTestEquality gt1 gt2 = do
        MkShimWit args' convargs <- mergeDolanArgumentsM mergeTypeType (groundTypeVarianceMap gt1) args1 args2
        return $ MkShimWit (GroundedDolanSingularType (MkDolanGroundedType gt1 args')) convargs
mergeSSEquality _ _ = empty

mergeSingularSingularType ::
       forall (ground :: GroundTypeKind) polarity ta tb. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity ta
    -> DolanSingularType ground polarity tb
    -> DolanTypeCheckM ground (DolanSingularShimWit ground polarity (JoinMeetType polarity ta tb))
mergeSingularSingularType ta tb =
    mergeSSSubtype ta tb <|> fmap (mapPolarShimWit iPolarSwap) (mergeSSSubtype tb ta) <|> mergeSSEquality ta tb

mergeSingularType ::
       forall (ground :: GroundTypeKind) polarity t1 tr. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t1
    -> DolanType ground polarity tr
    -> DolanTypeCheckM ground (DolanShimWit ground polarity (JoinMeetType polarity t1 tr))
mergeSingularType ts NilDolanType = return $ mkPolarShimWit $ ConsDolanType ts NilDolanType
mergeSingularType ts (ConsDolanType t1 tr) = do
    mts1 <- mcatch $ mergeSingularSingularType ts t1
    case mts1 of
        Nothing -> do
            MkShimWit tsr conv <- mergeSingularType ts tr
            return $ MkShimWit (ConsDolanType t1 tsr) $ polarF (polar2 . conv . polar1) (iPolarPair id $ conv . polar2)
        Just (MkShimWit ts1 conv1) -> do
            t' <- mergeSingularType ts1 tr
            return $ mapPolarShimWit (iPolarPair conv1 id . iPolarSwapR) t'

mergeInType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanTypeCheckM ground (DolanShimWit ground polarity t)
mergeInType NilDolanType = return nilDolanShimWit
mergeInType (ConsDolanType t1 tr) = do
    MkShimWit t1' conv1 <- mergeInSingularType t1
    MkShimWit tr' convr <- mergeInType tr
    t'' <- mergeSingularType t1' tr'
    return $ mapShimWit (iPolarPair conv1 convr) t''

mergeDuplicateGroundTypes ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanSubtypeGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a)
    => EndoM (DolanTypeCheckM ground) a
mergeDuplicateGroundTypes = mapPShimWitsM mergeInType mergeInType
