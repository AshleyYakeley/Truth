module Pinafore.Language.TypeSystem.Simplify.DuplicateGroundTypes
    ( mergeDuplicateGroundTypes
    ) where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.Type.Ground
import Pinafore.Language.TypeSystem.Type
import Shapes

mergeInSingularType ::
       Is PolarityType polarity => PinaforeSingularType polarity t -> PJMShimWit PinaforeSingularType polarity t
mergeInSingularType (GroundPinaforeSingularType gt args) =
    case mapDolanArguments
             mergeDuplicateGroundTypesInType
             (pinaforeGroundTypeVarianceType gt)
             (pinaforeGroundTypeVarianceMap gt)
             args of
        MkShimWit args' conv -> MkShimWit (GroundPinaforeSingularType gt args') conv
mergeInSingularType t = mkPJMShimWit t

mergeInTypes ::
       forall polarity ta tb. Is PolarityType polarity
    => PinaforeType polarity ta
    -> PinaforeType polarity tb
    -> PinaforeShimWit polarity (JoinMeetType polarity ta tb)
mergeInTypes ta tb =
    case polarityType @polarity of
        PositiveType ->
            chainShimWit mergeDuplicateGroundTypesInType $ joinMeetPinaforeShimWit (mkPJMShimWit ta) (mkPJMShimWit tb)
        NegativeType ->
            chainShimWit mergeDuplicateGroundTypesInType $ joinMeetPinaforeShimWit (mkPJMShimWit ta) (mkPJMShimWit tb)

mergeIn1SingularType ::
       forall polarity t1 tr. Is PolarityType polarity
    => PinaforeSingularType polarity t1
    -> PinaforeType polarity tr
    -> PinaforeShimWit polarity (JoinMeetType polarity t1 tr)
mergeIn1SingularType ts NilPinaforeType = mkPJMShimWit $ ConsPinaforeType ts NilPinaforeType
mergeIn1SingularType (GroundPinaforeSingularType gt1 args1) (ConsPinaforeType (GroundPinaforeSingularType gt2 args2) tr)
    | Just (Refl, HRefl) <- pinaforeGroundTypeTestEquality gt1 gt2 =
        case mergeDolanArguments
                 mergeInTypes
                 (pinaforeGroundTypeVarianceType gt1)
                 (pinaforeGroundTypeVarianceMap gt1)
                 args1
                 args2 of
            MkShimWit args' convargs ->
                ccontramap (polarBimap convargs id . polarSwapRight) $
                mergeIn1SingularType (GroundPinaforeSingularType gt1 args') tr
mergeIn1SingularType ts (ConsPinaforeType t1 tr) =
    case mergeIn1SingularType ts tr of
        MkShimWit tsr conv ->
            MkShimWit (ConsPinaforeType t1 tsr) $ polarF (polar2 . conv . polar1) (polarBimap id $ conv . polar2)

mergeDuplicateGroundTypesInType ::
       forall polarity t. Is PolarityType polarity
    => PinaforeType polarity t
    -> PinaforeShimWit polarity t
mergeDuplicateGroundTypesInType NilPinaforeType = mkPJMShimWit NilPinaforeType
mergeDuplicateGroundTypesInType (ConsPinaforeType t1 tr) =
    case mergeInSingularType t1 of
        MkShimWit t1' conv1 ->
            case mergeDuplicateGroundTypesInType tr of
                MkShimWit tr' convr -> ccontramap (polarBimap conv1 convr) $ mergeIn1SingularType t1' tr'

mergeDuplicateGroundTypes ::
       forall a. PShimWitMappable PinaforeShim PinaforeType a
    => a
    -> a
mergeDuplicateGroundTypes =
    mapPShimWits @_ @PinaforeType mergeDuplicateGroundTypesInType mergeDuplicateGroundTypesInType
