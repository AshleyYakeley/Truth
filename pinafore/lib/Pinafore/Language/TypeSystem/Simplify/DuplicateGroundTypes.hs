module Pinafore.Language.TypeSystem.Simplify.DuplicateGroundTypes
    ( mergeDuplicateGroundTypes
    ) where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.Type.Ground
import Pinafore.Language.TypeSystem.Type
import Shapes

mergeInSingularType ::
       Is PolarityType polarity
    => PinaforeSingularType baseupdate polarity t
    -> PJMShimWit (PinaforeSingularType baseupdate) polarity t
mergeInSingularType (GroundPinaforeSingularType gt args) =
    case mapDolanArguments
             mergeDuplicateGroundTypesInType
             (pinaforeGroundTypeVarianceType gt)
             (pinaforeGroundTypeVarianceMap gt)
             args of
        MkShimWit args' conv -> MkShimWit (GroundPinaforeSingularType gt args') conv
mergeInSingularType t = mkPJMShimWit t

mergeInTypes ::
       forall baseupdate polarity ta tb. Is PolarityType polarity
    => PinaforeType baseupdate polarity ta
    -> PinaforeType baseupdate polarity tb
    -> PinaforeShimWit baseupdate polarity (JoinMeetType polarity ta tb)
mergeInTypes ta tb =
    case representative @_ @_ @polarity of
        PositiveType ->
            chainShimWit mergeDuplicateGroundTypesInType $ joinPinaforeShimWit (mkPJMShimWit ta) (mkPJMShimWit tb)
        NegativeType ->
            chainShimWit mergeDuplicateGroundTypesInType $ meetPinaforeShimWit (mkPJMShimWit ta) (mkPJMShimWit tb)

mergeInPositiveSingularType ::
       PinaforeSingularType baseupdate 'Positive t1
    -> PinaforeType baseupdate 'Positive tr
    -> PinaforeShimWit baseupdate 'Positive (JoinType t1 tr)
mergeInPositiveSingularType ts NilPinaforeType = mkPJMShimWit $ ConsPinaforeType ts NilPinaforeType
mergeInPositiveSingularType (GroundPinaforeSingularType gt1 args1) (ConsPinaforeType (GroundPinaforeSingularType gt2 args2) tr)
    | Just (Refl, HRefl) <- pinaforeGroundTypeTestEquality gt1 gt2 =
        case mergeDolanArguments
                 mergeInTypes
                 (pinaforeGroundTypeVarianceType gt1)
                 (pinaforeGroundTypeVarianceMap gt1)
                 args1
                 args2 of
            MkShimWit args' convargs ->
                ccontramap (joinBimap convargs id . swapJoinRight) $
                mergeInPositiveSingularType (GroundPinaforeSingularType gt1 args') tr
mergeInPositiveSingularType ts (ConsPinaforeType t1 tr) =
    case mergeInPositiveSingularType ts tr of
        MkShimWit tsr conv ->
            MkShimWit (ConsPinaforeType t1 tsr) $ joinf (join2 . conv . join1) (joinBimap id $ conv . join2)

mergeInNegativeSingularType ::
       PinaforeSingularType baseupdate 'Negative t1
    -> PinaforeType baseupdate 'Negative tr
    -> PinaforeShimWit baseupdate 'Negative (MeetType t1 tr)
mergeInNegativeSingularType ts NilPinaforeType = mkPJMShimWit $ ConsPinaforeType ts NilPinaforeType
mergeInNegativeSingularType (GroundPinaforeSingularType gt1 args1) (ConsPinaforeType (GroundPinaforeSingularType gt2 args2) tr)
    | Just (Refl, HRefl) <- pinaforeGroundTypeTestEquality gt1 gt2 =
        case mergeDolanArguments
                 mergeInTypes
                 (pinaforeGroundTypeVarianceType gt1)
                 (pinaforeGroundTypeVarianceMap gt1)
                 args1
                 args2 of
            MkShimWit args' convargs ->
                cfmap (swapMeetRight . meetBimap convargs id) $
                mergeInNegativeSingularType (GroundPinaforeSingularType gt1 args') tr
mergeInNegativeSingularType ts (ConsPinaforeType t1 tr) =
    case mergeInNegativeSingularType ts tr of
        MkShimWit tsr conv ->
            MkShimWit (ConsPinaforeType t1 tsr) $ meetf (meet1 . conv . meet2) (meetBimap id $ meet2 . conv)

mergeDuplicateGroundTypesInType ::
       forall baseupdate polarity t. Is PolarityType polarity
    => PinaforeType baseupdate polarity t
    -> PinaforeShimWit baseupdate polarity t
mergeDuplicateGroundTypesInType NilPinaforeType = mkPJMShimWit NilPinaforeType
mergeDuplicateGroundTypesInType (ConsPinaforeType t1 tr) =
    case mergeInSingularType t1 of
        MkShimWit t1' conv1 ->
            case mergeDuplicateGroundTypesInType tr of
                MkShimWit tr' convr ->
                    case representative @_ @_ @polarity of
                        PositiveType -> ccontramap (joinBimap conv1 convr) $ mergeInPositiveSingularType t1' tr'
                        NegativeType -> cfmap (meetBimap conv1 convr) $ mergeInNegativeSingularType t1' tr'

mergeDuplicateGroundTypes ::
       forall baseupdate a. PShimWitMappable PinaforeShim (PinaforeType baseupdate) a
    => a
    -> a
mergeDuplicateGroundTypes =
    mapPShimWits @_ @(PinaforeType baseupdate) mergeDuplicateGroundTypesInType mergeDuplicateGroundTypesInType
