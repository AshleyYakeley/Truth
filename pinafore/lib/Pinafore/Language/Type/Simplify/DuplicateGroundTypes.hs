module Pinafore.Language.Type.Simplify.DuplicateGroundTypes
    ( mergeDuplicateGroundTypes
    ) where

import Data.Shim.Polarity
import Data.Shim.ShimWit
import Language.Expression.Dolan
import Pinafore.Language.GroundType
import Pinafore.Language.Type.Type
import Shapes

mergeInSingularType ::
       Is PolarityType polarity
    => PinaforeSingularType baseedit polarity t
    -> PJMShimWit (PinaforeSingularType baseedit) polarity t
mergeInSingularType (GroundPinaforeSingularType gt args) =
    case mapDolanArguments
             mergeDuplicateGroundTypesInType
             (pinaforeGroundTypeVarianceType gt)
             (pinaforeGroundTypeVarianceMap gt)
             args of
        MkShimWit args' conv -> MkShimWit (GroundPinaforeSingularType gt args') conv
mergeInSingularType t = mkPJMShimWit t

mergeInTypes ::
       forall baseedit polarity ta tb. Is PolarityType polarity
    => PinaforeType baseedit polarity ta
    -> PinaforeType baseedit polarity tb
    -> PinaforeShimWit baseedit polarity (JoinMeetType polarity ta tb)
mergeInTypes ta tb =
    case representative @_ @_ @polarity of
        PositiveType ->
            chainShimWit mergeDuplicateGroundTypesInType $ joinPinaforeShimWit (mkPJMShimWit ta) (mkPJMShimWit tb)
        NegativeType ->
            chainShimWit mergeDuplicateGroundTypesInType $ meetPinaforeShimWit (mkPJMShimWit ta) (mkPJMShimWit tb)

mergeInPositiveSingularType ::
       PinaforeSingularType baseedit 'Positive t1
    -> PinaforeType baseedit 'Positive tr
    -> PinaforeShimWit baseedit 'Positive (JoinType t1 tr)
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
       PinaforeSingularType baseedit 'Negative t1
    -> PinaforeType baseedit 'Negative tr
    -> PinaforeShimWit baseedit 'Negative (MeetType t1 tr)
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
       forall baseedit polarity t. Is PolarityType polarity
    => PinaforeType baseedit polarity t
    -> PinaforeShimWit baseedit polarity t
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
       forall baseedit a. PShimWitMappable PinaforeShim (PinaforeType baseedit) a
    => a
    -> a
mergeDuplicateGroundTypes =
    mapPShimWits @_ @(PinaforeType baseedit) mergeDuplicateGroundTypesInType mergeDuplicateGroundTypesInType
