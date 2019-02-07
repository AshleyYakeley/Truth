module Pinafore.Language.Type.Simplify.DuplicateGroundTypes
    ( mergeDuplicateGroundTypes
    ) where

import Language.Expression.Dolan
import Language.Expression.Polarity
import Language.Expression.TypeF
import Pinafore.Language.GroundType
import Pinafore.Language.Type.Type
import Shapes

swapJoinRight :: JoinType a (JoinType b c) -> JoinType (JoinType a b) c
swapJoinRight (LeftJoinType v) = LeftJoinType $ LeftJoinType v
swapJoinRight (RightJoinType (LeftJoinType v)) = LeftJoinType $ RightJoinType v
swapJoinRight (RightJoinType (RightJoinType v)) = RightJoinType v

swapMeetRight :: MeetType (MeetType a b) c -> MeetType a (MeetType b c)
swapMeetRight (MkMeetType (MkMeetType (a, b), c)) = MkMeetType (a, MkMeetType (b, c))

mergeInSingularType ::
       Is PolarityType polarity
    => PinaforeSingularType baseedit polarity t
    -> PTypeF (PinaforeSingularType baseedit) polarity t
mergeInSingularType (GroundPinaforeSingularType gt args) =
    case mapDolanArguments
             mergeDuplicateGroundTypesInType
             (pinaforeGroundTypeVarianceType gt)
             (pinaforeGroundTypeVarianceMap gt)
             args of
        MkTypeF args' conv -> MkTypeF (GroundPinaforeSingularType gt args') conv
mergeInSingularType t = mkPTypeF t

mergeInTypes ::
       forall baseedit polarity ta tb. Is PolarityType polarity
    => PinaforeType baseedit polarity ta
    -> PinaforeType baseedit polarity tb
    -> PinaforeTypeF baseedit polarity (JoinMeetType polarity ta tb)
mergeInTypes ta tb =
    case representative @_ @_ @polarity of
        PositiveType -> chainTypeF mergeDuplicateGroundTypesInType $ joinPinaforeTypeF (mkPTypeF ta) (mkPTypeF tb)
        NegativeType -> chainTypeF mergeDuplicateGroundTypesInType $ meetPinaforeTypeF (mkPTypeF ta) (mkPTypeF tb)

mergeInPositiveSingularType ::
       PinaforeSingularType baseedit 'Positive t1
    -> PinaforeType baseedit 'Positive tr
    -> PinaforeTypeF baseedit 'Positive (JoinType t1 tr)
mergeInPositiveSingularType ts NilPinaforeType = mkPTypeF $ ConsPinaforeType ts NilPinaforeType
mergeInPositiveSingularType (GroundPinaforeSingularType gt1 args1) (ConsPinaforeType (GroundPinaforeSingularType gt2 args2) tr)
    | Just (Refl, HRefl) <- pinaforeGroundTypeTestEquality gt1 gt2 =
        case mergeDolanArguments
                 mergeInTypes
                 (pinaforeGroundTypeVarianceType gt1)
                 (pinaforeGroundTypeVarianceMap gt1)
                 args1
                 args2 of
            MkTypeF args' convargs ->
                contramap (joinBimap convargs id . swapJoinRight) $
                mergeInPositiveSingularType (GroundPinaforeSingularType gt1 args') tr
mergeInPositiveSingularType ts (ConsPinaforeType t1 tr) =
    case mergeInPositiveSingularType ts tr of
        MkTypeF tsr conv ->
            MkTypeF (ConsPinaforeType t1 tsr) $ \case
                LeftJoinType v -> RightJoinType $ conv $ LeftJoinType v
                RightJoinType (LeftJoinType v) -> LeftJoinType v
                RightJoinType (RightJoinType v) -> RightJoinType $ conv $ RightJoinType v

mergeInNegativeSingularType ::
       PinaforeSingularType baseedit 'Negative t1
    -> PinaforeType baseedit 'Negative tr
    -> PinaforeTypeF baseedit 'Negative (MeetType t1 tr)
mergeInNegativeSingularType ts NilPinaforeType = mkPTypeF $ ConsPinaforeType ts NilPinaforeType
mergeInNegativeSingularType (GroundPinaforeSingularType gt1 args1) (ConsPinaforeType (GroundPinaforeSingularType gt2 args2) tr)
    | Just (Refl, HRefl) <- pinaforeGroundTypeTestEquality gt1 gt2 =
        case mergeDolanArguments
                 mergeInTypes
                 (pinaforeGroundTypeVarianceType gt1)
                 (pinaforeGroundTypeVarianceMap gt1)
                 args1
                 args2 of
            MkTypeF args' convargs ->
                fmap (swapMeetRight . meetBimap convargs id) $
                mergeInNegativeSingularType (GroundPinaforeSingularType gt1 args') tr
mergeInNegativeSingularType ts (ConsPinaforeType t1 tr) =
    case mergeInNegativeSingularType ts tr of
        MkTypeF tsr conv ->
            MkTypeF (ConsPinaforeType t1 tsr) $ \(MkMeetType (a, b)) ->
                MkMeetType (meet1 $ conv b, MkMeetType (a, meet2 $ conv b))

mergeDuplicateGroundTypesInType ::
       forall baseedit polarity t. Is PolarityType polarity
    => PinaforeType baseedit polarity t
    -> PinaforeTypeF baseedit polarity t
mergeDuplicateGroundTypesInType NilPinaforeType = mkPTypeF NilPinaforeType
mergeDuplicateGroundTypesInType (ConsPinaforeType t1 tr) =
    case mergeInSingularType t1 of
        MkTypeF t1' conv1 ->
            case mergeDuplicateGroundTypesInType tr of
                MkTypeF tr' convr ->
                    case representative @_ @_ @polarity of
                        PositiveType -> contramap (joinBimap conv1 convr) $ mergeInPositiveSingularType t1' tr'
                        NegativeType -> fmap (meetBimap conv1 convr) $ mergeInNegativeSingularType t1' tr'

mergeDuplicateGroundTypes ::
       forall baseedit a. PTypeMappable (->) (PinaforeType baseedit) a
    => a
    -> a
mergeDuplicateGroundTypes =
    mapPTypes @(PinaforeType baseedit) mergeDuplicateGroundTypesInType mergeDuplicateGroundTypesInType
