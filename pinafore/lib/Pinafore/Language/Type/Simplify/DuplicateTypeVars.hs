module Pinafore.Language.Type.Simplify.DuplicateTypeVars
    ( mergeDuplicateTypeVars
    ) where

import Language.Expression.Dolan
import Language.Expression.Polarity
import Language.Expression.TypeF
import Pinafore.Language.GroundType
import Pinafore.Language.Type.Type
import Shapes

mergeJoin :: JoinType a (JoinType a b) -> JoinType a b
mergeJoin (LeftJoinType v) = LeftJoinType v
mergeJoin (RightJoinType v) = v

mergeMeet :: MeetType a b -> MeetType a (MeetType a b)
mergeMeet (MkMeetType (a, b)) = MkMeetType (a, MkMeetType (a, b))

mergeInSingularType ::
       Is PolarityType polarity
    => PinaforeSingularType baseedit polarity t
    -> PTypeF (PinaforeSingularType baseedit) polarity t
mergeInSingularType (GroundPinaforeSingularType gt args) =
    case mapDolanArguments
             mergeDuplicateTypeVarsInType
             (pinaforeGroundTypeVarianceType gt)
             (pinaforeGroundTypeVarianceMap gt)
             args of
        MkTypeF args' conv -> MkTypeF (GroundPinaforeSingularType gt args') conv
mergeInSingularType t = mkPTypeF t

mergeInPositiveSingularType ::
       PinaforeSingularType baseedit 'Positive t1
    -> PinaforeType baseedit 'Positive tr
    -> PinaforeTypeF baseedit 'Positive (JoinType t1 tr)
mergeInPositiveSingularType ts NilPinaforeType = mkPTypeF $ ConsPinaforeType ts NilPinaforeType
mergeInPositiveSingularType (VarPinaforeSingularType vn1) (ConsPinaforeType (VarPinaforeSingularType vn2) tr)
    | Just Refl <- testEquality vn1 vn2 =
        contramap mergeJoin $ mergeInPositiveSingularType (VarPinaforeSingularType vn1) tr
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
mergeInNegativeSingularType (VarPinaforeSingularType vn1) (ConsPinaforeType (VarPinaforeSingularType vn2) tr)
    | Just Refl <- testEquality vn1 vn2 = fmap mergeMeet $ mergeInNegativeSingularType (VarPinaforeSingularType vn1) tr
mergeInNegativeSingularType ts (ConsPinaforeType t1 tr) =
    case mergeInNegativeSingularType ts tr of
        MkTypeF tsr conv ->
            MkTypeF (ConsPinaforeType t1 tsr) $ \(MkMeetType (a, b)) ->
                MkMeetType (meet1 $ conv b, MkMeetType (a, meet2 $ conv b))

mergeDuplicateTypeVarsInType ::
       forall baseedit polarity t. Is PolarityType polarity
    => PinaforeType baseedit polarity t
    -> PinaforeTypeF baseedit polarity t
mergeDuplicateTypeVarsInType NilPinaforeType = mkPTypeF NilPinaforeType
mergeDuplicateTypeVarsInType (ConsPinaforeType t1 tr) =
    case mergeInSingularType t1 of
        MkTypeF t1' conv1 ->
            case mergeDuplicateTypeVarsInType tr of
                MkTypeF tr' convr ->
                    case representative @_ @_ @polarity of
                        PositiveType -> contramap (joinBimap conv1 convr) $ mergeInPositiveSingularType t1' tr'
                        NegativeType -> fmap (meetBimap conv1 convr) $ mergeInNegativeSingularType t1' tr'

mergeDuplicateTypeVars ::
       forall baseedit a. PTypeMappable (->) (PinaforeType baseedit) a
    => a
    -> a
mergeDuplicateTypeVars = mapPTypes @(PinaforeType baseedit) mergeDuplicateTypeVarsInType mergeDuplicateTypeVarsInType
