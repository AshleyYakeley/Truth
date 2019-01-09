module Pinafore.Language.Type.Simplify.DuplicateGroundTypes
    ( mergeDuplicateGroundTypesInType
    , mergeDuplicateGroundTypesInExpression
    , mergeDuplicateGroundTypesInPattern
    ) where

import Language.Expression.Dolan
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
       IsTypePolarity polarity
    => PinaforeSingularType baseedit polarity t
    -> TypeF (PinaforeSingularType baseedit) polarity t
mergeInSingularType (GroundPinaforeSingularType gt args) =
    case mapDolanArguments mergeDuplicateGroundTypesInType (pinaforeGroundTypeKind gt) (pinaforeGroundTypeVary gt) args of
        MkTypeF args' conv -> MkTypeF (GroundPinaforeSingularType gt args') conv
mergeInSingularType t = mkTypeF t

mergeInTypes ::
       forall baseedit polarity ta tb. IsTypePolarity polarity
    => PinaforeType baseedit polarity ta
    -> PinaforeType baseedit polarity tb
    -> PinaforeTypeF baseedit polarity (JoinMeetType polarity ta tb)
mergeInTypes ta tb =
    case whichTypePolarity @polarity of
        Left Refl -> chainTypeF mergeDuplicateGroundTypesInType $ joinPinaforeTypeF (mkTypeF ta) (mkTypeF tb)
        Right Refl -> chainTypeF mergeDuplicateGroundTypesInType $ meetPinaforeTypeF (mkTypeF ta) (mkTypeF tb)

mergeInPositiveSingularType ::
       PinaforeSingularType baseedit 'PositivePolarity t1
    -> PinaforeType baseedit 'PositivePolarity tr
    -> PinaforeTypeF baseedit 'PositivePolarity (JoinType t1 tr)
mergeInPositiveSingularType ts NilPinaforeType = mkTypeF $ ConsPinaforeType ts NilPinaforeType
mergeInPositiveSingularType (GroundPinaforeSingularType gt1 args1) (ConsPinaforeType (GroundPinaforeSingularType gt2 args2) tr)
    | Just (Refl, HRefl) <- testPinaforeGroundTypeEquality gt1 gt2 =
        case mergeDolanArguments mergeInTypes (pinaforeGroundTypeKind gt1) (pinaforeGroundTypeVary gt1) args1 args2 of
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
       PinaforeSingularType baseedit 'NegativePolarity t1
    -> PinaforeType baseedit 'NegativePolarity tr
    -> PinaforeTypeF baseedit 'NegativePolarity (MeetType t1 tr)
mergeInNegativeSingularType ts NilPinaforeType = mkTypeF $ ConsPinaforeType ts NilPinaforeType
mergeInNegativeSingularType (GroundPinaforeSingularType gt1 args1) (ConsPinaforeType (GroundPinaforeSingularType gt2 args2) tr)
    | Just (Refl, HRefl) <- testPinaforeGroundTypeEquality gt1 gt2 =
        case mergeDolanArguments mergeInTypes (pinaforeGroundTypeKind gt1) (pinaforeGroundTypeVary gt1) args1 args2 of
            MkTypeF args' convargs ->
                fmap (swapMeetRight . meetBimap convargs id) $
                mergeInNegativeSingularType (GroundPinaforeSingularType gt1 args') tr
mergeInNegativeSingularType ts (ConsPinaforeType t1 tr) =
    case mergeInNegativeSingularType ts tr of
        MkTypeF tsr conv ->
            MkTypeF (ConsPinaforeType t1 tsr) $ \(MkMeetType (a, b)) ->
                MkMeetType (meet1 $ conv b, MkMeetType (a, meet2 $ conv b))

mergeDuplicateGroundTypesInType ::
       forall baseedit polarity t. IsTypePolarity polarity
    => PinaforeType baseedit polarity t
    -> PinaforeTypeF baseedit polarity t
mergeDuplicateGroundTypesInType NilPinaforeType = mkTypeF NilPinaforeType
mergeDuplicateGroundTypesInType (ConsPinaforeType t1 tr) =
    case mergeInSingularType t1 of
        MkTypeF t1' conv1 ->
            case mergeDuplicateGroundTypesInType tr of
                MkTypeF tr' convr ->
                    case whichTypePolarity @polarity of
                        Left Refl -> contramap (joinBimap conv1 convr) $ mergeInPositiveSingularType t1' tr'
                        Right Refl -> fmap (meetBimap conv1 convr) $ mergeInNegativeSingularType t1' tr'

mergeDuplicateGroundTypesInExpression :: PinaforeExpression baseedit -> PinaforeExpression baseedit
mergeDuplicateGroundTypesInExpression =
    mapSealedExpressionTypes mergeDuplicateGroundTypesInType mergeDuplicateGroundTypesInType

mergeDuplicateGroundTypesInPattern :: PinaforePattern baseedit -> PinaforePattern baseedit
mergeDuplicateGroundTypesInPattern =
    mapSealedPatternTypes mergeDuplicateGroundTypesInType mergeDuplicateGroundTypesInType
