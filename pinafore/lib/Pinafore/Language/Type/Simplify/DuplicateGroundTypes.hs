module Pinafore.Language.Type.Simplify.DuplicateGroundTypes
    ( mergeDuplicateGroundTypesInTypes
    ) where

import Language.Expression.Dolan
import Pinafore.Language.GroundType
import Pinafore.Language.Type.Type
import Shapes

mergeJoin :: JoinType a (JoinType a b) -> JoinType a b
mergeJoin (LeftJoinType v) = LeftJoinType v
mergeJoin (RightJoinType v) = v

mergeMeet :: MeetType a b -> MeetType a (MeetType a b)
mergeMeet (MkMeetType (a, b)) = MkMeetType (a, MkMeetType (a, b))

swapJoinRight :: JoinType a (JoinType b c) -> JoinType (JoinType a b) c
swapJoinRight (LeftJoinType v) = LeftJoinType $ LeftJoinType v
swapJoinRight (RightJoinType (LeftJoinType v)) = LeftJoinType $ RightJoinType v
swapJoinRight (RightJoinType (RightJoinType v)) = RightJoinType v

swapMeetRight :: MeetType (MeetType a b) c -> MeetType a (MeetType b c)
swapMeetRight (MkMeetType (MkMeetType (a, b), c)) = MkMeetType (a, MkMeetType (b, c))

mergeDuplicatesInSingularType ::
       IsTypePolarity polarity
    => PinaforeSingularType baseedit polarity t
    -> TypeF (PinaforeSingularType baseedit) polarity t
mergeDuplicatesInSingularType (GroundPinaforeSingularType gt args) =
    case mapDolanArguments mergeDuplicatesInType (pinaforeGroundTypeKind gt) (pinaforeGroundTypeVary gt) args of
        MkTypeF args' conv -> MkTypeF (GroundPinaforeSingularType gt args') conv
mergeDuplicatesInSingularType t = mkTypeF t

mergeDuplicatePositiveSingularGroundTypes ::
       PinaforeSingularType baseedit 'PositivePolarity t1
    -> PinaforeType baseedit 'PositivePolarity tr
    -> PinaforeTypeF baseedit 'PositivePolarity (JoinType t1 tr)
mergeDuplicatePositiveSingularGroundTypes ts NilPinaforeType = mkTypeF $ ConsPinaforeType ts NilPinaforeType
mergeDuplicatePositiveSingularGroundTypes (VarPinaforeSingularType vn1) (ConsPinaforeType (VarPinaforeSingularType vn2) tr)
    | Just Refl <- testEquality vn1 vn2 =
        contramap mergeJoin $ mergeDuplicatePositiveSingularGroundTypes (VarPinaforeSingularType vn1) tr
mergeDuplicatePositiveSingularGroundTypes (GroundPinaforeSingularType gt1 args1) (ConsPinaforeType (GroundPinaforeSingularType gt2 args2) tr)
    | Just (Refl, HRefl) <- testPinaforeGroundTypeEquality gt1 gt2 =
        case mergeDolanArguments
                 mergeDuplicatesInTypes
                 (pinaforeGroundTypeKind gt1)
                 (pinaforeGroundTypeVary gt1)
                 args1
                 args2 of
            MkTypeF args' convargs ->
                contramap (joinBimap convargs id . swapJoinRight) $
                mergeDuplicatePositiveSingularGroundTypes (GroundPinaforeSingularType gt1 args') tr
mergeDuplicatePositiveSingularGroundTypes ts (ConsPinaforeType t1 tr) =
    case mergeDuplicatePositiveSingularGroundTypes ts tr of
        MkTypeF tsr conv ->
            MkTypeF (ConsPinaforeType t1 tsr) $ \case
                LeftJoinType v -> RightJoinType $ conv $ LeftJoinType v
                RightJoinType (LeftJoinType v) -> LeftJoinType v
                RightJoinType (RightJoinType v) -> RightJoinType $ conv $ RightJoinType v

mergeDuplicateNegativeSingularGroundTypes ::
       PinaforeSingularType baseedit 'NegativePolarity t1
    -> PinaforeType baseedit 'NegativePolarity tr
    -> PinaforeTypeF baseedit 'NegativePolarity (MeetType t1 tr)
mergeDuplicateNegativeSingularGroundTypes ts NilPinaforeType = mkTypeF $ ConsPinaforeType ts NilPinaforeType
mergeDuplicateNegativeSingularGroundTypes (VarPinaforeSingularType vn1) (ConsPinaforeType (VarPinaforeSingularType vn2) tr)
    | Just Refl <- testEquality vn1 vn2 =
        fmap mergeMeet $ mergeDuplicateNegativeSingularGroundTypes (VarPinaforeSingularType vn1) tr
mergeDuplicateNegativeSingularGroundTypes (GroundPinaforeSingularType gt1 args1) (ConsPinaforeType (GroundPinaforeSingularType gt2 args2) tr)
    | Just (Refl, HRefl) <- testPinaforeGroundTypeEquality gt1 gt2 =
        case mergeDolanArguments
                 mergeDuplicatesInTypes
                 (pinaforeGroundTypeKind gt1)
                 (pinaforeGroundTypeVary gt1)
                 args1
                 args2 of
            MkTypeF args' convargs ->
                fmap (swapMeetRight . meetBimap convargs id) $
                mergeDuplicateNegativeSingularGroundTypes (GroundPinaforeSingularType gt1 args') tr
mergeDuplicateNegativeSingularGroundTypes ts (ConsPinaforeType t1 tr) =
    case mergeDuplicateNegativeSingularGroundTypes ts tr of
        MkTypeF tsr conv ->
            MkTypeF (ConsPinaforeType t1 tsr) $ \(MkMeetType (a, b)) ->
                MkMeetType (meet1 $ conv b, MkMeetType (a, meet2 $ conv b))

mergeDuplicatesInType ::
       forall baseedit polarity t. IsTypePolarity polarity
    => PinaforeType baseedit polarity t
    -> PinaforeTypeF baseedit polarity t
mergeDuplicatesInType NilPinaforeType = mkTypeF NilPinaforeType
mergeDuplicatesInType (ConsPinaforeType t1 tr) =
    case mergeDuplicatesInSingularType t1 of
        MkTypeF t1' conv1 ->
            case mergeDuplicatesInType tr of
                MkTypeF tr' convr ->
                    case whichTypePolarity @polarity of
                        Left Refl ->
                            contramap (joinBimap conv1 convr) $ mergeDuplicatePositiveSingularGroundTypes t1' tr'
                        Right Refl -> fmap (meetBimap conv1 convr) $ mergeDuplicateNegativeSingularGroundTypes t1' tr'

mergeDuplicatesInTypes ::
       forall baseedit polarity ta tb. IsTypePolarity polarity
    => PinaforeType baseedit polarity ta
    -> PinaforeType baseedit polarity tb
    -> PinaforeTypeF baseedit polarity (JoinMeetType polarity ta tb)
mergeDuplicatesInTypes ta tb =
    case whichTypePolarity @polarity of
        Left Refl -> chainTypeF mergeDuplicatesInType $ joinPinaforeTypeF (mkTypeF ta) (mkTypeF tb)
        Right Refl -> chainTypeF mergeDuplicatesInType $ meetPinaforeTypeF (mkTypeF ta) (mkTypeF tb)

mergeDuplicateGroundTypesInTypes :: PinaforeExpression baseedit name -> PinaforeExpression baseedit name
mergeDuplicateGroundTypesInTypes = mapSealedExpressionTypes mergeDuplicatesInType mergeDuplicatesInType
