module Pinafore.Language.Type.Simplify.DuplicateTypeVars
    ( mergeDuplicateTypeVarsInTypes
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

mergeInSingularType ::
       IsTypePolarity polarity
    => PinaforeSingularType baseedit polarity t
    -> TypeF (PinaforeSingularType baseedit) polarity t
mergeInSingularType (GroundPinaforeSingularType gt args) =
    case mapDolanArguments mergeInType (pinaforeGroundTypeKind gt) (pinaforeGroundTypeVary gt) args of
        MkTypeF args' conv -> MkTypeF (GroundPinaforeSingularType gt args') conv
mergeInSingularType t = mkTypeF t

mergeInPositiveSingularType ::
       PinaforeSingularType baseedit 'PositivePolarity t1
    -> PinaforeType baseedit 'PositivePolarity tr
    -> PinaforeTypeF baseedit 'PositivePolarity (JoinType t1 tr)
mergeInPositiveSingularType ts NilPinaforeType = mkTypeF $ ConsPinaforeType ts NilPinaforeType
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
       PinaforeSingularType baseedit 'NegativePolarity t1
    -> PinaforeType baseedit 'NegativePolarity tr
    -> PinaforeTypeF baseedit 'NegativePolarity (MeetType t1 tr)
mergeInNegativeSingularType ts NilPinaforeType = mkTypeF $ ConsPinaforeType ts NilPinaforeType
mergeInNegativeSingularType (VarPinaforeSingularType vn1) (ConsPinaforeType (VarPinaforeSingularType vn2) tr)
    | Just Refl <- testEquality vn1 vn2 = fmap mergeMeet $ mergeInNegativeSingularType (VarPinaforeSingularType vn1) tr
mergeInNegativeSingularType ts (ConsPinaforeType t1 tr) =
    case mergeInNegativeSingularType ts tr of
        MkTypeF tsr conv ->
            MkTypeF (ConsPinaforeType t1 tsr) $ \(MkMeetType (a, b)) ->
                MkMeetType (meet1 $ conv b, MkMeetType (a, meet2 $ conv b))

mergeInType ::
       forall baseedit polarity t. IsTypePolarity polarity
    => PinaforeType baseedit polarity t
    -> PinaforeTypeF baseedit polarity t
mergeInType NilPinaforeType = mkTypeF NilPinaforeType
mergeInType (ConsPinaforeType t1 tr) =
    case mergeInSingularType t1 of
        MkTypeF t1' conv1 ->
            case mergeInType tr of
                MkTypeF tr' convr ->
                    case whichTypePolarity @polarity of
                        Left Refl -> contramap (joinBimap conv1 convr) $ mergeInPositiveSingularType t1' tr'
                        Right Refl -> fmap (meetBimap conv1 convr) $ mergeInNegativeSingularType t1' tr'

mergeDuplicateTypeVarsInTypes :: PinaforeExpression baseedit name -> PinaforeExpression baseedit name
mergeDuplicateTypeVarsInTypes = mapSealedExpressionTypes mergeInType mergeInType
