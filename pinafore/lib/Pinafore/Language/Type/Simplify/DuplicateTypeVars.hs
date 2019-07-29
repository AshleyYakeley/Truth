module Pinafore.Language.Type.Simplify.DuplicateTypeVars
    ( mergeDuplicateTypeVars
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
             mergeDuplicateTypeVarsInType
             (pinaforeGroundTypeVarianceType gt)
             (pinaforeGroundTypeVarianceMap gt)
             args of
        MkShimWit args' conv -> MkShimWit (GroundPinaforeSingularType gt args') conv
mergeInSingularType t = mkPJMShimWit t

mergeInPositiveSingularType ::
       PinaforeSingularType baseedit 'Positive t1
    -> PinaforeType baseedit 'Positive tr
    -> PinaforeShimWit baseedit 'Positive (JoinType t1 tr)
mergeInPositiveSingularType ts NilPinaforeType = mkPJMShimWit $ ConsPinaforeType ts NilPinaforeType
mergeInPositiveSingularType (VarPinaforeSingularType vn1) (ConsPinaforeType (VarPinaforeSingularType vn2) tr)
    | Just Refl <- testEquality vn1 vn2 =
        ccontramap (joinf join1 id :: JMShim _ _) $ mergeInPositiveSingularType (VarPinaforeSingularType vn1) tr
mergeInPositiveSingularType ts (ConsPinaforeType t1 tr) =
    case mergeInPositiveSingularType ts tr of
        MkShimWit tsr conv ->
            MkShimWit (ConsPinaforeType t1 tsr) $ joinf (join2 . conv . join1) (joinBimap id $ conv . join2)

mergeInNegativeSingularType ::
       PinaforeSingularType baseedit 'Negative t1
    -> PinaforeType baseedit 'Negative tr
    -> PinaforeShimWit baseedit 'Negative (MeetType t1 tr)
mergeInNegativeSingularType ts NilPinaforeType = mkPJMShimWit $ ConsPinaforeType ts NilPinaforeType
mergeInNegativeSingularType (VarPinaforeSingularType vn1) (ConsPinaforeType (VarPinaforeSingularType vn2) tr)
    | Just Refl <- testEquality vn1 vn2 =
        cfmap (meetf meet1 id :: JMShim _ _) $ mergeInNegativeSingularType (VarPinaforeSingularType vn1) tr
mergeInNegativeSingularType ts (ConsPinaforeType t1 tr) =
    case mergeInNegativeSingularType ts tr of
        MkShimWit tsr conv ->
            MkShimWit (ConsPinaforeType t1 tsr) $ meetf (meet1 . conv . meet2) (meetBimap id $ meet2 . conv)

mergeDuplicateTypeVarsInType ::
       forall baseedit polarity t. Is PolarityType polarity
    => PinaforeType baseedit polarity t
    -> PinaforeShimWit baseedit polarity t
mergeDuplicateTypeVarsInType NilPinaforeType = mkPJMShimWit NilPinaforeType
mergeDuplicateTypeVarsInType (ConsPinaforeType t1 tr) =
    case mergeInSingularType t1 of
        MkShimWit t1' conv1 ->
            case mergeDuplicateTypeVarsInType tr of
                MkShimWit tr' convr ->
                    case representative @_ @_ @polarity of
                        PositiveType -> ccontramap (joinBimap conv1 convr) $ mergeInPositiveSingularType t1' tr'
                        NegativeType -> cfmap (meetBimap conv1 convr) $ mergeInNegativeSingularType t1' tr'

mergeDuplicateTypeVars ::
       forall baseedit a. PShimWitMappable PinaforeShim (PinaforeType baseedit) a
    => a
    -> a
mergeDuplicateTypeVars =
    mapPShimWits @_ @(PinaforeType baseedit) mergeDuplicateTypeVarsInType mergeDuplicateTypeVarsInType
