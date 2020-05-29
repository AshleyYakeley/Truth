module Pinafore.Language.TypeSystem.Simplify.DuplicateTypeVars
    ( mergeDuplicateTypeVars
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
             mergeDuplicateTypeVarsInType
             (pinaforeGroundTypeVarianceType gt)
             (pinaforeGroundTypeVarianceMap gt)
             args of
        MkShimWit args' conv -> MkShimWit (GroundPinaforeSingularType gt args') conv
mergeInSingularType t = mkPJMShimWit t

mergeIn1SingularType ::
       forall polarity t1 tr. Is PolarityType polarity
    => PinaforeSingularType polarity t1
    -> PinaforeType polarity tr
    -> PinaforeShimWit polarity (JoinMeetType polarity t1 tr)
mergeIn1SingularType ts NilPinaforeType = mkPJMShimWit $ ConsPinaforeType ts NilPinaforeType
mergeIn1SingularType (VarPinaforeSingularType vn1) (ConsPinaforeType (VarPinaforeSingularType vn2) tr)
    | Just Refl <- testEquality vn1 vn2 =
        ccontramap (polarF polar1 id :: PolarMap JMShim polarity _ _) $
        mergeIn1SingularType (VarPinaforeSingularType vn1) tr
mergeIn1SingularType ts (ConsPinaforeType t1 tr) =
    case mergeIn1SingularType ts tr of
        MkShimWit tsr conv ->
            MkShimWit (ConsPinaforeType t1 tsr) $ polarF (polar2 . conv . polar1) (polarBimap id $ conv . polar2)

mergeDuplicateTypeVarsInType ::
       forall polarity t. Is PolarityType polarity
    => PinaforeType polarity t
    -> PinaforeShimWit polarity t
mergeDuplicateTypeVarsInType NilPinaforeType = mkPJMShimWit NilPinaforeType
mergeDuplicateTypeVarsInType (ConsPinaforeType t1 tr) =
    case mergeInSingularType t1 of
        MkShimWit t1' conv1 ->
            case mergeDuplicateTypeVarsInType tr of
                MkShimWit tr' convr -> ccontramap (polarBimap conv1 convr) $ mergeIn1SingularType t1' tr'

mergeDuplicateTypeVars ::
       forall a. PShimWitMappable PinaforeShim PinaforeType a
    => a
    -> a
mergeDuplicateTypeVars = mapPShimWits @_ @PinaforeType mergeDuplicateTypeVarsInType mergeDuplicateTypeVarsInType
