module Pinafore.Language.Library.Std.Types where

import Pinafore.Base
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes

openEntityShimWit :: forall tid. OpenEntityType tid -> PinaforeShimWit 'Positive (OpenEntity tid)
openEntityShimWit tp =
    singleDolanShimWit $ mkPolarShimWit $ GroundedDolanSingularType (openEntityGroundType tp) NilCCRArguments

dynamicEntityShimWit :: Name -> DynamicType -> PinaforeShimWit 'Positive DynamicEntity
dynamicEntityShimWit n dt =
    singleDolanShimWit $
    mkPolarShimWit $ GroundedDolanSingularType (aDynamicEntityGroundType n $ singletonSet dt) NilCCRArguments

maybeShimWit :: forall a. PinaforeShimWit 'Positive a -> PinaforeShimWit 'Positive (Maybe a)
maybeShimWit swa =
    unPosShimWit swa $ \ta conva ->
        mapPosShimWit (applyCoPolyShim ccrVariation ccrVariation id conva) $
        singleDolanShimWit $
        mkPolarShimWit $
        GroundedDolanSingularType maybeGroundType $ ConsCCRArguments (CoCCRPolarArgument ta) NilCCRArguments

eitherShimWit ::
       forall a b. PinaforeShimWit 'Positive a -> PinaforeShimWit 'Positive b -> PinaforeShimWit 'Positive (Either a b)
eitherShimWit swa swb =
    unPosShimWit swa $ \ta conva ->
        unPosShimWit swb $ \tb convb ->
            mapPosShimWit (applyCoPolyShim ccrVariation ccrVariation (cfmap conva) convb) $
            singleDolanShimWit $
            mkPolarShimWit $
            GroundedDolanSingularType eitherGroundType $
            ConsCCRArguments (CoCCRPolarArgument ta) $ ConsCCRArguments (CoCCRPolarArgument tb) NilCCRArguments

funcShimWit ::
       forall polarity a b. Is PolarityType polarity
    => PinaforeShimWit (InvertPolarity polarity) a
    -> PinaforeShimWit polarity b
    -> PinaforeShimWit polarity (a -> b)
funcShimWit (MkShimWit ta conva) (MkShimWit tb convb) = let
    fshim =
        case polarityType @polarity of
            PositiveType ->
                case (conva, convb) of
                    (MkPolarMap shima, MkPolarMap shimb) ->
                        MkPolarMap $ applyCoPolyShim ccrVariation ccrVariation (ccontramap shima) shimb
            NegativeType ->
                case (conva, convb) of
                    (MkPolarMap shima, MkPolarMap shimb) ->
                        MkPolarMap $ applyCoPolyShim ccrVariation ccrVariation (ccontramap shima) shimb
    in mapPolarShimWit fshim $
       singleDolanShimWit $
       mkPolarShimWit $
       GroundedDolanSingularType funcGroundType $
       ConsCCRArguments (ContraCCRPolarArgument ta) $ ConsCCRArguments (CoCCRPolarArgument tb) NilCCRArguments

actionShimWit :: forall a. PinaforeShimWit 'Positive a -> PinaforeShimWit 'Positive (PinaforeAction a)
actionShimWit swa =
    unPosShimWit swa $ \ta conva ->
        mapPosShimWit (cfmap conva) $
        singleDolanShimWit $
        mkPolarShimWit $
        GroundedDolanSingularType actionGroundType $ ConsCCRArguments (CoCCRPolarArgument ta) NilCCRArguments
