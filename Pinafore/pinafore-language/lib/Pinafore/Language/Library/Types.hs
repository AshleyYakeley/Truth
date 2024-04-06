module Pinafore.Language.Library.Types where

import Pinafore.Base
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes

openEntityShimWit :: forall tid. OpenEntityType tid -> QShimWit 'Positive (OpenEntity tid)
openEntityShimWit tp = typeToDolan $ MkDolanGroundedType (openStorableGroundType tp) NilCCRArguments

concreteDynamicEntityShimWit :: FullName -> ConcreteDynamicType -> QShimWit 'Positive DynamicEntity
concreteDynamicEntityShimWit n dt =
    typeToDolan $ MkDolanGroundedType (concreteDynamicStorableGroundType n dt) NilCCRArguments

maybeShimWit :: forall a. QShimWit 'Positive a -> QShimWit 'Positive (Maybe a)
maybeShimWit swa =
    unPosShimWit swa $ \ta conva ->
        mapPosShimWit (applyCoPolyShim ccrVariation ccrVariation id conva) $
        typeToDolan $ MkDolanGroundedType maybeGroundType $ ConsCCRArguments (CoCCRPolarArgument ta) NilCCRArguments

eitherShimWit :: forall a b. QShimWit 'Positive a -> QShimWit 'Positive b -> QShimWit 'Positive (Either a b)
eitherShimWit swa swb =
    unPosShimWit swa $ \ta conva ->
        unPosShimWit swb $ \tb convb ->
            mapPosShimWit (applyCoPolyShim ccrVariation ccrVariation (cfmap conva) convb) $
            typeToDolan $
            MkDolanGroundedType eitherGroundType $
            ConsCCRArguments (CoCCRPolarArgument ta) $ ConsCCRArguments (CoCCRPolarArgument tb) NilCCRArguments

funcShimWit ::
       forall polarity (pshim :: PolyShimKind) a b.
       ( ApplyPolyShim pshim
       , JoinMeetIsoShim (pshim Type)
       , CatFunctor (CatDual (pshim Type)) (pshim (Type -> Type)) (->)
       , Is PolarityType polarity
       )
    => PShimWit (pshim Type) QType (InvertPolarity polarity) a
    -> PShimWit (pshim Type) QType polarity b
    -> PShimWit (pshim Type) QType polarity (a -> b)
funcShimWit (MkShimWit ta conva) (MkShimWit tb convb) = let
    fshim =
        case polarityType @polarity of
            PositiveType ->
                case (conva, convb) of
                    (MkPolarShim shima, MkPolarShim shimb) ->
                        MkPolarShim $ applyCoPolyShim ccrVariation ccrVariation (ccontramap shima) shimb
            NegativeType ->
                case (conva, convb) of
                    (MkPolarShim shima, MkPolarShim shimb) ->
                        MkPolarShim $ applyCoPolyShim ccrVariation ccrVariation (ccontramap shima) shimb
    in mapPolarShimWit fshim $
       typeToDolan $
       MkDolanGroundedType funcGroundType $
       ConsCCRArguments (ContraCCRPolarArgument ta) $ ConsCCRArguments (CoCCRPolarArgument tb) NilCCRArguments

actionShimWit :: forall a. QShimWit 'Positive a -> QShimWit 'Positive (Action a)
actionShimWit swa =
    unPosShimWit swa $ \ta conva ->
        mapPosShimWit (cfmap conva) $
        typeToDolan $ MkDolanGroundedType actionGroundType $ ConsCCRArguments (CoCCRPolarArgument ta) NilCCRArguments
