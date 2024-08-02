module Pinafore.Language.Library.Types
    ( openEntityShimWit
    , maybeShimWit
    , listShimWit
    , list1ShimWit
    , eitherShimWit
    , resultShimWit
    , pairShimWit
    , funcShimWit
    , actionShimWit
    ) where

import Import
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Type

openEntityShimWit :: forall tid. OpenEntityType tid -> QShimWit 'Positive (OpenEntity tid)
openEntityShimWit tp = typeToDolan $ MkDolanGroundedType (openStorableGroundType tp) NilCCRArguments

coFShimWit ::
       forall f polarity a. (HasVariance f, VarianceOf f ~ Covariance, Is PolarityType polarity)
    => QGroundType '[ CoCCRVariance] f
    -> QShimWit polarity a
    -> QShimWit polarity (f a)
coFShimWit gt wa =
    shimWitToDolan $
    mkDolanGroundedShimWit gt $
    consCCRPolarArgumentsShimWit (qgtVarianceMap gt) (coCCRArgument wa) nilCCRPolarArgumentsShimWit

maybeShimWit ::
       forall polarity a. Is PolarityType polarity
    => QShimWit polarity a
    -> QShimWit polarity (Maybe a)
maybeShimWit = coFShimWit maybeGroundType

listShimWit ::
       forall polarity a. Is PolarityType polarity
    => QShimWit polarity a
    -> QShimWit polarity [a]
listShimWit = coFShimWit listGroundType

list1ShimWit ::
       forall polarity a. Is PolarityType polarity
    => QShimWit polarity a
    -> QShimWit polarity (NonEmpty a)
list1ShimWit = coFShimWit list1GroundType

cocoFShimWit ::
       forall f polarity a b. Is PolarityType polarity
    => QGroundType '[ CoCCRVariance, CoCCRVariance] f
    -> QShimWit polarity a
    -> QShimWit polarity b
    -> QShimWit polarity (f a b)
cocoFShimWit gt wa wb =
    shimWitToDolan $
    mkDolanGroundedShimWit gt $
    consCCRPolarArgumentsShimWit (qgtVarianceMap gt) (coCCRArgument wa) $
    consCCRPolarArgumentsShimWit (nextCCRVariancesMap $ qgtVarianceMap gt) (coCCRArgument wb) $
    nilCCRPolarArgumentsShimWit

eitherShimWit ::
       forall polarity a b. Is PolarityType polarity
    => QShimWit polarity a
    -> QShimWit polarity b
    -> QShimWit polarity (Either a b)
eitherShimWit = cocoFShimWit eitherGroundType

resultShimWit ::
       forall polarity a b. Is PolarityType polarity
    => QShimWit polarity a
    -> QShimWit polarity b
    -> QShimWit polarity (Result a b)
resultShimWit = cocoFShimWit resultGroundType

pairShimWit ::
       forall polarity a b. Is PolarityType polarity
    => QShimWit polarity a
    -> QShimWit polarity b
    -> QShimWit polarity (a, b)
pairShimWit = cocoFShimWit pairGroundType

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
