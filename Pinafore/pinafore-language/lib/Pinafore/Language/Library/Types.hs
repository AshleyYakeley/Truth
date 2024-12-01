module Pinafore.Language.Library.Types
    ( coShimWit
    , contraShimWit
    , rangeShimWit
    , cocoShimWit
    , openEntityShimWit
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
import Pinafore.Language.Type

openEntityShimWit :: forall tid. OpenEntityType tid -> QShimWit 'Positive (OpenEntity tid)
openEntityShimWit tp = typeToDolan $ MkDolanGroundedType (openEntityGroundType tp) NilCCRArguments

coShimWit ::
       forall f polarity a. Is PolarityType polarity
    => QGroundType '[ CoCCRVariance] f
    -> QShimWit polarity a
    -> QShimWit polarity (f a)
coShimWit gt wa =
    shimWitToDolan $
    mkDolanGroundedShimWit gt $
    consCCRPolarArgumentsShimWit (qgtVarianceMap gt) (coCCRArgument wa) nilCCRPolarArgumentsShimWit

contraShimWit ::
       forall f polarity a. Is PolarityType polarity
    => QGroundType '[ ContraCCRVariance] f
    -> QShimWit (InvertPolarity polarity) a
    -> QShimWit polarity (f a)
contraShimWit gt wa =
    shimWitToDolan $
    mkDolanGroundedShimWit gt $
    consCCRPolarArgumentsShimWit (qgtVarianceMap gt) (contraCCRArgument wa) nilCCRPolarArgumentsShimWit

rangeShimWit ::
       forall f polarity p q. Is PolarityType polarity
    => QGroundType '[ 'RangeCCRVariance] f
    -> QShimWit (InvertPolarity polarity) p
    -> QShimWit polarity q
    -> QShimWit polarity (f '( p, q))
rangeShimWit gt wp wq =
    shimWitToDolan $
    mkDolanGroundedShimWit gt $
    consCCRPolarArgumentsShimWit (qgtVarianceMap gt) (rangeCCRArgument wp wq) nilCCRPolarArgumentsShimWit

maybeShimWit ::
       forall polarity a. Is PolarityType polarity
    => QShimWit polarity a
    -> QShimWit polarity (Maybe a)
maybeShimWit = coShimWit maybeGroundType

listShimWit ::
       forall polarity a. Is PolarityType polarity
    => QShimWit polarity a
    -> QShimWit polarity [a]
listShimWit = coShimWit listGroundType

list1ShimWit ::
       forall polarity a. Is PolarityType polarity
    => QShimWit polarity a
    -> QShimWit polarity (NonEmpty a)
list1ShimWit = coShimWit list1GroundType

cocoShimWit ::
       forall f polarity a b. Is PolarityType polarity
    => QGroundType '[ CoCCRVariance, CoCCRVariance] f
    -> QShimWit polarity a
    -> QShimWit polarity b
    -> QShimWit polarity (f a b)
cocoShimWit gt wa wb =
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
eitherShimWit = cocoShimWit eitherGroundType

resultShimWit ::
       forall polarity a b. Is PolarityType polarity
    => QShimWit polarity a
    -> QShimWit polarity b
    -> QShimWit polarity (Result a b)
resultShimWit = cocoShimWit resultGroundType

pairShimWit ::
       forall polarity a b. Is PolarityType polarity
    => QShimWit polarity a
    -> QShimWit polarity b
    -> QShimWit polarity (a, b)
pairShimWit = cocoShimWit pairGroundType

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
