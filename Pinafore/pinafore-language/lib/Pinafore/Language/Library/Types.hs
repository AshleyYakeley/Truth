module Pinafore.Language.Library.Types
    ( openEntityShimWit
    , concreteDynamicEntityShimWit
    , maybeShimWit
    , listShimWit
    , list1ShimWit
    , eitherShimWit
    , resultShimWit
    , nullShimWit
    , pairShimWit
    , funcShimWit
    , actionShimWit
    ) where

import Import
import Pinafore.Language.Convert.Types
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Type

openEntityShimWit :: forall tid. OpenEntityType tid -> QShimWit 'Positive (OpenEntity tid)
openEntityShimWit tp = typeToDolan $ MkDolanGroundedType (openStorableGroundType tp) NilCCRArguments

concreteDynamicEntityShimWit :: FullName -> ConcreteDynamicType -> QShimWit 'Positive DynamicEntity
concreteDynamicEntityShimWit n dt =
    typeToDolan $ MkDolanGroundedType (concreteDynamicStorableGroundType n dt) NilCCRArguments

coFShimWit ::
       forall f polarity a. (HasVariance f, VarianceOf f ~ Covariance, Is PolarityType polarity)
    => QGroundType '[ CoCCRVariance] f
    -> QShimWit polarity a
    -> QShimWit polarity (f a)
coFShimWit gt (MkShimWit ta conva) = let
    fshim =
        case polarityType @polarity of
            PositiveType ->
                case conva of
                    MkPolarShim shima -> MkPolarShim $ applyCoPolyShim ccrVariation ccrVariation id shima
            NegativeType ->
                case conva of
                    MkPolarShim shima -> MkPolarShim $ applyCoPolyShim ccrVariation ccrVariation id shima
    in mapPolarShimWit fshim $
       typeToDolan $ MkDolanGroundedType gt $ ConsCCRArguments (CoCCRPolarArgument ta) NilCCRArguments

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
cocoFShimWit gt (MkShimWit ta conva) (MkShimWit tb convb) = let
    fshim =
        case qgtVarianceMap gt of
            ConsCCRVariancesMap ccrva mapb ->
                case polarityType @polarity of
                    PositiveType ->
                        case (conva, convb) of
                            (MkPolarShim (shima :: _ x y), MkPolarShim shimb) ->
                                MkPolarShim $
                                applyCoPolyShim
                                    (case mapb of
                                         ConsCCRVariancesMap ccrvb NilCCRVariancesMap -> ccrvb)
                                    (case mapb of
                                         ConsCCRVariancesMap ccrvb NilCCRVariancesMap -> ccrvb)
                                    (applyCoPolyShim ccrva ccrva id shima)
                                    shimb
                    NegativeType ->
                        case (conva, convb) of
                            (MkPolarShim (shima :: _ x y), MkPolarShim shimb) ->
                                MkPolarShim $
                                applyCoPolyShim
                                    (case mapb of
                                         ConsCCRVariancesMap ccrvb NilCCRVariancesMap -> ccrvb)
                                    (case mapb of
                                         ConsCCRVariancesMap ccrvb NilCCRVariancesMap -> ccrvb)
                                    (applyCoPolyShim ccrva ccrva id shima)
                                    shimb
    in mapPolarShimWit fshim $
       typeToDolan $
       MkDolanGroundedType gt $
       ConsCCRArguments (CoCCRPolarArgument ta) $ ConsCCRArguments (CoCCRPolarArgument tb) NilCCRArguments

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

nullShimWit ::
       forall polarity. Is PolarityType polarity
    => QShimWit polarity ()
nullShimWit = typeToDolan $ MkDolanGroundedType unitGroundType NilCCRArguments

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
