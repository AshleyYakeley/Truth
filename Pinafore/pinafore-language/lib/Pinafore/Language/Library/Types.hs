module Pinafore.Language.Library.Types
    ( cocoShimWit
    , openEntityShimWit
    , maybeShimWit
    , listShimWit
    , list1ShimWit
    , eitherShimWit
    , resultShimWit
    , pairShimWit
    , funcShimWit
    , actionShimWit
    )
where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Type

openEntityShimWit :: forall tid. OpenEntityType tid -> QShimWit 'Positive (OpenEntity tid)
openEntityShimWit tp = typeToDolan $ MkDolanGroundedType (openEntityGroundType tp) NilCCRArguments

maybeShimWit ::
    forall polarity a.
    Is PolarityType polarity =>
    QShimWit polarity a ->
    QShimWit polarity (LangMaybe a)
maybeShimWit = coShimWit qGroundType

listShimWit ::
    forall polarity a.
    Is PolarityType polarity =>
    QShimWit polarity a ->
    QShimWit polarity (LangList a)
listShimWit = coShimWit qGroundType

list1ShimWit ::
    forall polarity a.
    Is PolarityType polarity =>
    QShimWit polarity a ->
    QShimWit
        polarity
        ( LangList1
            a
        )
list1ShimWit = coShimWit qGroundType

eitherShimWit ::
    forall polarity a b.
    Is PolarityType polarity =>
    QShimWit polarity a ->
    QShimWit polarity b ->
    QShimWit polarity (Either a b)
eitherShimWit = cocoShimWit qGroundType

resultShimWit ::
    forall polarity a b.
    Is PolarityType polarity =>
    QShimWit polarity a ->
    QShimWit polarity b ->
    QShimWit polarity (Result a b)
resultShimWit = cocoShimWit qGroundType

pairShimWit ::
    forall polarity a b.
    Is PolarityType polarity =>
    QShimWit polarity a ->
    QShimWit polarity b ->
    QShimWit polarity (a, b)
pairShimWit = cocoShimWit qGroundType

funcShimWit ::
    forall polarity (pshim :: PolyShimKind) a b.
    ( ApplyPolyShim pshim
    , JoinMeetIsoShim (pshim Type)
    , CatFunctor (CatDual (pshim Type)) (pshim (Type -> Type)) (->)
    , Is PolarityType polarity
    ) =>
    PShimWit (pshim Type) QType (InvertPolarity polarity) a ->
    PShimWit (pshim Type) QType polarity b ->
    PShimWit (pshim Type) QType polarity (a -> b)
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
    in mapPolarShimWit fshim
        $ typeToDolan
        $ MkDolanGroundedType (qGroundType :: _ (->))
        $ ConsCCRArguments (ContraCCRPolarArgument ta)
        $ ConsCCRArguments (CoCCRPolarArgument tb) NilCCRArguments

actionShimWit :: forall a. QShimWit 'Positive a -> QShimWit 'Positive (Action a)
actionShimWit swa =
    unPosShimWit swa $ \ta conva ->
        mapPosShimWit (cfmap conva)
            $ typeToDolan
            $ MkDolanGroundedType qGroundType
            $ ConsCCRArguments (CoCCRPolarArgument ta) NilCCRArguments
