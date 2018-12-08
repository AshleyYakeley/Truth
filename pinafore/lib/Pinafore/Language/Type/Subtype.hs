{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Type.Subtype
    ( SubtypeContext(..)
    , subtypeGroundTypes
    ) where

import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.GroundType
import Pinafore.Language.Scope
import Pinafore.Language.Show
import Pinafore.Language.SimpleEntityType
import Pinafore.Language.Type.Type
import Shapes

data SubtypeContext baseedit m pola polb = MkSubtypeContext
    { subtypeTypes :: forall ta tb. PinaforeType baseedit pola ta -> PinaforeType baseedit polb tb -> m (ta -> tb)
    , subtypeLift :: forall a. PinaforeSourceScoped baseedit a -> m a
    , subtypeInverted :: SubtypeContext baseedit m (InvertPolarity polb) (InvertPolarity pola)
    }

meetUnjoin1 ::
       forall polarity a. IsTypePolarity polarity
    => JoinMeetType polarity a (LimitType polarity)
    -> a
meetUnjoin1 =
    case whichTypePolarity @polarity of
        Left Refl -> unjoin1
        Right Refl -> meet1

subtypeVariance ::
       (Applicative m, IsTypePolarity pola, IsTypePolarity polb)
    => SubtypeContext baseedit m pola polb
    -> SingleVarianceType sv
    -> SingleArgument sv (PinaforeType baseedit) pola a
    -> SingleArgument sv (PinaforeType baseedit) polb b
    -> m (SingleVarianceFunc sv a b)
subtypeVariance sc CovarianceType ta tb = subtypeTypes sc ta tb
subtypeVariance sc ContravarianceType ta tb = do
    ba <- subtypeTypes (subtypeInverted sc) tb ta
    return $ MkCatDual ba
subtypeVariance sc RangevarianceType (MkRangeType tpa tqa) (MkRangeType tpb tqb) = do
    pba <- subtypeTypes (subtypeInverted sc) tpb tpa
    qab <- subtypeTypes sc tqa tqb
    return $ MkWithRange pba qab

subtypeArguments' ::
       forall baseedit m pola polb dv gta gtb ta tb. (Applicative m, IsTypePolarity pola, IsTypePolarity polb)
    => SubtypeContext baseedit m pola polb
    -> DolanVarianceType dv
    -> DolanKindVary dv gta
    -> DolanArguments dv (PinaforeType baseedit) gta pola ta
    -> DolanArguments dv (PinaforeType baseedit) gtb polb tb
    -> m (KindFunction (DolanVarianceKind dv) gta gtb -> ta -> tb)
subtypeArguments' _ NilListType NilDolanKindVary NilDolanArguments NilDolanArguments = pure id
subtypeArguments' sc (ConsListType (svt :: SingleVarianceType sv) (dvt :: DolanVarianceType dv')) (ConsDolanKindVary svm dvm) (ConsDolanArguments sta dta) (ConsDolanArguments stb dtb) = do
    sfunc <- subtypeVariance sc svt sta stb
    f <- subtypeArguments' sc dvt dvm dta dtb
    pure $
        case dolanVarianceKMCategory @(->) dvt of
            Dict -> \(MkNestedMorphism conv) -> f (conv . svm sfunc)

subtypeArguments ::
       forall baseedit m pola polb pol dv gt argsa argsb. (Applicative m, IsTypePolarity pola, IsTypePolarity polb)
    => SubtypeContext baseedit m pola polb
    -> PinaforeGroundType baseedit pol dv gt
    -> DolanArguments dv (PinaforeType baseedit) gt pola argsa
    -> DolanArguments dv (PinaforeType baseedit) gt polb argsb
    -> m (argsa -> argsb)
subtypeArguments sc gt argsa argsb = let
    vkt = pinaforeGroundTypeKind gt
    in case dolanVarianceKMCategory @(->) vkt of
           Dict -> fmap (\f -> f id) $ subtypeArguments' sc vkt (pinaforeGroundTypeVary gt) argsa argsb

subtypeGroundTypes ::
       forall baseedit m pola polb dva gta a dvb gtb b. (Applicative m, IsTypePolarity pola, IsTypePolarity polb)
    => SubtypeContext baseedit m pola polb
    -> PinaforeGroundType baseedit pola dva gta
    -> DolanArguments dva (PinaforeType baseedit) gta pola a
    -> PinaforeGroundType baseedit polb dvb gtb
    -> DolanArguments dvb (PinaforeType baseedit) gtb polb b
    -> m (a -> b)
subtypeGroundTypes _ ActionPinaforeGroundType NilDolanArguments ActionPinaforeGroundType NilDolanArguments = pure id
subtypeGroundTypes sc OrderPinaforeGroundType argsa OrderPinaforeGroundType argsb =
    subtypeArguments sc OrderPinaforeGroundType argsa argsb
subtypeGroundTypes _ UserInterfacePinaforeGroundType NilDolanArguments UserInterfacePinaforeGroundType NilDolanArguments =
    pure id
subtypeGroundTypes sc (SimpleEntityPinaforeGroundType t1) NilDolanArguments (SimpleEntityPinaforeGroundType t2) NilDolanArguments =
    subtypeLift sc $ getSubtype t1 t2
subtypeGroundTypes sc PairPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) (SimpleEntityPinaforeGroundType TopSimpleEntityType) NilDolanArguments =
    (\conva convb (a, b) -> pairToEntity (meetUnjoin1 @polb $ conva a, meetUnjoin1 @polb $ convb b)) <$>
    subtypeTypes
        sc
        ta
        (singlePinaforeType $
         GroundPinaforeSingularType (SimpleEntityPinaforeGroundType TopSimpleEntityType) NilDolanArguments) <*>
    subtypeTypes
        sc
        tb
        (singlePinaforeType $
         GroundPinaforeSingularType (SimpleEntityPinaforeGroundType TopSimpleEntityType) NilDolanArguments)
subtypeGroundTypes sc EitherPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) (SimpleEntityPinaforeGroundType TopSimpleEntityType) NilDolanArguments =
    (\conva convb eab ->
         eitherToEntity $ either (Left . meetUnjoin1 @polb . conva) (Right . meetUnjoin1 @polb . convb) eab) <$>
    subtypeTypes
        sc
        ta
        (singlePinaforeType $
         GroundPinaforeSingularType (SimpleEntityPinaforeGroundType TopSimpleEntityType) NilDolanArguments) <*>
    subtypeTypes
        sc
        tb
        (singlePinaforeType $
         GroundPinaforeSingularType (SimpleEntityPinaforeGroundType TopSimpleEntityType) NilDolanArguments)
subtypeGroundTypes sc FuncPinaforeGroundType argsa FuncPinaforeGroundType argsb =
    subtypeArguments sc FuncPinaforeGroundType argsa argsb
subtypeGroundTypes sc ListPinaforeGroundType argsa ListPinaforeGroundType argsb =
    subtypeArguments sc ListPinaforeGroundType argsa argsb
subtypeGroundTypes sc PairPinaforeGroundType argsa PairPinaforeGroundType argsb =
    subtypeArguments sc PairPinaforeGroundType argsa argsb
subtypeGroundTypes sc EitherPinaforeGroundType argsa EitherPinaforeGroundType argsb =
    subtypeArguments sc EitherPinaforeGroundType argsa argsb
subtypeGroundTypes sc ReferencePinaforeGroundType argsa ReferencePinaforeGroundType argsb =
    subtypeArguments sc ReferencePinaforeGroundType argsa argsb
subtypeGroundTypes sc SetPinaforeGroundType argsa SetPinaforeGroundType argsb =
    subtypeArguments sc SetPinaforeGroundType argsa argsb
subtypeGroundTypes sc MorphismPinaforeGroundType argsa MorphismPinaforeGroundType argsb =
    subtypeArguments sc MorphismPinaforeGroundType argsa argsb
subtypeGroundTypes sc ga argsa gb argsb =
    subtypeLift sc $
    convertFailure
        (unpack $ exprShow $ GroundPinaforeSingularType ga argsa)
        (unpack $ exprShow $ GroundPinaforeSingularType gb argsb)
