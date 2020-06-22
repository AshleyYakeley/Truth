module Language.Expression.Dolan.Inverted
    ( invertedPolarSubtype
    ) where

import Data.Shim
import Language.Expression.Dolan.Solver
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

invertedContext ::
       forall (ground :: GroundTypeKind) wit. (IsDolanSubtypeGroundType ground)
    => SubtypeContext (DolanType ground) (DolanPolyShim ground Type) (Solver ground wit) 'Negative 'Positive
invertedContext = MkSubtypeContext {subtypeTypes = subtypeTT, subtypeInverted = invertedContext}

subtypeSS ::
       forall (ground :: GroundTypeKind) wit p q. (IsDolanSubtypeGroundType ground)
    => DolanSingularType ground 'Negative p
    -> DolanSingularType ground 'Positive q
    -> Solver ground wit (DolanPolyShim ground Type p q)
subtypeSS (VarDolanSingularType np) (VarDolanSingularType nq)
    | Just Refl <- testEquality np nq = pure id
subtypeSS (GroundDolanSingularType gp argsp) (GroundDolanSingularType gq argsq) =
    subtypeGroundTypes solverLiftM invertedContext gp argsp gq argsq
subtypeSS _ _ = empty

subtypeSP ::
       forall (ground :: GroundTypeKind) wit p q. (IsDolanSubtypeGroundType ground)
    => DolanSingularType ground 'Negative p
    -> DolanPlainType ground 'Positive q
    -> Solver ground wit (DolanPolyShim ground Type p q)
subtypeSP _ NilDolanPlainType = empty
subtypeSP sp (ConsDolanPlainType sq tq) =
    fmap (\conv -> join1 . conv) (subtypeSS sp sq) <|> fmap (\conv -> join2 . conv) (subtypeSP sp tq)

subtypePP ::
       forall (ground :: GroundTypeKind) wit p q. (IsDolanSubtypeGroundType ground)
    => DolanPlainType ground 'Negative p
    -> DolanPlainType ground 'Positive q
    -> Solver ground wit (DolanPolyShim ground Type p q)
subtypePP NilDolanPlainType _ = empty
subtypePP (ConsDolanPlainType sp tp) tq =
    fmap (\conv -> conv . meet1) (subtypeSP sp tq) <|> fmap (\conv -> conv . meet2) (subtypePP tp tq)

subtypeTT ::
       forall (ground :: GroundTypeKind) wit p q. (IsDolanSubtypeGroundType ground)
    => DolanType ground 'Negative p
    -> DolanType ground 'Positive q
    -> Solver ground wit (DolanPolyShim ground Type p q)
subtypeTT = solveRecursiveTypes subtypePP

invertedSubtype ::
       forall (ground :: GroundTypeKind) wit p q. (IsDolanSubtypeGroundType ground)
    => DolanPlainType ground 'Negative p
    -> DolanPlainType ground 'Positive q
    -> Solver ground wit (DolanPolyShim ground Type p q)
invertedSubtype tp tq = subtypePP tp tq <|> solverLiftM (throwTypeConvertInverseError tp tq)

invertedPolarSubtype ::
       forall (ground :: GroundTypeKind) polarity wit p q. (Is PolarityType polarity, IsDolanSubtypeGroundType ground)
    => DolanPlainType ground (InvertPolarity polarity) p
    -> DolanPlainType ground polarity q
    -> Solver ground wit (DolanPolarMap ground polarity p q)
invertedPolarSubtype tp tq =
    case polarityType @polarity of
        PositiveType -> fmap MkPolarMap $ invertedSubtype tp tq
        NegativeType -> fmap MkPolarMap $ invertedSubtype tq tp
