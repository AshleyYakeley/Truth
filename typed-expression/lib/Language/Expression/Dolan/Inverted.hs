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
subtypeSS sta@(RecursiveDolanSingularType _ _) stb = solveRecursiveSingularTypes invertedSubtype sta stb
subtypeSS sta stb@(RecursiveDolanSingularType _ _) = solveRecursiveSingularTypes invertedSubtype sta stb
subtypeSS _ _ = empty

subtypeST ::
       forall (ground :: GroundTypeKind) wit p q. (IsDolanSubtypeGroundType ground)
    => DolanSingularType ground 'Negative p
    -> DolanType ground 'Positive q
    -> Solver ground wit (DolanPolyShim ground Type p q)
subtypeST _ NilDolanType = empty
subtypeST sp (ConsDolanType sq tq) =
    fmap (\conv -> join1 . conv) (subtypeSS sp sq) <|> fmap (\conv -> join2 . conv) (subtypeST sp tq)

subtypeTT ::
       forall (ground :: GroundTypeKind) wit p q. (IsDolanSubtypeGroundType ground)
    => DolanType ground 'Negative p
    -> DolanType ground 'Positive q
    -> Solver ground wit (DolanPolyShim ground Type p q)
subtypeTT NilDolanType _ = empty
subtypeTT (ConsDolanType sp tp) tq =
    fmap (\conv -> conv . meet1) (subtypeST sp tq) <|> fmap (\conv -> conv . meet2) (subtypeTT tp tq)

invertedSubtype ::
       forall (ground :: GroundTypeKind) wit p q. (IsDolanSubtypeGroundType ground)
    => DolanType ground 'Negative p
    -> DolanType ground 'Positive q
    -> Solver ground wit (DolanPolyShim ground Type p q)
invertedSubtype tp tq = subtypeTT tp tq <|> solverLiftM (throwTypeConvertInverseError tp tq)

invertedPolarSubtype ::
       forall (ground :: GroundTypeKind) polarity wit p q. (Is PolarityType polarity, IsDolanSubtypeGroundType ground)
    => DolanType ground (InvertPolarity polarity) p
    -> DolanType ground polarity q
    -> Solver ground wit (DolanPolarMap ground polarity p q)
invertedPolarSubtype tp tq =
    case polarityType @polarity of
        PositiveType -> fmap MkPolarMap $ invertedSubtype tp tq
        NegativeType -> fmap MkPolarMap $ invertedSubtype tq tp
