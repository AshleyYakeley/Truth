module Language.Expression.Dolan.Inverted
    ( invertedPolarSubtype
    ) where

import Data.Shim
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

invertedContext ::
       forall (ground :: GroundTypeKind) m. (IsDolanSubtypeGroundType ground, MonadPlus m)
    => MFunction (DolanM ground) m
    -> SubtypeContext (DolanType ground) (DolanPolyShim ground Type) m 'Negative 'Positive
invertedContext dlift = MkSubtypeContext {subtypeTypes = subtypeTT dlift, subtypeInverted = invertedContext dlift}

subtypeSS ::
       forall (ground :: GroundTypeKind) m p q. (IsDolanSubtypeGroundType ground, MonadPlus m)
    => MFunction (DolanM ground) m
    -> DolanSingularType ground 'Negative p
    -> DolanSingularType ground 'Positive q
    -> m (DolanPolyShim ground Type p q)
subtypeSS _ (VarDolanSingularType np) (VarDolanSingularType nq)
    | Just Refl <- testEquality np nq = return id
subtypeSS dlift (GroundDolanSingularType gp argsp) (GroundDolanSingularType gq argsq) =
    subtypeGroundTypes dlift (invertedContext dlift) gp argsp gq argsq
subtypeSS _ _ _ = empty

subtypeSP ::
       forall (ground :: GroundTypeKind) m p q. (IsDolanSubtypeGroundType ground, MonadPlus m)
    => MFunction (DolanM ground) m
    -> DolanSingularType ground 'Negative p
    -> DolanPlainType ground 'Positive q
    -> m (DolanPolyShim ground Type p q)
subtypeSP _ _ NilDolanPlainType = empty
subtypeSP dlift sp (ConsDolanPlainType sq tq) =
    fmap (\conv -> join1 . conv) (subtypeSS dlift sp sq) <|> fmap (\conv -> join2 . conv) (subtypeSP dlift sp tq)

subtypePP ::
       forall (ground :: GroundTypeKind) m p q. (IsDolanSubtypeGroundType ground, MonadPlus m)
    => MFunction (DolanM ground) m
    -> DolanPlainType ground 'Negative p
    -> DolanPlainType ground 'Positive q
    -> m (DolanPolyShim ground Type p q)
subtypePP _ NilDolanPlainType _ = empty
subtypePP dlift (ConsDolanPlainType sp tp) tq =
    fmap (\conv -> conv . meet1) (subtypeSP dlift sp tq) <|> fmap (\conv -> conv . meet2) (subtypePP dlift tp tq)

subtypeTT ::
       forall (ground :: GroundTypeKind) m p q. (IsDolanSubtypeGroundType ground, MonadPlus m)
    => MFunction (DolanM ground) m
    -> DolanType ground 'Negative p
    -> DolanType ground 'Positive q
    -> m (DolanPolyShim ground Type p q)
subtypeTT dlift (PlainDolanType ta) (PlainDolanType tb) = subtypePP dlift ta tb
subtypeTT dlift (RecursiveDolanType n t) _ = dlift $ throwTypeRecursiveError n t
subtypeTT dlift _ (RecursiveDolanType n t) = dlift $ throwTypeRecursiveError n t

invertedSubtype ::
       forall (ground :: GroundTypeKind) m p q. (IsDolanSubtypeGroundType ground, MonadPlus m)
    => MFunction (DolanM ground) m
    -> DolanType ground 'Negative p
    -> DolanType ground 'Positive q
    -> m (DolanPolyShim ground Type p q)
invertedSubtype dlift tp tq = subtypeTT dlift tp tq <|> dlift (throwTypeConvertInverseError tp tq)

invertedPolarSubtype ::
       forall (ground :: GroundTypeKind) polarity m p q.
       (Is PolarityType polarity, IsDolanSubtypeGroundType ground, MonadPlus m)
    => MFunction (DolanM ground) m
    -> DolanType ground (InvertPolarity polarity) p
    -> DolanType ground polarity q
    -> m (DolanPolarMap ground polarity p q)
invertedPolarSubtype dlift tp tq =
    case polarityType @polarity of
        PositiveType -> do
            conv <- invertedSubtype dlift tp tq
            return $ MkPolarMap conv
        NegativeType -> do
            conv <- invertedSubtype dlift tq tp
            return $ MkPolarMap conv
