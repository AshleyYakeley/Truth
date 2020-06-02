module Language.Expression.Dolan.Inverted
    ( invertedSubtype
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

subtypeST ::
       forall (ground :: GroundTypeKind) m p q. (IsDolanSubtypeGroundType ground, MonadPlus m)
    => MFunction (DolanM ground) m
    -> DolanSingularType ground 'Negative p
    -> DolanType ground 'Positive q
    -> m (DolanPolyShim ground Type p q)
subtypeST _ _ NilDolanType = empty
subtypeST dlift sp (ConsDolanType sq tq) =
    fmap (\conv -> join1 . conv) (subtypeSS dlift sp sq) <|> fmap (\conv -> join2 . conv) (subtypeST dlift sp tq)

subtypeTT ::
       forall (ground :: GroundTypeKind) m p q. (IsDolanSubtypeGroundType ground, MonadPlus m)
    => MFunction (DolanM ground) m
    -> DolanType ground 'Negative p
    -> DolanType ground 'Positive q
    -> m (DolanPolyShim ground Type p q)
subtypeTT _ NilDolanType _ = empty
subtypeTT dlift (ConsDolanType sp tp) tq =
    fmap (\conv -> conv . meet1) (subtypeST dlift sp tq) <|> fmap (\conv -> conv . meet2) (subtypeTT dlift tp tq)

invertedSubtype ::
       forall (ground :: GroundTypeKind) m p q. (IsDolanSubtypeGroundType ground, MonadPlus m)
    => MFunction (DolanM ground) m
    -> DolanType ground 'Negative p
    -> DolanType ground 'Positive q
    -> m (DolanPolyShim ground Type p q)
invertedSubtype dlift tp tq = subtypeTT dlift tp tq <|> dlift (throwTypeConvertInverseError tp tq)
