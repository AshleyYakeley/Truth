module Language.Expression.Common.Subsumer
    ( Subsumer(..)
    , SimplifySubsumer(..)
    , SubsumerOpenExpression
    , SubsumerSealedExpression
    , subsumeExpression
    ) where

import Data.Shim
import Language.Expression.Common.Expression
import Language.Expression.Common.Named
import Language.Expression.Common.Sealed
import Shapes

class ( Monad (SubsumerMonad subsumer)
      , Applicative subsumer
      , Category (SubsumerShim subsumer)
      , InCategory (SubsumerShim subsumer)
      ) => Subsumer subsumer where
    type SubsumerMonad subsumer :: Type -> Type
    type SubsumerNegWitness subsumer :: Type -> Type
    type SubsumerPosWitness subsumer :: Type -> Type
    type SubsumerSubstitutions subsumer :: Type
    type SubsumerShim subsumer :: Type -> Type -> Type
    solveSubsumer :: subsumer a -> SubsumerMonad subsumer (a, SubsumerSubstitutions subsumer)
    -- This should generate substitutions only for the inferred type, not the declared type.
    subsumerNegSubstitute ::
           SubsumerSubstitutions subsumer
        -> SubsumerNegWitness subsumer t
        -> SubsumerMonad subsumer (SubsumerNegShimWit subsumer t)
    subsumePosWitnesses ::
           SubsumerPosWitness subsumer inf
        -> SubsumerPosWitness subsumer decl
        -> SubsumerMonad subsumer (subsumer (SubsumerShim subsumer inf decl))

class Subsumer subsumer => SimplifySubsumer subsumer where
    simplifyPosType :: SubsumerPosWitness subsumer t -> SubsumerMonad subsumer (SubsumerPosShimWit subsumer t)

type SubsumerNegShimWit subsumer = ShimWit (SubsumerShim subsumer) (SubsumerNegWitness subsumer) 'Negative

type SubsumerPosShimWit subsumer = ShimWit (SubsumerShim subsumer) (SubsumerPosWitness subsumer) 'Positive

type SubsumerOpenExpression name subsumer = NamedExpression name (SubsumerNegShimWit subsumer)

type SubsumerSealedExpression name subsumer
     = SealedExpression name (SubsumerNegShimWit subsumer) (SubsumerPosShimWit subsumer)

subsumerExpressionSubstitute ::
       forall subsumer name a. Subsumer subsumer
    => SubsumerSubstitutions subsumer
    -> SubsumerOpenExpression name subsumer a
    -> SubsumerMonad subsumer (SubsumerOpenExpression name subsumer a)
subsumerExpressionSubstitute _ (ClosedExpression a) = return $ ClosedExpression a
subsumerExpressionSubstitute subs (OpenExpression (MkNameWitness name tw) expr) = do
    tw' <- chainShimWitM (subsumerNegSubstitute @subsumer subs) tw
    expr' <- subsumerExpressionSubstitute @subsumer subs expr
    return $ OpenExpression (MkNameWitness name tw') expr'

-- Note the user's declared type will be simplified first, so they'll end up seeing a simplified version of the type they declared for their expression.
subsumeExpression ::
       forall subsumer name. SimplifySubsumer subsumer
    => AnyW (SubsumerPosWitness subsumer)
    -> SubsumerSealedExpression name subsumer
    -> SubsumerMonad subsumer (SubsumerSealedExpression name subsumer)
subsumeExpression (MkAnyW rawdecltype) (MkSealedExpression (MkShimWit inftype infconv) expr) = do
    MkShimWit decltype _ <- simplifyPosType @subsumer rawdecltype
    uab <- subsumePosWitnesses @subsumer inftype decltype
    (conv, subs) <- solveSubsumer uab
    expr' <- subsumerExpressionSubstitute @subsumer subs expr
    return $ MkSealedExpression (MkShimWit decltype $ MkPolarMap conv . infconv) expr'
