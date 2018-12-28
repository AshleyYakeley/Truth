module Language.Expression.Subsumer where

import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Sealed

--import Language.Expression.Unifier
import Shapes

class (Monad (SubsumerMonad subsumer), Applicative subsumer) => Subsumer subsumer where
    type SubsumerMonad subsumer :: Type -> Type
    type SubsumerNegWitness subsumer :: Type -> Type
    type SubsumerPosWitness subsumer :: Type -> Type
    type SubsumerSubstitutions subsumer :: Type
    solveSubsumer :: subsumer a -> SubsumerMonad subsumer (a, SubsumerSubstitutions subsumer)
    -- This should generate substitutions only for the inferred type, not the declared type.
    subsumerNegSubstitute ::
           SubsumerSubstitutions subsumer
        -> SubsumerNegWitness subsumer t
        -> (forall t'. SubsumerNegWitness subsumer t' -> (t' -> t) -> SubsumerMonad subsumer r)
        -> SubsumerMonad subsumer r
    subsumePosWitnesses ::
           SubsumerPosWitness subsumer inf
        -> SubsumerPosWitness subsumer decl
        -> SubsumerMonad subsumer (subsumer (inf -> decl))
    simplifyPosType :: AnyW (SubsumerPosWitness subsumer) -> AnyW (SubsumerPosWitness subsumer)

type SubsumerOpenExpression name subsumer = NamedExpression name (SubsumerNegWitness subsumer)

type SubsumerSealedExpression name subsumer
     = SealedExpression name (SubsumerNegWitness subsumer) (SubsumerPosWitness subsumer)

liftSubsumer :: Monad (SubsumerMonad subsumer) => subsumer a -> Compose (SubsumerMonad subsumer) subsumer a
liftSubsumer ua = Compose $ return ua

subsumerExpressionSubstitute ::
       forall subsumer name a. Subsumer subsumer
    => SubsumerSubstitutions subsumer
    -> SubsumerOpenExpression name subsumer a
    -> SubsumerMonad subsumer (SubsumerOpenExpression name subsumer a)
subsumerExpressionSubstitute _ (ClosedExpression a) = return $ ClosedExpression a
subsumerExpressionSubstitute subs (OpenExpression (MkNameWitness name tw) expr) =
    subsumerNegSubstitute @subsumer subs tw $ \tw' conv -> do
        expr' <- subsumerExpressionSubstitute @subsumer subs expr
        return $ OpenExpression (MkNameWitness name tw') $ fmap (\ta -> ta . conv) expr'

-- Note the user's declared type will be simplified first, so they'll end up seeing a simplified version of the type they declared for their expression.
subsumeExpression ::
       forall subsumer name. Subsumer subsumer
    => AnyW (SubsumerPosWitness subsumer)
    -> SubsumerSealedExpression name subsumer
    -> SubsumerMonad subsumer (SubsumerSealedExpression name subsumer)
subsumeExpression rawdecltype (MkSealedExpression inftype expr) =
    case simplifyPosType @subsumer rawdecltype of
        MkAnyW decltype -> do
            uab <- subsumePosWitnesses @subsumer inftype decltype
            (conv, subs) <- solveSubsumer uab
            expr' <- subsumerExpressionSubstitute @subsumer subs expr
            return $ MkSealedExpression decltype $ fmap conv expr'
