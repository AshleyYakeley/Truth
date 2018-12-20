module Language.Expression.Subsumer where

import Language.Expression.Sealed
import Language.Expression.Unifier
import Shapes

class Unifier subsumer => Subsumer subsumer where
    -- This should generate substitutions only for the inferred type, not the declared type.
    subsumePosWitnesses ::
           UnifierPosWitness subsumer inf
        -> UnifierPosWitness subsumer decl
        -> UnifierMonad subsumer (subsumer (inf -> decl))
    simplifyPosType :: AnyW (UnifierPosWitness subsumer) -> AnyW (UnifierPosWitness subsumer)

-- Note the user's declared type will be simplified first, so they'll end up seeing a simplified version of the type they declared for their expression.
subsumeExpression ::
       forall subsumer. Subsumer subsumer
    => AnyW (UnifierPosWitness subsumer)
    -> UnifierSealedExpression subsumer
    -> UnifierMonad subsumer (UnifierSealedExpression subsumer)
subsumeExpression rawdecltype (MkSealedExpression inftype expr) =
    case simplifyPosType @subsumer rawdecltype of
        MkAnyW decltype -> do
            uab <- subsumePosWitnesses @subsumer inftype decltype
            (conv, subs) <- solveUnifier uab
            return $ MkSealedExpression decltype $ fmap conv $ unifierExpressionSubstitute @subsumer subs expr
