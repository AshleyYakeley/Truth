{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Pattern.Match where

import Language.Expression.Common.Named
import Language.Expression.Common.Pattern.Pattern
import Language.Expression.Common.Pattern.Sealed
import Language.Expression.Common.WitnessTraversable
import Shapes

type Match (name :: Type) (patwit :: Type -> Type) (negwit :: Type -> Type)
     = SealedPattern patwit (NamedExpression name negwit)

instance IsPatternWitness poswit patwit => WitnessTraversable poswit negwit (Match name patwit negwit) where
    traverseWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkSealedPattern tt pat) -> do
            tt' <- unEndoM (traverseWitnessesM mapPos mapNeg) tt
            pat' <- unEndoM (traverseWitnessesM mapPos mapNeg) pat
            pure $ MkSealedPattern tt' $ pat'
