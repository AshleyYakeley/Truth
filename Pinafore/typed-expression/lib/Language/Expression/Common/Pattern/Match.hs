{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Pattern.Match where

import Language.Expression.Common.Named
import Language.Expression.Common.Pattern.Sealed
import Language.Expression.Common.WitnessMappable
import Shapes

type Match (name :: Type) (vw :: Type -> Type) (tw :: Type -> Type) = SealedPattern name vw (NamedExpression name tw)

instance WitnessMappable poswit negwit (Match name poswit negwit) where
    mapWitnessesM mapPos mapNeg (MkSealedPattern tt pat) = do
        tt' <- mapWitnessesM mapPos mapNeg tt
        pat' <- mapWitnessesM mapPos mapNeg pat
        pure $ MkSealedPattern tt' $ pat'
