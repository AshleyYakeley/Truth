{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Common.Pattern.Named where

import Language.Expression.Common.NameWit
import Language.Expression.Common.Named
import Language.Expression.Common.Pattern.Pattern
import Language.Expression.Common.Witness
import Language.Expression.Common.WitnessMappable
import Shapes

type NameTypePattern nw vw = Pattern (NameTypeWitness nw vw)

varNameTypePattern :: nw n -> vw n t -> NameTypePattern nw vw t ()
varNameTypePattern n t = varPattern $ MkNameTypeWitness n t

type NamedPattern name w = NameTypePattern (UnitType name) (UnitType' w)

instance WitnessMappable poswit negwit (NamedPattern name poswit a b) where
    mapWitnessesM _ _ (ClosedPattern a) = pure $ ClosedPattern a
    mapWitnessesM mapPos mapNeg (OpenPattern (MkNameWitness name tt) pat) = do
        tt' <- mapPos tt
        pat' <- mapWitnessesM mapPos mapNeg pat
        pure $ OpenPattern (MkNameWitness name tt') pat'

patternNames :: NamedPattern name vw q a -> [name]
patternNames = patternFreeWitnesses $ \(MkNameTypeWitness (MkUnitType name) _) -> name

substitutePattern :: WitnessSubstitution Type vw1 vw2 -> NamedPattern name vw1 q a -> NamedPattern name vw2 q a
substitutePattern _ (ClosedPattern a) = ClosedPattern a
substitutePattern witmap@(MkWitnessMap wm) (OpenPattern (MkNameWitness name wt) pat) =
    wm wt $ \wt' bij ->
        OpenPattern (MkNameWitness name wt') $ fmap (\(t, a) -> (isoForwards bij t, a)) $ substitutePattern witmap pat

varNamedPattern :: name -> vw t -> NamedPattern name vw t ()
varNamedPattern n t = varNameTypePattern (MkUnitType n) (MkUnitType' t)
