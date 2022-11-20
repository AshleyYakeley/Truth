{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Common.Pattern.Named where

import Language.Expression.Common.NameWit
import Language.Expression.Common.Named
import Language.Expression.Common.Pattern.Pattern
import Language.Expression.Common.Witness
import Language.Expression.Common.WitnessMappable
import Shapes

type NameTypePattern :: forall kn. (kn -> Type) -> (kn -> Type -> Type) -> Type -> Type -> Type
type NameTypePattern nw vw = Pattern (NameTypeWitness nw vw)

varNameTypePattern :: nw n -> vw n t -> NameTypePattern nw vw t ()
varNameTypePattern n t = varPattern $ MkNameTypeWitness n t

type NamedPattern :: Type -> (Type -> Type) -> Type -> Type -> Type
type NamedPattern name w = NameTypePattern (UnitType name) (UnitType' w)

instance WitnessMappable poswit negwit (NamedPattern name poswit a b) where
    mapWitnessesM mapPos _ (MkPattern ww pf) = do
        ww' <-
            for ww $ \(MkSomeFor (MkNameWitness name tt) conv) -> do
                tt' <- mapPos tt
                return $ MkSomeFor (MkNameWitness name tt') conv
        return $ MkPattern ww' pf

patternNames :: NamedPattern name vw q a -> [name]
patternNames = patternFreeWitnesses $ \(MkNameWitness name _) -> name

substitutePattern ::
       forall vw1 vw2 name q a.
       WitnessSubstitution Type vw1 vw2
    -> NamedPattern name vw1 q a
    -> NamedPattern name vw2 q a
substitutePattern witmap (MkPattern ww pf) = let
    susbstSomeFor :: SomeFor ((->) t) (NameWitness name vw1) -> SomeFor ((->) t) (NameWitness name vw2)
    susbstSomeFor (MkSomeFor (MkNameWitness name t1) conv) =
        unWitnessConvert witmap t1 $ \t2 bij -> MkSomeFor (MkNameWitness name t2) $ isoForwards bij . conv
    in MkPattern (fmap susbstSomeFor ww) pf

varNamedPattern :: name -> vw t -> NamedPattern name vw t ()
varNamedPattern n t = varNameTypePattern (MkUnitType n) (MkUnitType' t)
