{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.TypeSystem
    ( module Pinafore.Language.TypeSystem.Type
    , module Pinafore.Language.Type.Entity
    , module Pinafore.Language.Type.Ground
    , module Data.Shim
    , module Language.Expression.UVar
    , module Language.Expression.Dolan
    , module Pinafore.Language.Scope
    ) where

import Data.Shim
import Language.Expression.Dolan
import Language.Expression.Renamer
import Language.Expression.TypeSystem
import Language.Expression.UVar
import Language.Expression.Unifier
import Pinafore.Language.Name
import Pinafore.Language.Scope
import Pinafore.Language.Type.Entity
import Pinafore.Language.Type.Ground
import Pinafore.Language.TypeSystem.Bisubstitute
import Pinafore.Language.TypeSystem.Rename ()
import Pinafore.Language.TypeSystem.Simplify
import Pinafore.Language.TypeSystem.Subsume
import Pinafore.Language.TypeSystem.Type
import Pinafore.Language.TypeSystem.Unify
import Shapes

instance Unifier (PinaforeUnifier baseedit) where
    type UnifierName (PinaforeUnifier baseedit) = Name
    type UnifierMonad (PinaforeUnifier baseedit) = PinaforeTypeCheck baseedit
    type UnifierNegWitness (PinaforeUnifier baseedit) = PinaforeType baseedit 'Negative
    type UnifierPosWitness (PinaforeUnifier baseedit) = PinaforeType baseedit 'Positive
    type UnifierSubstitutions (PinaforeUnifier baseedit) = [PinaforeBisubstitution baseedit]
    type UnifierShim (PinaforeUnifier baseedit) = PinaforeShim
    unifyNegWitnesses ta tb = return $ uuLiftNegShimWit $ meetPinaforeShimWit (mkShimWit ta) (mkShimWit tb)
    unifyPosWitnesses ta tb = return $ uuLiftPosShimWit $ joinPinaforeShimWit (mkShimWit ta) (mkShimWit tb)
    unifyPosNegWitnesses tq tp = fmap MkUUShim $ getCompose $ unifyPosNegPinaforeTypes tq tp
    solveUnifier = runUnifier
    unifierPosSubstitute = bisubstitutesType
    unifierNegSubstitute = bisubstitutesType
    simplify = return . pinaforeSimplifyTypes @baseedit

instance TypeSystem (PinaforeTypeSystem baseedit) where
    type TSRenamer (PinaforeTypeSystem baseedit) = VarRenamerT (PinaforeTypeSystem baseedit)
    type TSUnifier (PinaforeTypeSystem baseedit) = PinaforeUnifier baseedit
    type TSScoped (PinaforeTypeSystem baseedit) = PinaforeSourceScoped baseedit
    type TSSubsumer (PinaforeTypeSystem baseedit) = PinaforeSubsumer baseedit
    tsFunctionPosWitness ta tb =
        singlePinaforeShimWit $
        mkPJMShimWit $
        GroundPinaforeSingularType FuncPinaforeGroundType $
        ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
    tsFunctionNegWitness ta tb =
        singlePinaforeShimWit $
        mkPJMShimWit $
        GroundPinaforeSingularType FuncPinaforeGroundType $
        ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
