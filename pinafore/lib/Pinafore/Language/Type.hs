{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Type
    ( module Pinafore.Language.Type.Type
    , module Pinafore.Language.EntityType
    , module Pinafore.Language.GroundType
    , module Data.Shim.Polarity
    , module Data.Shim.ShimWit
    , module Language.Expression.UVar
    , module Language.Expression.Dolan
    , module Pinafore.Language.Scope
    ) where

import Data.Shim.Polarity
import Data.Shim.ShimWit
import Language.Expression.Dolan
import Language.Expression.Renamer
import Language.Expression.TypeSystem
import Language.Expression.UVar
import Language.Expression.Unifier
import Pinafore.Language.EntityType
import Pinafore.Language.GroundType
import Pinafore.Language.Name
import Pinafore.Language.Scope
import Pinafore.Language.Type.Bisubstitute
import Pinafore.Language.Type.Rename ()
import Pinafore.Language.Type.Simplify
import Pinafore.Language.Type.Subsume
import Pinafore.Language.Type.Type
import Pinafore.Language.Type.Unify
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
