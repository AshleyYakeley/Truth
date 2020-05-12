{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.TypeSystem
    ( module Pinafore.Language.TypeSystem.Type
    , module Pinafore.Language.Type.Entity
    , module Pinafore.Language.Type.EntityAdapter
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
import Pinafore.Language.Type.EntityAdapter
import Pinafore.Language.Type.Ground
import Pinafore.Language.TypeSystem.Bisubstitute
import Pinafore.Language.TypeSystem.Rename ()
import Pinafore.Language.TypeSystem.Simplify
import Pinafore.Language.TypeSystem.Subsume
import Pinafore.Language.TypeSystem.Type
import Pinafore.Language.TypeSystem.Unify
import Shapes

instance Unifier (PinaforeUnifier baseupdate) where
    type UnifierName (PinaforeUnifier baseupdate) = Name
    type UnifierMonad (PinaforeUnifier baseupdate) = PinaforeTypeCheck baseupdate
    type UnifierNegWitness (PinaforeUnifier baseupdate) = PinaforeType baseupdate 'Negative
    type UnifierPosWitness (PinaforeUnifier baseupdate) = PinaforeType baseupdate 'Positive
    type UnifierSubstitutions (PinaforeUnifier baseupdate) = [PinaforeBisubstitution baseupdate]
    type UnifierShim (PinaforeUnifier baseupdate) = PinaforeShim
    unifyNegWitnesses ta tb = return $ uuLiftNegShimWit $ meetPinaforeShimWit (mkShimWit ta) (mkShimWit tb)
    unifyPosWitnesses ta tb = return $ uuLiftPosShimWit $ joinPinaforeShimWit (mkShimWit ta) (mkShimWit tb)
    unifyPosNegWitnesses tq tp = fmap MkUUShim $ getCompose $ unifyPosNegPinaforeTypes tq tp
    solveUnifier = runUnifier
    unifierPosSubstitute = bisubstitutesType
    unifierNegSubstitute = bisubstitutesType
    simplify = return . pinaforeSimplifyTypes @baseupdate

instance TypeSystem (PinaforeTypeSystem baseupdate) where
    type TSRenamer (PinaforeTypeSystem baseupdate) = VarRenamerT (PinaforeTypeSystem baseupdate)
    type TSUnifier (PinaforeTypeSystem baseupdate) = PinaforeUnifier baseupdate
    type TSScoped (PinaforeTypeSystem baseupdate) = PinaforeSourceScoped baseupdate
    type TSSubsumer (PinaforeTypeSystem baseupdate) = PinaforeSubsumer baseupdate
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
