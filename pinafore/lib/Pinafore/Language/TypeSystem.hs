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
import Pinafore.Language.Shim
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

instance Unifier PinaforeUnifier where
    type UnifierName PinaforeUnifier = Name
    type UnifierMonad PinaforeUnifier = PinaforeTypeCheck
    type UnifierNegWitness PinaforeUnifier = PinaforeType 'Negative
    type UnifierPosWitness PinaforeUnifier = PinaforeType 'Positive
    type UnifierSubstitutions PinaforeUnifier = [PinaforeBisubstitution]
    type UnifierShim PinaforeUnifier = PinaforeShim Type
    unifyNegWitnesses ta tb = return $ uuLiftNegShimWit $ joinMeetPinaforeShimWit (mkShimWit ta) (mkShimWit tb)
    unifyPosWitnesses ta tb = return $ uuLiftPosShimWit $ joinMeetPinaforeShimWit (mkShimWit ta) (mkShimWit tb)
    unifyPosNegWitnesses tq tp = fmap MkUUShim $ getCompose $ unifyPosNegPinaforeTypes tq tp
    solveUnifier = runUnifier
    unifierPosSubstitute = bisubstitutesType
    unifierNegSubstitute = bisubstitutesType
    simplify = return . pinaforeSimplifyTypes

instance TypeSystem PinaforeTypeSystem where
    type TSRenamer PinaforeTypeSystem = VarRenamerT PinaforeTypeSystem
    type TSUnifier PinaforeTypeSystem = PinaforeUnifier
    type TSScoped PinaforeTypeSystem = PinaforeSourceScoped
    type TSSubsumer PinaforeTypeSystem = PinaforeSubsumer
    tsFunctionPosWitness ta tb =
        singlePinaforeShimWit $
        mkShimWit $
        GroundPinaforeSingularType FuncPinaforeGroundType $
        ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
    tsFunctionNegWitness ta tb =
        singlePinaforeShimWit $
        mkShimWit $
        GroundPinaforeSingularType FuncPinaforeGroundType $
        ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
