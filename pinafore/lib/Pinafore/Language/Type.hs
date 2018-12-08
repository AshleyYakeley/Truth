{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Type
    ( module Pinafore.Language.Type.Type
    , module Pinafore.Language.GroundType
    , module Pinafore.Language.SimpleEntityType
    , module Language.Expression.UVar
    , module Language.Expression.Dolan
    , module Pinafore.Language.Scope
    ) where

import Language.Expression.Dolan
import Language.Expression.Renamer
import Language.Expression.Typed
import Language.Expression.UVar
import Language.Expression.Unifier
import Pinafore.Language.GroundType
import Pinafore.Language.Name
import Pinafore.Language.Scope
import Pinafore.Language.SimpleEntityType
import Pinafore.Language.Type.Simplify
import Pinafore.Language.Type.Type
import Pinafore.Language.Type.Unify
import Shapes

instance Unifier (PinaforeUnifier baseedit) where
    type UnifierName (PinaforeUnifier baseedit) = Name
    type UnifierMonad (PinaforeUnifier baseedit) = PinaforeUnifierMonad baseedit
    type UnifierTSNegWitness (PinaforeUnifier baseedit) = PinaforeType baseedit 'NegativePolarity
    type UnifierTSPosWitness (PinaforeUnifier baseedit) = PinaforeType baseedit 'PositivePolarity
    type UnifierSubstitutions (PinaforeUnifier baseedit) = [PinaforeBisubstitution baseedit]
    unifyTSNegWitnesses ta tb cont = meetPinaforeTypes ta tb $ \tab conva convb -> cont tab $ pure (conva, convb)
    unifyTSPosWitnesses ta tb cont = joinPinaforeTypes ta tb $ \tab conva convb -> cont tab $ pure (conva, convb)
    unifyPosTSNegWitnesses tq tp = getCompose $ unifyPosNegPinaforeTypes tq tp
    solveUnifier = runUnifier
    unifierPosSubstitute subs t = unTypeF $ bisubstituteAllPositiveType subs t
    unifierNegSubstitute subs t = unTypeF $ bisubstituteAllNegativeType subs t
    simplifyExpressionType = pinaforeSimplifyExpressionType

instance TypeSystem (PinaforeTypeSystem baseedit) where
    type TSRenamer (PinaforeTypeSystem baseedit) = VarRenamer (PinaforeTypeSystem baseedit)
    type TSUnifier (PinaforeTypeSystem baseedit) = PinaforeUnifier baseedit
    type TSNegWitness (PinaforeTypeSystem baseedit) = PinaforeType baseedit 'NegativePolarity
    type TSPosWitness (PinaforeTypeSystem baseedit) = PinaforeType baseedit 'PositivePolarity
    type TSScoped (PinaforeTypeSystem baseedit) = PinaforeSourceScoped baseedit
    typeSystemFunctionTSPosWitness ta tb =
        unTypeF $
        singlePinaforeTypeF $
        mkTypeF $
        GroundPinaforeSingularType FuncPinaforeGroundType $
        ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
    typeSystemFunctionTSNegWitness ta tb =
        unTypeF $
        singlePinaforeTypeF $
        mkTypeF $
        GroundPinaforeSingularType FuncPinaforeGroundType $
        ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
