{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Type
    ( module Pinafore.Language.Type.Type
    , module Pinafore.Language.GroundType
    , module Pinafore.Language.SimpleEntityType
    , module Language.Expression.UVar
    , module Language.Expression.Dolan
    , module Pinafore.Language.TypeContext
    ) where

import Language.Expression.Dolan
import Language.Expression.Renamer
import Language.Expression.Typed
import Language.Expression.UVar
import Language.Expression.Unifier
import Pinafore.Language.GroundType
import Pinafore.Language.SimpleEntityType
import Pinafore.Language.Type.Simplify
import Pinafore.Language.Type.Type
import Pinafore.Language.Type.Unify
import Pinafore.Language.TypeContext
import Shapes

instance Unifier (PinaforeUnifier baseedit) where
    type UnifierMonad (PinaforeUnifier baseedit) = VarRenamer (PinaforeTypeSystem baseedit) SourcePinaforeTypeCheck
    type UnifierNegWitness (PinaforeUnifier baseedit) = PinaforeType baseedit 'NegativePolarity
    type UnifierPosWitness (PinaforeUnifier baseedit) = PinaforeType baseedit 'PositivePolarity
    type UnifierSubstitutions (PinaforeUnifier baseedit) = [PinaforeBisubstitution baseedit]
    unifyNegWitnesses ta tb cont = meetPinaforeTypes ta tb $ \tab conva convb -> cont tab $ pure (conva, convb)
    unifyPosWitnesses ta tb cont = joinPinaforeTypes ta tb $ \tab conva convb -> cont tab $ pure (conva, convb)
    unifyPosNegWitnesses tq tp = getCompose $ unifyPosNegPinaforeTypes tq tp
    solveUnifier = runUnifier
    unifierPosSubstitute subs t = unTypeF $ bisubstituteAllPositiveType subs t
    unifierNegSubstitute subs t = unTypeF $ bisubstituteAllNegativeType subs t
    simplifyExpressionType = pinaforeSimplifyExpressionType

instance TypeSystem (PinaforeTypeSystem baseedit) where
    type TypeRenamer (PinaforeTypeSystem baseedit) = VarRenamer (PinaforeTypeSystem baseedit)
    type TypeUnifier (PinaforeTypeSystem baseedit) = PinaforeUnifier baseedit
    type NegWitness (PinaforeTypeSystem baseedit) = PinaforeType baseedit 'NegativePolarity
    type PosWitness (PinaforeTypeSystem baseedit) = PinaforeType baseedit 'PositivePolarity
    type TypeCheck (PinaforeTypeSystem baseedit) = SourcePinaforeTypeCheck
    typeSystemFunctionPosWitness ta tb =
        unTypeF $
        singlePinaforeTypeF $
        mkTypeF $
        GroundPinaforeSingularType FuncPinaforeGroundType $
        ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
    typeSystemFunctionNegWitness ta tb =
        unTypeF $
        singlePinaforeTypeF $
        mkTypeF $
        GroundPinaforeSingularType FuncPinaforeGroundType $
        ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
