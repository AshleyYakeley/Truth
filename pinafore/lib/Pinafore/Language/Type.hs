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
import Language.Expression.TypeSystem
import Language.Expression.UVar
import Language.Expression.Unifier
import Pinafore.Language.GroundType
import Pinafore.Language.Name
import Pinafore.Language.Scope
import Pinafore.Language.SimpleEntityType
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
    type UnifierNegWitness (PinaforeUnifier baseedit) = PinaforeType baseedit 'NegativePolarity
    type UnifierPosWitness (PinaforeUnifier baseedit) = PinaforeType baseedit 'PositivePolarity
    type UnifierSubstitutions (PinaforeUnifier baseedit) = [PinaforeBisubstitution baseedit]
    unifyNegWitnesses ta tb cont = meetPinaforeTypes ta tb $ \tab conva convb -> cont tab $ pure (conva, convb)
    unifyPosWitnesses ta tb cont = joinPinaforeTypes ta tb $ \tab conva convb -> cont tab $ pure (conva, convb)
    unifyPosNegWitnesses tq tp = getCompose $ unifyPosNegPinaforeTypes tq tp
    solveUnifier = runUnifier
    unifierPosSubstitute subs t cont = do
        t' <- bisubstitutesType subs t
        unTypeF t' cont
    unifierNegSubstitute subs t cont = do
        t' <- bisubstitutesType subs t
        unTypeF t' cont
    simplifyExpressionType = return . pinaforeSimplifyTypes
    simplifyPatternType = return . pinaforeSimplifyTypes

instance TypeSystem (PinaforeTypeSystem baseedit) where
    type TSRenamer (PinaforeTypeSystem baseedit) = VarRenamer (PinaforeTypeSystem baseedit)
    type TSUnifier (PinaforeTypeSystem baseedit) = PinaforeUnifier baseedit
    type TSScoped (PinaforeTypeSystem baseedit) = PinaforeSourceScoped baseedit
    type TSSubsumer (PinaforeTypeSystem baseedit) = PinaforeSubsumer baseedit
    tsFunctionPosWitness ta tb =
        unTypeF $
        singlePinaforeTypeF $
        mkTypeF $
        GroundPinaforeSingularType FuncPinaforeGroundType $
        ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
    tsFunctionNegWitness ta tb =
        unTypeF $
        singlePinaforeTypeF $
        mkTypeF $
        GroundPinaforeSingularType FuncPinaforeGroundType $
        ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
