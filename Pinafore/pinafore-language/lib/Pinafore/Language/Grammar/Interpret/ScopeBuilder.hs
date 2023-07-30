module Pinafore.Language.Grammar.Interpret.ScopeBuilder
    ( ScopeBuilder
    , runScopeBuilder
    , sourcePosScopeBuilder
    , interpScopeBuilder
    , refScopeBuilder
    , pureScopeBuilder
    , allocateVarScopeBuilder
    ) where

import Pinafore.Language.Grammar.Interpret.RefNotation
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.VarID
import Shapes

type ScopeBuilder = TransformT RefNotation

runScopeBuilder :: ScopeBuilder a -> (a -> RefNotation b) -> RefNotation b
runScopeBuilder sb = unTransformT sb

sourcePosScopeBuilder :: SourcePos -> ScopeBuilder ()
sourcePosScopeBuilder spos = interpScopeBuilder $ refPut (transformParamRef sourcePosParam) spos

interpScopeBuilder :: QScopeInterpreter --> ScopeBuilder
interpScopeBuilder = liftTransformT . liftTransformT

refScopeBuilder :: RefNotation (ScopeBuilder a) -> ScopeBuilder a
refScopeBuilder = execMapTransformT

pureScopeBuilder :: QScope -> ScopeBuilder ()
pureScopeBuilder scope = interpScopeBuilder $ registerScope scope

allocateVarScopeBuilder :: Maybe FullName -> ScopeBuilder (FullName, VarID)
allocateVarScopeBuilder n = interpScopeBuilder $ allocateVar n
