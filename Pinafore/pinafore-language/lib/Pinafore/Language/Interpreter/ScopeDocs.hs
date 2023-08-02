module Pinafore.Language.Interpreter.ScopeDocs where

import Pinafore.Language.Grammar.Docs
import Pinafore.Language.Interpreter.Interpreter
import Pinafore.Language.Interpreter.Scope
import Pinafore.Language.Type.Subtype ()
import Shapes

data QScopeDocs = MkQScopeDocs
    { sdScopes :: [QScope]
    , sdDocs :: Docs
    }

instance Semigroup QScopeDocs where
    MkQScopeDocs sa da <> MkQScopeDocs sb db = MkQScopeDocs (sa <> sb) (da <> db)

instance Monoid QScopeDocs where
    mempty = MkQScopeDocs mempty mempty

withScopeDocs :: QScopeDocs -> QInterpreter --> QInterpreter
withScopeDocs sd ma = do
    scope <- joinAllScopes $ sdScopes sd
    paramLocalM scopeParam (\oldscope -> joinScopes oldscope scope) ma
