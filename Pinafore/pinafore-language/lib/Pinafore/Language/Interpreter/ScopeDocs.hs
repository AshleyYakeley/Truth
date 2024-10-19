module Pinafore.Language.Interpreter.ScopeDocs where

import Import
import Pinafore.Language.Interpreter.Interpreter
import Pinafore.Language.Interpreter.Scope
import Pinafore.Language.Type.Subtype ()

data QScopeDocs = MkQScopeDocs
    { sdScopes :: [QScope]
    , sdDocs :: Docs
    }

scopeDocs :: QScope -> QScopeDocs
scopeDocs scope = mempty {sdScopes = [scope]}

instance Semigroup QScopeDocs where
    MkQScopeDocs sa da <> MkQScopeDocs sb db = MkQScopeDocs (sa <> sb) (da <> db)

instance Monoid QScopeDocs where
    mempty = MkQScopeDocs mempty mempty

withScopeDocs :: QScopeDocs -> QInterpreter --> QInterpreter
withScopeDocs sd ma = do
    scope <- joinAllScopes $ sdScopes sd
    paramLocalM scopeParam (\oldscope -> joinScopes oldscope scope) ma

moduleScopeDocs :: QModule -> QScopeDocs
moduleScopeDocs MkQModule {..} = let
    sdScopes = [moduleScope]
    sdDocs = moduleDoc
    in MkQScopeDocs {..}
