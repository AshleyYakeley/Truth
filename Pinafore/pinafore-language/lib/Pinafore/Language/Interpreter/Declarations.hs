module Pinafore.Language.Interpreter.Declarations where

import Import
import Pinafore.Language.Interpreter.Interpreter
import Pinafore.Language.Interpreter.Scope
import Pinafore.Language.Type.Subtype ()

data QDeclarations = MkQDeclarations
    { declsScopes :: [QScope]
    , declsDocs :: Docs
    }

declarations :: QScope -> QDeclarations
declarations scope = mempty{declsScopes = [scope]}

declarationsToScope :: QDeclarations -> QInterpreter QScope
declarationsToScope decls = joinAllScopes $ declsScopes decls

instance Semigroup QDeclarations where
    MkQDeclarations sa da <> MkQDeclarations sb db = MkQDeclarations (sa <> sb) (da <> db)

instance Monoid QDeclarations where
    mempty = MkQDeclarations mempty mempty

withDeclarations :: QDeclarations -> QInterpreter --> QInterpreter
withDeclarations decls ma = do
    scope <- declarationsToScope decls
    withScope scope ma

moduleDeclarations :: QModule -> QDeclarations
moduleDeclarations MkQModule{..} = let
    declsScopes = [moduleScope]
    declsDocs = moduleDoc
    in MkQDeclarations{..}

declarationsModule :: QDeclarations -> QInterpreter QModule
declarationsModule decls = do
    moduleScope <- declarationsToScope decls
    let moduleDoc = declsDocs decls
    return MkQModule{..}
