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

instance Semigroup QDeclarations where
    MkQDeclarations sa da <> MkQDeclarations sb db = MkQDeclarations (sa <> sb) (da <> db)

instance Monoid QDeclarations where
    mempty = MkQDeclarations mempty mempty

withDeclarations :: QDeclarations -> QInterpreter --> QInterpreter
withDeclarations sd ma = do
    scope <- joinAllScopes $ declsScopes sd
    paramLocalM scopeParam (\oldscope -> joinScopes oldscope scope) ma

moduleDeclarations :: QModule -> QDeclarations
moduleDeclarations MkQModule{..} = let
    declsScopes = [moduleScope]
    declsDocs = moduleDoc
    in MkQDeclarations{..}

declarationsModule :: QDeclarations -> QInterpreter QModule
declarationsModule MkQDeclarations{..} = do
    moduleScope <- joinAllScopes declsScopes
    let moduleDoc = declsDocs
    return MkQModule{..}
