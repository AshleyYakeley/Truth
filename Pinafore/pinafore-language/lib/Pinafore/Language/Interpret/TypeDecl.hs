module Pinafore.Language.Interpret.TypeDecl
    ( interpretSequentialTypeDeclaration
    , interpretRecursiveTypeDeclarations
    ) where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Interpret.TypeDecl.Data
import Pinafore.Language.Interpret.TypeDecl.OpenEntity
import Pinafore.Language.Interpret.TypeDecl.Predicate
import Pinafore.Language.Interpret.TypeDecl.StorableData
import Pinafore.Language.Interpreter
import Pinafore.Language.Type

getGroundType :: QSingularType 'Negative t -> QInterpreter (Some (QGroundType '[]))
getGroundType (GroundedDolanSingularType (MkDolanGroundedType gt NilCCRArguments)) = return $ MkSome gt
getGroundType t = throw $ DeclareDatatypeBadSupertypeError $ exprShow t

getGroundTypes :: QType 'Negative t -> QInterpreter [Some (QGroundType '[])]
getGroundTypes NilDolanType = return []
getGroundTypes (ConsDolanType t1 tr) = do
    gt1 <- getGroundType t1
    gtr <- getGroundTypes tr
    return $ gt1 : gtr

typeDeclarationTypeBox ::
       (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression)
    => FullName
    -> RawMarkdown
    -> SyntaxTypeDeclaration
    -> QInterpreter (QFixBox () ())
typeDeclarationTypeBox name doc OpenEntitySyntaxTypeDeclaration = makeOpenEntityTypeBox name doc
typeDeclarationTypeBox name doc (PredicateSyntaxTypeDeclaration storable st predicate) =
    makePredicateTypeBox name doc storable st predicate
typeDeclarationTypeBox name doc (StorableDatatypeSyntaxTypeDeclaration params sconss) =
    makeStorableDataTypeBox name doc params sconss
typeDeclarationTypeBox name doc (PlainDatatypeSyntaxTypeDeclaration params msst sconss) = do
    mstl <-
        for msst $ \sst -> do
            st <- interpretType @'Negative sst
            case st of
                MkSome t -> getGroundTypes t
    makePlainDataTypeBox (fromMaybe [] mstl) name doc params sconss

interpretSequentialTypeDeclaration ::
       (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression)
    => FullName
    -> RawMarkdown
    -> SyntaxTypeDeclaration
    -> QScopeBuilder ()
interpretSequentialTypeDeclaration name doc tdecl = do
    tbox <- builderLift $ typeDeclarationTypeBox name doc tdecl
    boxSequential tbox ()
    registerDocs $ pureForest $ typeDeclDoc name tdecl doc

interpretRecursiveTypeDeclarations ::
       (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression)
    => [(SourcePos, FullName, RawMarkdown, SyntaxTypeDeclaration)]
    -> QScopeBuilder ()
interpretRecursiveTypeDeclarations decls = do
    wfs <-
        for decls $ \(spos, name, doc, tdecl) -> do
            wf <- builderLift $ paramWith sourcePosParam spos $ typeDeclarationTypeBox name doc tdecl
            registerDocs $ pureForest $ typeDeclDoc name tdecl doc
            return wf
    boxRecursiveIO (mconcat wfs) ()
