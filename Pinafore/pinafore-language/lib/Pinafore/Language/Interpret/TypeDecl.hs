module Pinafore.Language.Interpret.TypeDecl
    ( interpretSequentialTypeDeclaration
    , interpretRecursiveTypeDeclarations
    , interpretNonrecursiveTypeDeclaration
    )
where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Interpret.TypeDecl.Data
import Pinafore.Language.Interpret.TypeDecl.OpenEntity
import Pinafore.Language.Interpret.TypeDecl.Predicate
import Pinafore.Language.Interpret.TypeDecl.StorableData
import Pinafore.Language.Interpret.TypeDecl.Synonym
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
    (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression) =>
    FullName ->
    RawMarkdown ->
    SyntaxRecursiveTypeDeclaration ->
    QInterpreter (QFixBox () ())
typeDeclarationTypeBox name doc OpenEntitySyntaxRecursiveTypeDeclaration = makeOpenEntityTypeBox name doc
typeDeclarationTypeBox name doc (StorableDatatypeSyntaxRecursiveTypeDeclaration params sconss) =
    makeStorableDataTypeBox name doc params sconss
typeDeclarationTypeBox name doc (PlainDatatypeSyntaxRecursiveTypeDeclaration params msst sconss) = do
    mstl <-
        for msst $ \sst -> do
            st <- interpretType @'Negative sst
            case st of
                MkSome t -> getGroundTypes t
    makePlainDataTypeBox (fromMaybe [] mstl) name doc params sconss

interpretSequentialTypeDeclaration ::
    (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression) =>
    FullName ->
    RawMarkdown ->
    SyntaxRecursiveTypeDeclaration ->
    QScopeBuilder ()
interpretSequentialTypeDeclaration name doc tdecl = do
    tbox <- builderLift $ typeDeclarationTypeBox name doc tdecl
    boxSequential tbox ()
    registerDocs $ pureForest $ typeDeclDoc name tdecl doc

interpretRecursiveTypeDeclarations ::
    (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression) =>
    [(SourcePos, FullName, RawMarkdown, SyntaxRecursiveTypeDeclaration)] ->
    QScopeBuilder ()
interpretRecursiveTypeDeclarations decls = do
    wfs <-
        for decls $ \(spos, name, doc, tdecl) -> do
            wf <- builderLift $ paramWith sourcePosParam spos $ typeDeclarationTypeBox name doc tdecl
            registerDocs $ pureForest $ typeDeclDoc name tdecl doc
            return wf
    boxRecursiveIO (mconcat wfs) ()

interpretNonrecursiveTypeDeclaration ::
    (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression) =>
    FullName ->
    RawMarkdown ->
    SyntaxNonrecursiveTypeDeclaration ->
    QScopeBuilder ()
interpretNonrecursiveTypeDeclaration name doc (SynonymSyntaxNonrecursiveTypeDeclaration storable tparams st) =
    makeSynonymTypeBox name doc storable tparams st
interpretNonrecursiveTypeDeclaration name doc (PredicateSyntaxNonrecursiveTypeDeclaration storable st predicate) = do
    tbox <- builderLift $ makePredicateTypeBox name doc storable st predicate
    boxSequential tbox ()
