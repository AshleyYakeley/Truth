module Pinafore.Language.Grammar.Interpret.TypeDecl
    ( interpretSequentialTypeDeclaration
    , interpretRecursiveTypeDeclarations
    ) where

import Data.Graph
import Pinafore.Language.Error
import Pinafore.Language.Grammar.Interpret.Type
import Pinafore.Language.Grammar.Interpret.TypeDecl.Data
import Pinafore.Language.Grammar.Interpret.TypeDecl.DynamicEntity
import Pinafore.Language.Grammar.Interpret.TypeDecl.OpenEntity
import Pinafore.Language.Grammar.Interpret.TypeDecl.StorableData
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Text
import Shapes

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
typeDeclarationTypeBox name doc (StorableDatatypeSyntaxTypeDeclaration params sconss) =
    makeStorableDataTypeBox name doc params sconss
typeDeclarationTypeBox name doc (PlainDatatypeSyntaxTypeDeclaration params msst sconss) = do
    mstl <-
        for msst $ \sst -> do
            st <- interpretType @'Negative sst
            case st of
                MkSome t -> getGroundTypes t
    makePlainDataTypeBox (fromMaybe [] mstl) name doc params sconss
typeDeclarationTypeBox name doc (DynamicEntitySyntaxTypeDeclaration stcons) = makeDynamicEntityTypeBox name doc stcons

checkDynamicTypeCycles :: [(SourcePos, FullName, RawMarkdown, SyntaxTypeDeclaration)] -> QInterpreter ()
checkDynamicTypeCycles decls = let
    constructorName :: SyntaxDynamicEntityConstructor -> Maybe FullName
    constructorName (NameSyntaxDynamicEntityConstructor ns nref) = Just $ namespaceConcatFullName ns nref
    constructorName _ = Nothing
    getDynamicTypeReferences ::
           (SourcePos, FullName, RawMarkdown, SyntaxTypeDeclaration)
        -> Maybe ((SourcePos, FullName), FullName, [FullName])
    getDynamicTypeReferences (spos, n, _, DynamicEntitySyntaxTypeDeclaration cs) =
        Just $ ((spos, n), n, mapMaybe constructorName $ toList cs)
    getDynamicTypeReferences _ = Nothing
    sccs :: [SCC (SourcePos, FullName)]
    sccs = stronglyConnComp $ mapMaybe getDynamicTypeReferences decls
    sccNames :: forall a. SCC a -> Maybe (NonEmpty a)
    sccNames (CyclicSCC (n:nn)) = Just $ n :| nn
    sccNames _ = Nothing
    in case mapMaybe sccNames sccs of
           [] -> return ()
           (nn@((spos, _) :| _):_) -> paramWith sourcePosParam spos $ throw $ DeclareDynamicTypeCycleError $ fmap snd nn

interpretSequentialTypeDeclaration ::
       (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression)
    => FullName
    -> RawMarkdown
    -> SyntaxTypeDeclaration
    -> QScopeInterpreter ()
interpretSequentialTypeDeclaration name doc tdecl = do
    tbox <- lift $ typeDeclarationTypeBox name doc tdecl
    boxSequential tbox ()

interpretRecursiveTypeDeclarations ::
       (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression)
    => [(SourcePos, FullName, RawMarkdown, SyntaxTypeDeclaration)]
    -> QScopeInterpreter ()
interpretRecursiveTypeDeclarations decls = do
    lift $ checkDynamicTypeCycles decls
    wfs <-
        for decls $ \(spos, name, doc, tdecl) ->
            lift $ paramWith sourcePosParam spos $ typeDeclarationTypeBox name doc tdecl
    boxRecursiveIO (mconcat wfs) ()
