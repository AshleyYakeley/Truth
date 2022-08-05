module Pinafore.Language.Grammar.Interpret.TypeDecl
    ( interpretSequentialTypeDeclaration
    , interpretRecursiveTypeDeclarations
    ) where

import Data.Graph
import Pinafore.Language.Error
import Pinafore.Language.Grammar.Interpret.TypeDecl.ClosedEntity
import Pinafore.Language.Grammar.Interpret.TypeDecl.Data
import Pinafore.Language.Grammar.Interpret.TypeDecl.DynamicEntity
import Pinafore.Language.Grammar.Interpret.TypeDecl.OpenEntity
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes

typeDeclarationTypeBox :: Name -> Markdown -> SyntaxTypeDeclaration -> PinaforeInterpreter (PinaforeFixBox () ())
typeDeclarationTypeBox name doc OpenEntitySyntaxTypeDeclaration = makeOpenEntityTypeBox name doc
typeDeclarationTypeBox name doc (ClosedEntitySyntaxTypeDeclaration params sconss) =
    makeClosedEntityTypeBox name doc params sconss
typeDeclarationTypeBox name doc (DatatypeSyntaxTypeDeclaration params sconss) = makeDataTypeBox name doc params sconss
typeDeclarationTypeBox name doc (DynamicEntitySyntaxTypeDeclaration stcons) = makeDynamicEntityTypeBox name doc stcons

checkDynamicTypeCycles :: [(SourcePos, Name, Markdown, SyntaxTypeDeclaration)] -> PinaforeInterpreter ()
checkDynamicTypeCycles decls = let
    constructorName :: SyntaxDynamicEntityConstructor -> Maybe Name
    constructorName (NameSyntaxDynamicEntityConstructor (UnqualifiedReferenceName n)) = Just n
    constructorName _ = Nothing
    getDynamicTypeReferences ::
           (SourcePos, Name, Markdown, SyntaxTypeDeclaration) -> Maybe ((SourcePos, Name), Name, [Name])
    getDynamicTypeReferences (spos, n, _, DynamicEntitySyntaxTypeDeclaration cs) =
        Just $ ((spos, n), n, mapMaybe constructorName $ toList cs)
    getDynamicTypeReferences _ = Nothing
    sccs :: [SCC (SourcePos, Name)]
    sccs = stronglyConnComp $ mapMaybe getDynamicTypeReferences decls
    sccNames :: forall a. SCC a -> Maybe (NonEmpty a)
    sccNames (CyclicSCC (n:nn)) = Just $ n :| nn
    sccNames _ = Nothing
    in case mapMaybe sccNames sccs of
           [] -> return ()
           (nn@((spos, _) :| _):_) -> paramWith sourcePosParam spos $ throw $ DeclareDynamicTypeCycleError $ fmap snd nn

interpretSequentialTypeDeclaration :: Name -> Markdown -> SyntaxTypeDeclaration -> PinaforeScopeInterpreter ()
interpretSequentialTypeDeclaration name doc tdecl = do
    tbox <- lift $ typeDeclarationTypeBox name doc tdecl
    boxSequential tbox ()

interpretRecursiveTypeDeclarations ::
       [(SourcePos, Name, Markdown, SyntaxTypeDeclaration)] -> PinaforeScopeInterpreter ()
interpretRecursiveTypeDeclarations decls = do
    lift $ checkDynamicTypeCycles decls
    wfs <-
        for decls $ \(spos, name, doc, tdecl) ->
            lift $ paramWith sourcePosParam spos $ typeDeclarationTypeBox name doc tdecl
    boxRecursiveIO (mconcat wfs) ()
