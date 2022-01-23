module Pinafore.Language.Grammar.Interpret.TypeDecl
    ( interpretTypeDeclaration
    , interpretRecursiveTypeDeclarations
    ) where

import Data.Graph
import Pinafore.Language.Error
import Pinafore.Language.Grammar.Interpret.TypeDecl.ClosedEntity
import Pinafore.Language.Grammar.Interpret.TypeDecl.Data
import Pinafore.Language.Grammar.Interpret.TypeDecl.DynamicEntity
import Pinafore.Language.Grammar.Interpret.TypeDecl.OpenEntity
import Pinafore.Language.Grammar.Interpret.TypeDecl.TypeBox
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes

typeDeclarationTypeBox :: Name -> Markdown -> SyntaxTypeDeclaration -> PinaforeInterpreter PinaforeTypeBox
typeDeclarationTypeBox name doc OpenEntitySyntaxTypeDeclaration = makeOpenEntityTypeBox name doc
typeDeclarationTypeBox name doc (ClosedEntitySyntaxTypeDeclaration sconss) = makeClosedEntityTypeBox name doc sconss
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
           (nn@((spos, _) :| _):_) -> withD sourcePosParam spos $ throw $ DeclareDynamicTypeCycleError $ fmap snd nn

interpretTypeDeclaration :: Name -> Markdown -> SyntaxTypeDeclaration -> PinaforeInterpreter --> PinaforeInterpreter
interpretTypeDeclaration name doc tdecl ma = do
    tbox <- typeDeclarationTypeBox name doc tdecl
    (wtt, MkCatEndo wcc) <- registerTypeName tbox
    runWMFunction (wtt . wcc) ma

interpretRecursiveTypeDeclarations ::
       [(SourcePos, Name, Markdown, SyntaxTypeDeclaration)] -> PinaforeInterpreter --> PinaforeInterpreter
interpretRecursiveTypeDeclarations decls ma = do
    checkDynamicTypeCycles decls
    wfs <- for decls $ \(spos, name, doc, tdecl) -> withD sourcePosParam spos $ typeDeclarationTypeBox name doc tdecl
    (wtt, MkCatEndo wcc) <- registerRecursiveTypeNames wfs
    runWMFunction (wtt . wcc) ma
