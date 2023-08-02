module Pinafore.Language.Grammar.Interpret.TypeDecl
    ( interpretSequentialTypeDeclaration
    , interpretRecursiveTypeDeclarations
    ) where

import Data.Graph (SCC(..), stronglyConnComp)
import Pinafore.Language.DefDoc
import Pinafore.Language.Error
import Pinafore.Language.Grammar.Docs
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

typeDeclDoc :: FullName -> SyntaxTypeDeclaration -> RawMarkdown -> Tree DefDoc
typeDeclDoc = let
    sigDoc' :: SyntaxSignature' -> DocItem
    sigDoc' (ValueSyntaxSignature name stype msdef) = ValueSignatureDocItem name (exprShow stype) (isJust msdef)
    sigDoc' (SupertypeConstructorSyntaxSignature name) = SupertypeConstructorSignatureDocItem name
    sigDoc :: SyntaxSignature -> DefDoc
    sigDoc (MkSyntaxWithDoc doc (MkWithSourcePos _ sig)) = MkDefDoc (sigDoc' sig) doc
    funcPNT :: PrecNamedText -> PrecNamedText -> PrecNamedText
    funcPNT ta tb = namedTextPrec 6 $ precNamedText 5 ta <> " -> " <> precNamedText 6 tb
    funcPNTList :: [PrecNamedText] -> PrecNamedText -> PrecNamedText
    funcPNTList [] t = t
    funcPNTList (a:aa) t = funcPNT a $ funcPNTList aa t
    consDoc :: FullName -> [NamedText] -> SyntaxConstructorOrSubtype extra -> (DocItem, Docs)
    consDoc tname _ (ConstructorSyntaxConstructorOrSubtype cname tt _) =
        ( ValuePatternDocItem (pure $ fullNameRef cname) $
          toNamedText $ funcPNTList (fmap exprShowPrec tt) (exprShowPrec tname)
        , mempty)
    consDoc _ tparams (SubtypeSyntaxConstructorOrSubtype tname tt) =
        (TypeDocItem (pure $ fullNameRef tname) tparams, typeConssDoc tname tparams tt)
    consDoc tname _ (RecordSyntaxConstructorOrSubtype cname sigs) =
        (ValuePatternDocItem (pure $ fullNameRef cname) (exprShow tname), lpure $ fmap sigDoc sigs)
    typeConsDoc :: FullName -> [NamedText] -> SyntaxWithDoc (SyntaxConstructorOrSubtype extra) -> Tree DefDoc
    typeConsDoc tname tparams (MkSyntaxWithDoc cdoc scs) = let
        (item, rest) = consDoc tname tparams scs
        in MkTree (MkDefDoc item cdoc) rest
    typeConssDoc :: FullName -> [NamedText] -> [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)] -> Docs
    typeConssDoc tname tparams sdocs = MkForest $ fmap (typeConsDoc tname tparams) sdocs
    in \name defn doc -> let
           diNames = pure $ fullNameRef name
           (diParams, items) =
               case defn of
                   StorableDatatypeSyntaxTypeDeclaration params conss -> let
                       tparams = fmap exprShow params
                       in (tparams, typeConssDoc name tparams conss)
                   PlainDatatypeSyntaxTypeDeclaration params _ conss -> let
                       tparams = fmap exprShow params
                       in (tparams, typeConssDoc name tparams conss)
                   _ -> mempty
           docItem = TypeDocItem {..}
           docDescription = doc
           in MkTree MkDefDoc {..} items

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
    builderLift $ checkDynamicTypeCycles decls
    wfs <-
        for decls $ \(spos, name, doc, tdecl) -> do
            wf <- builderLift $ paramWith sourcePosParam spos $ typeDeclarationTypeBox name doc tdecl
            registerDocs $ pureForest $ typeDeclDoc name tdecl doc
            return wf
    boxRecursiveIO (mconcat wfs) ()
