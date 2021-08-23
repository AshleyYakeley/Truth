module Pinafore.Language.Grammar.Interpret
    ( interpretTopExpression
    , interpretModule
    , interpretTopDeclarations
    , interpretType
    ) where

import Data.Graph
import Pinafore.Base
import Pinafore.Language.DefDoc
import Pinafore.Language.DocTree
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Interpret.RefNotation
import Pinafore.Language.Grammar.Interpret.Type
import Pinafore.Language.Grammar.Interpret.TypeDecl
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.If
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Pinafore.Language.Var
import Pinafore.Markdown
import Shapes

type instance EntryDoc PinaforeTypeSystem = DefDoc

interpretPatternConstructor :: SyntaxConstructor -> PinaforeSourceInterpreter (QPatternConstructor)
interpretPatternConstructor (SLNamedConstructor name) = lookupPatternConstructor name
interpretPatternConstructor (SLNumber v) =
    return $
    qToPatternConstructor $ \v' ->
        if v == v'
            then Just ()
            else Nothing
interpretPatternConstructor (SLString v) =
    return $
    qToPatternConstructor $ \v' ->
        if v == v'
            then Just ()
            else Nothing
interpretPatternConstructor SLUnit = return $ qToPatternConstructor $ \() -> Just ()
interpretPatternConstructor SLPair = return $ qToPatternConstructor $ \(a :: A, b :: B) -> Just $ (a, (b, ()))

interpretPattern :: SyntaxPattern -> RefNotation QPattern
interpretPattern (MkWithSourcePos _ AnySyntaxPattern) = return qAnyPattern
interpretPattern (MkWithSourcePos _ (VarSyntaxPattern n)) = return $ qVarPattern n
interpretPattern (MkWithSourcePos spos (BothSyntaxPattern spat1 spat2)) = do
    pat1 <- interpretPattern spat1
    pat2 <- interpretPattern spat2
    liftRefNotation $ runSourcePos spos $ qBothPattern pat1 pat2
interpretPattern (MkWithSourcePos spos (ConstructorSyntaxPattern scons spats)) = do
    pc <- liftRefNotation $ runSourcePos spos $ interpretPatternConstructor scons
    pats <- for spats interpretPattern
    liftRefNotation $ runSourcePos spos $ qConstructPattern pc pats
interpretPattern (MkWithSourcePos spos (TypedSyntaxPattern spat stype)) = do
    pat <- interpretPattern spat
    liftRefNotation $
        runSourcePos spos $ do
            mtp <- interpretType @'Positive stype
            case mtp of
                MkAnyW tp -> do
                    MkGreatestDynamicSupertype dtp _ convm <- getGreatestDynamicSupertype tp
                    let
                        pc :: QPatternConstructor
                        pc =
                            toPatternConstructor dtp (ConsListType (mkPolarShimWit tp) NilListType) $ \dt ->
                                fmap (\a -> (a, ())) (shimToFunction convm dt)
                    qConstructPattern pc [pat]

interpretPatternOrName :: SyntaxPattern -> Either Name (RefNotation QPattern)
interpretPatternOrName (MkWithSourcePos _ (VarSyntaxPattern n)) = Left n
interpretPatternOrName pat = Right $ interpretPattern pat

interpretExpression :: SyntaxExpression -> RefExpression
interpretExpression (MkWithSourcePos spos sexpr) = interpretExpression' spos sexpr

type DocSyntaxBinding = (Markdown, SyntaxBinding)

getBindingNode :: DocSyntaxBinding -> (DocSyntaxBinding, Name, [Name])
getBindingNode db@(_, b@(MkSyntaxBinding _ _ n _)) = (db, n, setToList $ syntaxFreeVariables b)

-- | Group bindings into a topologically-sorted list of strongly-connected components
clumpBindings :: [DocSyntaxBinding] -> [[DocSyntaxBinding]]
clumpBindings bb = fmap flattenSCC $ stronglyConnComp $ fmap getBindingNode bb

interpretRecursiveLetBindingsClump :: SourcePos -> [DocSyntaxBinding] -> MFunction RefNotation RefNotation
interpretRecursiveLetBindingsClump spos sbinds ra = do
    bl <- interpretBindings sbinds
    remonadRefNotation
        (MkWMFunction $ \se -> do
             bmap <- runSourcePos spos $ qUncheckedBindingsRecursiveLetExpr bl
             withNewLetBindings bmap se)
        ra

interpretRecursiveLetBindingss :: SourcePos -> [[DocSyntaxBinding]] -> MFunction RefNotation RefNotation
interpretRecursiveLetBindingss _ [] ra = ra
interpretRecursiveLetBindingss spos (b:bb) ra =
    interpretRecursiveLetBindingsClump spos b $ interpretRecursiveLetBindingss spos bb ra

interpretRecursiveLetBindings :: SourcePos -> [DocSyntaxBinding] -> MFunction RefNotation RefNotation
interpretRecursiveLetBindings spos sbinds ra = do
    liftRefNotation $ runSourcePos spos $ checkSyntaxBindingsDuplicates $ fmap snd sbinds
    interpretRecursiveLetBindingss spos (clumpBindings sbinds) ra

interpretSequentialLetBinding :: DocSyntaxBinding -> MFunction RefNotation RefNotation
interpretSequentialLetBinding sbind ra = do
    b <- interpretBinding sbind
    remonadRefNotation
        (MkWMFunction $ \se -> do
             bmap <- runSourcePos (getSourcePos $ snd sbind) $ qBindingSequentialLetExpr b
             withNewLetBindings bmap se)
        ra

interpretRecursiveDocDeclarations ::
       SourcePos -> [SyntaxWithDoc SyntaxDirectDeclaration] -> RefNotation a -> RefNotation ([DocTreeEntry DefDoc], a)
interpretRecursiveDocDeclarations dspos ddecls ma = do
    (typeDecls, MkChain subtypeDecls, bindingDecls, docDecls) <-
        liftRefNotation $
        fmap mconcat $
        for ddecls $ \(MkSyntaxWithDoc doc decl) ->
            case decl of
                TypeSyntaxDeclaration spos name defn ->
                    return $ let
                        docName = toText name
                        docValueType = ""
                        docType = TypeDocType
                        docDescription = doc
                        in (pure (spos, name, doc, defn), mempty, mempty, pure $ EntryDocTreeEntry $ MkDefDoc {..})
                SubtypeSyntaxDeclaration spos sta stb ->
                    return $ let
                        docName = exprShow sta <> " <: " <> exprShow stb
                        docValueType = ""
                        docType = SubtypeRelationDocType
                        docDescription = doc
                        in ( mempty
                           , MkChain $ MkWMFunction $ mapSourcePos spos $ interpretSubtypeRelation sta stb
                           , mempty
                           , pure $ EntryDocTreeEntry $ MkDefDoc {..})
                BindingSyntaxDeclaration sbind@(MkSyntaxBinding _ mtype name _) ->
                    return $ let
                        docName = toText name
                        docValueType =
                            case mtype of
                                Nothing -> ""
                                Just st -> exprShow st
                        docType = ValueDocType
                        docDescription = doc
                        in (mempty, mempty, pure (doc, sbind), pure $ EntryDocTreeEntry $ MkDefDoc {..})
    a <-
        remonadRefNotation (MkWMFunction (interpretRecursiveTypeDeclarations typeDecls) . subtypeDecls) $
        interpretRecursiveLetBindings dspos bindingDecls ma
    return (docDecls, a)

exposeDeclids :: [Name] -> [DefDoc] -> [DefDoc]
exposeDeclids names decls = let
    inDecl :: Name -> Maybe DefDoc
    inDecl n = find (\doc -> docName doc == toText n) decls
    isSubtypeDDI :: DefDoc -> Bool
    isSubtypeDDI doc =
        case docType doc of
            SubtypeRelationDocType -> True
            _ -> False
    in mapMaybe inDecl names <> filter isSubtypeDDI decls

exposeDeclsDocTree :: [Name] -> [DocTreeEntry DefDoc] -> [DocTreeEntry DefDoc]
exposeDeclsDocTree names = fmap EntryDocTreeEntry . exposeDeclids names . mconcat . fmap toList

interpretExpose :: SyntaxExpose -> RefNotation ([DocTreeEntry DefDoc] -> [DocTreeEntry DefDoc], PinaforeScope)
interpretExpose (SExpExpose spos names) = do
    scope <- liftRefNotation $ runSourcePos spos $ exportScope names
    return (exposeDeclsDocTree names, scope)
interpretExpose (SExpLet sdecls expose) = do
    (olddoc, (restrict, scope)) <- interpretDocDeclarations sdecls $ interpretExpose expose
    return (\newdoc -> restrict $ olddoc <> newdoc, scope)

interpretDocDeclaration :: SyntaxWithDoc SyntaxDeclaration -> RefNotation a -> RefNotation ([DocTreeEntry DefDoc], a)
interpretDocDeclaration (MkSyntaxWithDoc doc decl) ma =
    case decl of
        ExposeSyntaxDeclaration _ sexp -> do
            (docs, scope) <- interpretExpose sexp
            a <- remonadRefNotation (MkWMFunction $ importScope scope) ma
            return $ (docs [], a)
        ImportSyntaxDeclaration spos mname -> do
            newmod <- liftRefNotation $ runSourcePos spos $ getModule mname
            a <- remonadRefNotation (MkWMFunction $ importScope $ moduleScope newmod) ma
            return $ (pure $ TreeDocTreeEntry $ moduleDoc newmod, a)
        DirectSyntaxDeclaration (TypeSyntaxDeclaration spos name defn) -> do
            a <- remonadRefNotation (MkWMFunction $ interpretTypeDeclaration spos name doc defn) ma
            let
                docName = toText name
                docValueType = ""
                docType = TypeDocType
                docDescription = doc
            return (pure $ EntryDocTreeEntry $ MkDefDoc {..}, a)
        DirectSyntaxDeclaration (SubtypeSyntaxDeclaration spos sta stb) -> do
            a <- remonadRefNotation (MkWMFunction $ mapSourcePos spos $ interpretSubtypeRelation sta stb) ma
            let
                docName = exprShow sta <> " <: " <> exprShow stb
                docValueType = ""
                docType = SubtypeRelationDocType
                docDescription = doc
            return (pure $ EntryDocTreeEntry $ MkDefDoc {..}, a)
        DirectSyntaxDeclaration (BindingSyntaxDeclaration sbind@(MkSyntaxBinding _ mtype name _)) -> do
            a <- interpretSequentialLetBinding (doc, sbind) ma
            let
                docName = toText name
                docValueType =
                    case mtype of
                        Nothing -> ""
                        Just st -> exprShow st
                docType = ValueDocType
                docDescription = doc
            return (pure $ EntryDocTreeEntry $ MkDefDoc {..}, a)
        RecursiveSyntaxDeclaration spos rdecls -> interpretRecursiveDocDeclarations spos rdecls ma

interpretDocDeclarations :: [SyntaxWithDoc SyntaxDeclaration] -> RefNotation a -> RefNotation ([DocTreeEntry DefDoc], a)
interpretDocDeclarations [] ma = do
    a <- ma
    return ([], a)
interpretDocDeclarations (decl:decls) ma =
    fmap (\(olddoc, (newdoc, a)) -> (olddoc <> newdoc, a)) $
    interpretDocDeclaration decl $ interpretDocDeclarations decls ma

interpretDeclarations :: [SyntaxWithDoc SyntaxDeclaration] -> MFunction RefNotation RefNotation
interpretDeclarations decls ma = fmap snd $ interpretDocDeclarations decls ma

interpretNamedConstructor :: SourcePos -> ReferenceName -> RefExpression
interpretNamedConstructor spos n = do
    me <- liftRefNotation $ runSourcePos spos $ lookupLetBinding n
    case me of
        Just e -> return e
        Nothing -> throwErrorType spos $ InterpretConstructorUnknownError n

interpretConstructor :: SourcePos -> SyntaxConstructor -> RefExpression
interpretConstructor _ (SLNumber n) =
    return $
    case decode safeRationalNumber n of
        Just r ->
            case decode integerSafeRational r of
                Just i -> qConstExprAny $ jmToValue i
                Nothing -> qConstExprAny $ jmToValue r
        Nothing -> qConstExprAny $ jmToValue n
interpretConstructor _ (SLString v) = return $ qConstExprAny $ jmToValue v
interpretConstructor spos (SLNamedConstructor v) = interpretNamedConstructor spos v
interpretConstructor _ SLPair = return $ qConstExprAny $ jmToValue ((,) :: A -> B -> (A, B))
interpretConstructor _ SLUnit = return $ qConstExprAny $ jmToValue ()

specialFormArg :: PinaforeAnnotation t -> SyntaxAnnotation -> ComposeM Maybe PinaforeSourceInterpreter t
specialFormArg AnnotAnchor (SAAnchor anchor) = return anchor
specialFormArg AnnotMonoEntityType (SAType st) = liftOuter $ interpretMonoEntityType st
specialFormArg AnnotOpenEntityType (SAType st) = liftOuter $ interpretOpenEntityType st
specialFormArg AnnotConcreteDynamicEntityType (SAType st) = liftOuter $ interpretConcreteDynamicEntityType st
specialFormArg AnnotPositiveType (SAType st) = liftOuter $ interpretType @'Positive st
specialFormArg AnnotNegativeType (SAType st) = liftOuter $ interpretType @'Negative st
specialFormArg _ _ = liftInner Nothing

specialFormArgs ::
       ListType PinaforeAnnotation lt -> [SyntaxAnnotation] -> ComposeM Maybe PinaforeSourceInterpreter (HList lt)
specialFormArgs NilListType [] = return ()
specialFormArgs (ConsListType t tt) (a:aa) = do
    v <- specialFormArg t a
    vv <- specialFormArgs tt aa
    return (v, vv)
specialFormArgs _ _ = liftInner Nothing

showSA :: SyntaxAnnotation -> Text
showSA (SAType _) = "type"
showSA (SAAnchor _) = "anchor"

showAnnotation :: PinaforeAnnotation a -> Text
showAnnotation AnnotAnchor = "anchor"
showAnnotation AnnotMonoEntityType = "type"
showAnnotation AnnotOpenEntityType = "type"
showAnnotation AnnotConcreteDynamicEntityType = "type"
showAnnotation AnnotPositiveType = "type"
showAnnotation AnnotNegativeType = "type"

interpretSpecialForm :: ReferenceName -> NonEmpty SyntaxAnnotation -> PinaforeSourceInterpreter QValue
interpretSpecialForm name annotations = do
    MkSpecialForm largs val <- lookupSpecialForm name
    margs <- getComposeM $ specialFormArgs largs $ toList annotations
    case margs of
        Just args -> val args
        Nothing ->
            throw $
            SpecialFormWrongAnnotationsError
                name
                (listTypeToList showAnnotation largs)
                (fmap showSA $ toList annotations)

interpretConstant :: SourcePos -> SyntaxConstant -> RefExpression
interpretConstant _ SCIfThenElse = return $ qConstExprAny $ jmToValue qifthenelse
interpretConstant _ SCBind = return $ qConstExprAny $ jmToValue qbind
interpretConstant _ SCBind_ = return $ qConstExprAny $ jmToValue qbind_
interpretConstant spos (SCConstructor lit) = interpretConstructor spos lit

interpretCase :: SyntaxCase -> RefNotation (QPattern, QExpr)
interpretCase (MkSyntaxCase spat sexpr) = do
    pat <- interpretPattern spat
    expr <- interpretExpressionShadowed (sealedPatternNames pat) sexpr
    return (pat, expr)

interpretExpressionShadowed :: [Name] -> SyntaxExpression -> RefExpression
interpretExpressionShadowed names sbody =
    remonadRefNotation (MkWMFunction $ withRemovedBindings names) $ interpretExpression sbody

interpretExpression' :: SourcePos -> SyntaxExpression' -> RefExpression
interpretExpression' spos (SESubsume sexpr stype) = do
    expr <- interpretExpression sexpr
    liftRefNotation $
        runSourcePos spos $ do
            t <- interpretType stype
            qSubsumeExpr t expr
interpretExpression' spos (SEAbstract spat sbody) =
    case interpretPatternOrName spat of
        Left name -> do
            val <- interpretExpressionShadowed [name] sbody
            liftRefNotation $ runSourcePos spos $ qAbstractExpr name val
        Right mpat -> do
            pat <- mpat
            val <- interpretExpressionShadowed (sealedPatternNames pat) sbody
            liftRefNotation $ runSourcePos spos $ qCaseAbstract [(pat, val)]
interpretExpression' _ (SELet sdecls sbody) = interpretDeclarations sdecls $ interpretExpression sbody
interpretExpression' spos (SECase sbody scases) = do
    body <- interpretExpression sbody
    pairs <- for scases interpretCase
    liftRefNotation $ runSourcePos spos $ qCase body pairs
interpretExpression' spos (SEApply sf sarg) = do
    f <- interpretExpression sf
    arg <- interpretExpression sarg
    liftRefNotation $ runSourcePos spos $ qApplyExpr f arg
interpretExpression' spos (SEConst c) = interpretConstant spos c
interpretExpression' spos (SEVar name) = varRefExpr spos name
interpretExpression' spos (SESpecialForm name annots) =
    liftRefNotation $
    runSourcePos spos $ do
        val <- interpretSpecialForm name annots
        return $ qConstExprAny val
interpretExpression' spos (SERef sexpr) = refNotationQuote spos $ interpretExpression sexpr
interpretExpression' spos (SEUnref sexpr) = refNotationUnquote spos $ interpretExpression sexpr
interpretExpression' spos (SEList sexprs) = do
    exprs <- for sexprs interpretExpression
    liftRefNotation $ runSourcePos spos $ qSequenceExpr exprs

interpretBinding :: DocSyntaxBinding -> RefNotation QBinding
interpretBinding (doc, MkSyntaxBinding spos mstype name sexpr) = do
    mtype <- liftRefNotation $ runSourcePos spos $ for mstype interpretType
    expr <- interpretExpression sexpr
    return $ qBindExpr name doc mtype expr

interpretBindings :: [DocSyntaxBinding] -> RefNotation [QBinding]
interpretBindings sbinds = for sbinds interpretBinding

interpretTopDeclarations :: SyntaxTopDeclarations -> MFunction PinaforeInterpreter PinaforeInterpreter
interpretTopDeclarations (MkSyntaxTopDeclarations spos sdecls) ma =
    runSourcePos spos $ runRefNotation $ interpretDeclarations sdecls $ liftRefNotation ma

interpretTopExpression :: SyntaxExpression -> PinaforeInterpreter QExpr
interpretTopExpression sexpr@(MkWithSourcePos spos _) = runSourcePos spos $ runRefNotation $ interpretExpression sexpr

interpretModule :: ModuleName -> SyntaxModule -> PinaforeSourceInterpreter PinaforeModule
interpretModule moduleName smod = do
    (docs, scope) <- runRefNotation $ interpretExpose smod
    return $ MkModule (MkDocTree (toText moduleName) "" $ docs []) scope
