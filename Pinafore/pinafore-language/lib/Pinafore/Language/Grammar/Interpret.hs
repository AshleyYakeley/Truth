module Pinafore.Language.Grammar.Interpret
    ( interpretTopExpression
    , interpretModule
    , interpretTopDeclarations
    , interpretType
    , interpretImportDeclaration
    ) where

import Data.Graph
import Pinafore.Base
import Pinafore.Language.DefDoc
import Pinafore.Language.DocTree
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Interpret.RefNotation
import Pinafore.Language.Grammar.Interpret.ScopeBuilder
import Pinafore.Language.Grammar.Interpret.Type
import Pinafore.Language.Grammar.Interpret.TypeDecl
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.If
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Std.Types
import Pinafore.Language.Name
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Pinafore.Language.Var
import Pinafore.Language.VarID
import Pinafore.Markdown
import Shapes
import Pinafore.Language.Grammar.FreeVars

type instance EntryDoc QTypeSystem = DefDoc

interpretPatternConstructor :: SyntaxConstructor -> QInterpreter (QPatternConstructor)
interpretPatternConstructor (SLNamedConstructor name) = lookupPatternConstructor name
interpretPatternConstructor (SLNumber v) =
    return $
    qToPatternConstructor $
    ImpureFunction $ \v' ->
        if v == v'
            then Just ()
            else Nothing
interpretPatternConstructor (SLString v) =
    return $
    qToPatternConstructor $
    ImpureFunction $ \v' ->
        if v == v'
            then Just ()
            else Nothing
interpretPatternConstructor SLUnit = return $ qToPatternConstructor $ PureFunction $ \() -> ()
interpretPatternConstructor SLPair = return $ qToPatternConstructor $ PureFunction $ \(a :: A, b :: B) -> (a, (b, ()))

interpretPattern' :: SyntaxPattern' -> ScopeBuilder QPattern
interpretPattern' AnySyntaxPattern = return qAnyPattern
interpretPattern' (VarSyntaxPattern n) = do
    vid <- allocateVarScopeBuilder n
    return $ qVarPattern vid
interpretPattern' (BothSyntaxPattern spat1 spat2) = do
    pat1 <- interpretPattern spat1
    pat2 <- interpretPattern spat2
    lift $ liftRefNotation $ qBothPattern pat1 pat2
interpretPattern'  (ConstructorSyntaxPattern scons spats) = do
    pc <- lift $ liftRefNotation $ interpretPatternConstructor scons
    pats <- for spats interpretPattern
    pat@(MkSealedPattern (MkExpressionWitness tw vexpr) patw) <-
        lift $ liftRefNotation $ qConstructPattern pc pats
    return $
        case getOptGreatestDynamicSupertypeSW tw of
            Nothing -> pat
            Just stw -> let
                ff (MkMeetType (mt, r)) = do
                    t <- mt
                    return (MkMeetType (t, r))
                in MkSealedPattern (MkExpressionWitness stw vexpr) $ contramap1Pattern (ImpureFunction ff) patw
interpretPattern' (TypedSyntaxPattern spat stype) = do
    pat <- interpretPattern spat
    lift $
        liftRefNotation $ do
            mtn <- interpretType @'Negative stype
            case mtn of
                MkSome tn ->
                    case getOptGreatestDynamicSupertype tn of
                        Nothing -> return pat
                        Just dtn -> do
                            tpw <- invertType tn
                            let
                                pc :: QPatternConstructor
                                pc =
                                    toExpressionPatternConstructor $
                                    toPatternConstructor dtn (ConsListType tpw NilListType) $
                                    ImpureFunction $ fmap $ \a -> (a, ())
                            qConstructPattern pc [pat]
interpretPattern' (NamespaceSyntaxPattern _ _) = lift $ throw $ KnownIssueError 170 "NYI"

interpretPattern :: SyntaxPattern -> ScopeBuilder QPattern
interpretPattern (MkWithSourcePos spos pat) = do
    sourcePosScopeBuilder spos
    interpretPattern' pat


interpretExpression :: SyntaxExpression -> RefExpression
interpretExpression (MkWithSourcePos spos sexpr) = sourcePosRefNotation spos $ interpretExpression' sexpr

type DocSyntaxBinding = SyntaxWithDoc SyntaxBinding

dsbBinding :: DocSyntaxBinding -> SyntaxBinding
dsbBinding (MkSyntaxWithDoc _ b) = b

dsbName :: DocSyntaxBinding -> Name
dsbName dsb = case dsbBinding dsb of
    MkSyntaxBinding _ n _ -> n

dsbNode :: DocSyntaxBinding -> (DocSyntaxBinding, Name, [Name])
dsbNode db = (db, dsbName db, setToList $ bindingFreeVariables $ dsbBinding db)

-- | Group bindings into a topologically-sorted list of strongly-connected components
clumpBindings :: [DocSyntaxBinding] -> [[DocSyntaxBinding]]
clumpBindings bb = fmap flattenSCC $ stronglyConnComp $ fmap dsbNode bb

composeMap :: Ord b => Map b c -> Map a b -> Map a c
composeMap m = mapMaybe $ \k -> lookup k m

interpretRecursiveLetBindingsClump :: [DocSyntaxBinding] -> ScopeBuilder ()
interpretRecursiveLetBindingsClump sbinds = do
    items <-
        for sbinds $ \sbind -> do
            let name = dsbName sbind
            vid <- allocateVarScopeBuilder name
            return (name, vid, sbind)
    let nvmap = mapFromList $ fmap (\(name, vid, _) -> (name, vid)) items
    bl <- lift $ interpretBindings $ fmap (\(_, vid, sbind) -> (sbind, vid)) items
    interpScopeBuilder $ do
        bmap <- lift $ qUncheckedBindingsRecursiveLetExpr bl
        registerLetBindings (composeMap bmap nvmap)

interpretRecursiveLetBindingss :: [[DocSyntaxBinding]] -> ScopeBuilder ()
interpretRecursiveLetBindingss bb = for_ bb interpretRecursiveLetBindingsClump

interpretRecursiveLetBindings :: [DocSyntaxBinding] -> ScopeBuilder ()
interpretRecursiveLetBindings sbinds = do
    lift $ liftRefNotation $ checkSyntaxBindingsDuplicates $ fmap dsbBinding sbinds
    interpretRecursiveLetBindingss $ clumpBindings sbinds

interpretSequentialLetBinding ::  DocSyntaxBinding -> ScopeBuilder ()
interpretSequentialLetBinding sbind = do
    let n = dsbName sbind
    vid <- allocateVarScopeBuilder n
    b <- lift $ interpretBinding (sbind, vid)
    interpScopeBuilder $ do
        bmap <- lift $ qBindingSequentialLetExpr b
        registerLetBindings (composeMap bmap $ singletonMap n vid)

interpretRecursiveDocDeclarations :: [SyntaxRecursiveDeclaration] -> ScopeBuilder Docs
interpretRecursiveDocDeclarations ddecls = do
    let
        interp (MkSyntaxWithDoc doc (MkWithSourcePos spos decl)) =
            case decl of
                TypeSyntaxDeclaration  name defn -> let
                    diName = name
                    diParams =
                        case defn of
                            ClosedEntitySyntaxTypeDeclaration params _ -> fmap exprShow params
                            DatatypeSyntaxTypeDeclaration params _ -> fmap exprShow params
                            _ -> []
                    docItem = TypeDocItem {..}
                    docDescription = doc
                    in (pure (spos, name, doc, defn), mempty, mempty, pure $ EntryDocTreeEntry $ MkDefDoc {..})
                SubtypeSyntaxDeclaration  trustme sta stb mbody ->
                    ( mempty
                    , sourcePosScopeBuilder spos >> interpretSubtypeRelation doc trustme sta stb mbody
                    , mempty
                    , mempty)
                BindingSyntaxDeclaration sbind@(MkSyntaxBinding mtype name _) -> let
                    diName = name
                    diType =
                        case mtype of
                            Nothing -> ""
                            Just st -> exprShow st
                    docItem = ValueDocItem {..}
                    docDescription = doc
                    in (mempty, mempty, pure $ MkSyntaxWithDoc doc sbind, pure $ EntryDocTreeEntry $ MkDefDoc {..})
        (typeDecls, subtypeSB, bindingDecls, docDecls) = mconcat $ fmap interp ddecls
    interpScopeBuilder $ interpretRecursiveTypeDeclarations typeDecls
    stDocDecls <- subtypeSB
    interpretRecursiveLetBindings bindingDecls
    return $ stDocDecls <> docDecls

interpretExpose :: SyntaxExpose -> RefNotation (Docs -> Docs, QScope)
interpretExpose (SExpExpose spos names) = do
    scope <- liftRefNotation $ paramWith sourcePosParam spos $ exportScope names
    return (exposeDocs names, scope)
interpretExpose (SExpLet sdecls expose) =
    runScopeBuilder (interpretDocDeclarations sdecls) $ \olddoc -> do
        (restrict, scope) <- interpretExpose expose
        return (\newdoc -> restrict $ olddoc <> newdoc, scope)

interpretExposeDeclaration :: SyntaxExpose -> ScopeBuilder Docs
interpretExposeDeclaration sexp = do
    (docs, scope) <- lift $ interpretExpose sexp
    pureScopeBuilder scope
    return $ docs []

interpretImportDeclaration :: ModuleName -> QScopeInterpreter Docs
interpretImportDeclaration modname = do
    newmod <- lift $ getModule modname
    registerScope $ moduleScope newmod
    return [TreeDocTreeEntry $ moduleDoc newmod]

interpretDocDeclaration :: SyntaxDeclaration -> ScopeBuilder Docs
interpretDocDeclaration (MkSyntaxWithDoc doc (MkWithSourcePos spos decl)) = do
    sourcePosScopeBuilder spos
    case decl of
        ExposeSyntaxDeclaration sexp -> interpretExposeDeclaration sexp
        ImportSyntaxDeclaration  modname Nothing -> do
            interpScopeBuilder $ interpretImportDeclaration modname
        ImportSyntaxDeclaration  modname (Just names) -> do
            refScopeBuilder $
                runScopeBuilder (interpScopeBuilder $ interpretImportDeclaration modname) $ \docentries -> do
                    scope <- liftRefNotation $ exportScope names
                    return $ do
                        pureScopeBuilder scope
                        return (exposeDocs names docentries)
        DirectSyntaxDeclaration (TypeSyntaxDeclaration  name defn) -> do
            interpScopeBuilder $ interpretSequentialTypeDeclaration name doc defn
            let
                diName = name
                diParams =
                    case defn of
                        ClosedEntitySyntaxTypeDeclaration params _ -> fmap exprShow params
                        DatatypeSyntaxTypeDeclaration params _ -> fmap exprShow params
                        _ -> []
                docItem = TypeDocItem {..}
                docDescription = doc
            return $ defDocs MkDefDoc {..}
        DirectSyntaxDeclaration (SubtypeSyntaxDeclaration  trustme sta stb mbody) -> do
            interpretSubtypeRelation doc trustme sta stb mbody
        DirectSyntaxDeclaration (BindingSyntaxDeclaration sbind@(MkSyntaxBinding  mtype name _)) -> do
            interpretSequentialLetBinding $ MkSyntaxWithDoc doc sbind
            let
                diName = name
                diType =
                    case mtype of
                        Nothing -> ""
                        Just st -> exprShow st
                docItem = ValueDocItem {..}
                docDescription = doc
            return $ defDocs MkDefDoc {..}
        RecursiveSyntaxDeclaration  rdecls -> do
            interpretRecursiveDocDeclarations rdecls
        UsingSyntaxDeclaration _ -> lift $ throw $ KnownIssueError 170 "NYI"
        NamespaceSyntaxDeclaration _ _ -> lift $ throw $ KnownIssueError 170 "NYI"

interpretDocDeclarations :: [ SyntaxDeclaration] -> ScopeBuilder Docs
interpretDocDeclarations decls = mconcat $ fmap interpretDocDeclaration decls

interpretDeclarations :: [ SyntaxDeclaration] -> RefNotation --> RefNotation
interpretDeclarations decls ma = runScopeBuilder (interpretDocDeclarations decls) $ \_ -> ma

interpretNamedConstructor :: ReferenceName -> RefExpression
interpretNamedConstructor n = do
    me <- liftRefNotation $ lookupLetBinding n
    case me of
        Just (Right e) -> return e
        _ -> throw $ InterpretConstructorUnknownError n

interpretConstructor :: SyntaxConstructor -> RefExpression
interpretConstructor (SLNumber n) =
    return $
    case decode safeRationalNumber n of
        Just r ->
            case decode integerSafeRational r of
                Just i -> qConstExprAny $ jmToValue i
                Nothing -> qConstExprAny $ jmToValue r
        Nothing -> qConstExprAny $ jmToValue n
interpretConstructor (SLString v) = return $ qConstExprAny $ jmToValue v
interpretConstructor (SLNamedConstructor v) = interpretNamedConstructor v
interpretConstructor SLPair = return $ qConstExprAny $ jmToValue ((,) :: A -> B -> (A, B))
interpretConstructor SLUnit = return $ qConstExprAny $ jmToValue ()

specialFormArg :: QAnnotation t -> SyntaxAnnotation -> ComposeInner Maybe QInterpreter t
specialFormArg AnnotAnchor (SAAnchor anchor) = return anchor
specialFormArg AnnotNonpolarType (SAType st) = lift $ interpretNonpolarType st
specialFormArg AnnotPositiveType (SAType st) = lift $ interpretType @'Positive st
specialFormArg AnnotNegativeType (SAType st) = lift $ interpretType @'Negative st
specialFormArg _ _ = liftInner Nothing

specialFormArgs :: ListType QAnnotation lt -> [SyntaxAnnotation] -> ComposeInner Maybe QInterpreter (ListProduct lt)
specialFormArgs NilListType [] = return ()
specialFormArgs (ConsListType t tt) (a:aa) = do
    v <- specialFormArg t a
    vv <- specialFormArgs tt aa
    return (v, vv)
specialFormArgs _ _ = liftInner Nothing

showSA :: SyntaxAnnotation -> Text
showSA (SAType _) = "type"
showSA (SAAnchor _) = "anchor"

showAnnotation :: QAnnotation a -> Text
showAnnotation AnnotAnchor = "anchor"
showAnnotation AnnotNonpolarType = "type"
showAnnotation AnnotPositiveType = "type"
showAnnotation AnnotNegativeType = "type"

interpretSpecialForm :: ReferenceName -> NonEmpty SyntaxAnnotation -> QInterpreter QValue
interpretSpecialForm name annotations = do
    MkSpecialForm largs val <- lookupSpecialForm name
    margs <- unComposeInner $ specialFormArgs largs $ toList annotations
    case margs of
        Just args -> val args
        Nothing ->
            throw $
            SpecialFormWrongAnnotationsError
                name
                (listTypeToList showAnnotation largs)
                (fmap showSA $ toList annotations)

interpretConstant :: SyntaxConstant -> RefExpression
interpretConstant SCIfThenElse = return $ qConstExprAny $ jmToValue qifthenelse
interpretConstant SCBind = return $ qConstExprAny $ jmToValue qbind
interpretConstant SCBind_ = return $ qConstExprAny $ jmToValue qbind_
interpretConstant (SCConstructor lit) = interpretConstructor lit

interpretCase :: SyntaxMatch -> RefNotation (QPattern, QExpression)
interpretCase (MkSyntaxMatch spat sexpr) =
    runScopeBuilder (interpretPattern spat) $ \pat -> do
        expr <- interpretExpression sexpr
        return (pat, expr)

interpretMultimatch :: SyntaxMultimatch n -> RefNotation (FixedList n QPattern, QExpression)
interpretMultimatch (MkSyntaxMultimatch spats sexpr) =
    runScopeBuilder (for spats interpretPattern) $ \pats -> do
        expr <- interpretExpression sexpr
        return (pats, expr)

interpretExpression' :: SyntaxExpression' -> RefExpression
interpretExpression' (SESubsume sexpr stype) = do
    expr <- interpretExpression sexpr
    liftRefNotation $ do
        t <- interpretType stype
        qSubsumeExpr t expr
interpretExpression' (SEAbstract (MkSyntaxMatch spat sbody)) =
    runScopeBuilder (interpretPattern spat) $ \pat -> do
        val <- interpretExpression sbody
        liftRefNotation $ qCaseAbstract [(pat, val)]
interpretExpression' (SEAbstracts (MkSome multimatch)) = do
    (pats, expr) <- interpretMultimatch multimatch
    liftRefNotation $ qMultiCaseAbstract (fixedListLength pats) [(pats, expr)]
interpretExpression' (SELet sdecls sbody) = interpretDeclarations sdecls $ interpretExpression sbody
interpretExpression' (SEMatch scases) = do
    pairs <- for scases interpretCase
    liftRefNotation $ qCaseAbstract pairs
interpretExpression' (SEMatches (MkSyntaxMultimatchList nn scases)) = do
    pairs <- for scases interpretMultimatch
    liftRefNotation $ qMultiCaseAbstract nn pairs
interpretExpression' (SEApply sf sarg) = do
    f <- interpretExpression sf
    arg <- interpretExpression sarg
    liftRefNotation $ qApplyExpr f arg
interpretExpression' (SEConst c) = interpretConstant c
interpretExpression' (SEVar name) = liftRefNotation $ qName name
interpretExpression' (SESpecialForm name annots) =
    liftRefNotation $ do
        val <- interpretSpecialForm name annots
        return $ qConstExprAny val
interpretExpression' (SERef sexpr) = refNotationQuote $ interpretExpression sexpr
interpretExpression' (SEUnref sexpr) = refNotationUnquote $ interpretExpression sexpr
interpretExpression' (SEList sexprs) = do
    exprs <- for sexprs interpretExpression
    liftRefNotation $ qSequenceExpr exprs

checkExprVars :: MonadThrow PinaforeError m => QExpression -> m ()
checkExprVars (MkSealedExpression _ expr) = let
    getBadVarErrors ::
           forall w t. AllConstraint Show w
        => NameTypeWitness (UnitType VarID) (UnitType' w) t
        -> Maybe ErrorMessage
    getBadVarErrors w@(MkNameWitness (BadVarID spos _) _) =
        Just $ MkErrorMessage spos (ExpressionErrorError $ UndefinedBindingsError [show w]) mempty
    getBadVarErrors _ = Nothing
    errorMessages :: [ErrorMessage]
    errorMessages = catMaybes $ expressionFreeWitnesses getBadVarErrors expr
    in case errorMessages of
           [] -> return ()
           errs -> throw $ MkPinaforeError errs

interpretClosedExpression :: SyntaxExpression -> RefExpression
interpretClosedExpression sexpr = do
    expr <- interpretExpression sexpr
    checkExprVars expr
    return expr

interpretBinding :: (DocSyntaxBinding, VarID) -> RefNotation QBinding
interpretBinding ((MkSyntaxWithDoc doc  (MkSyntaxBinding  mstype _ sexpr)),vid) = do
    mtype <- liftRefNotation $ for mstype interpretType
    expr <- interpretClosedExpression sexpr
    return $ qBindExpr vid doc mtype expr

interpretBindings :: [(DocSyntaxBinding, VarID)] -> RefNotation [QBinding]
interpretBindings sbinds = for sbinds interpretBinding

interpretTopDeclarations :: SyntaxTopDeclarations -> QInterpreter --> QInterpreter
interpretTopDeclarations (MkSyntaxTopDeclarations spos sdecls) ma =
    paramWith sourcePosParam spos $ runRefNotation $ interpretDeclarations sdecls $ liftRefNotation ma

interpretTopExpression :: SyntaxExpression -> QInterpreter QExpression
interpretTopExpression sexpr = runRefNotation $ interpretExpression sexpr

interpretGeneralSubtypeRelation :: TrustOrVerify -> SyntaxType -> SyntaxType -> SyntaxExpression -> ScopeBuilder ()
interpretGeneralSubtypeRelation trustme sta stb sbody =
    interpScopeBuilder $ do
        ata <- lift $ interpretType @'Negative sta
        atb <- lift $ interpretType @'Positive stb
        case ata of
            MkSome ta@(dolanToMaybeType -> Just gta) ->
                case atb of
                    MkSome tb@(dolanToMaybeType -> Just gtb) -> do
                        let
                            funcWit :: QIsoShimWit 'Positive _
                            funcWit = funcShimWit (mkShimWit ta) (mkShimWit tb)
                        body <- lift $ interpretTopExpression sbody
                        case funcWit of
                            MkShimWit funcType iconv -> do
                                convexpr <- lift $ typedSubsumeExpressionToOpen funcType body
                                registerSubtypeConversion $
                                    subtypeConversionEntry trustme gta gtb $
                                    fmap
                                        (functionToShim "user-subtype" . (shimToFunction $ polarPolyIsoNegative iconv))
                                        convexpr
                    _ -> lift $ throw $ InterpretTypeNotGroundedError $ exprShow atb
            _ -> lift $ throw $ InterpretTypeNotGroundedError $ exprShow ata

nonpolarSimpleEntityType :: PinaforeNonpolarType t -> QInterpreter (QGroundType '[] t, EntityGroundType t)
nonpolarSimpleEntityType (GroundedNonpolarType t NilCCRArguments)
    | Just (NilListType, et) <- dolanToMonoGroundType t = return (t, et)
nonpolarSimpleEntityType t = throw $ InterpretTypeNotSimpleEntityError $ exprShow t

interpretOpenEntitySubtypeRelation :: SyntaxType -> SyntaxType -> ScopeBuilder ()
interpretOpenEntitySubtypeRelation sta stb =
    interpScopeBuilder $ do
        ata <- lift $ interpretNonpolarType sta
        atb <- lift $ interpretNonpolarType stb
        case (ata, atb) of
            (MkSome ta, MkSome tb) -> do
                (gta, tea@(MkEntityGroundType tfa _)) <- lift $ nonpolarSimpleEntityType ta
                (gtb, MkEntityGroundType tfb _) <- lift $ nonpolarSimpleEntityType tb
                case matchFamilyType openEntityFamilyWitness tfb of
                    Just (MkLiftedFamily _) ->
                        registerSubtypeConversion $
                        case matchFamilyType openEntityFamilyWitness tfa of
                            Just (MkLiftedFamily _) -> MkSubtypeConversionEntry Verify gta gtb coerceSubtypeConversion
                            Nothing ->
                                MkSubtypeConversionEntry TrustMe gta gtb $
                                nilSubtypeConversion $
                                coerceShim "open entity" .
                                (functionToShim "entityConvert" $
                                 entityAdapterConvert $ entityGroundTypeAdapter tea NilArguments)
                    Nothing -> lift $ throw $ InterpretTypeNotOpenEntityError $ exprShow tb

interpretSubtypeRelation ::
       Markdown -> TrustOrVerify -> SyntaxType -> SyntaxType -> Maybe SyntaxExpression -> ScopeBuilder Docs
interpretSubtypeRelation docDescription trustme sta stb mbody = do
    case mbody of
        Just body -> interpretGeneralSubtypeRelation trustme sta stb body
        Nothing -> interpretOpenEntitySubtypeRelation sta stb
    let
        diSubtype = exprShow sta
        diSupertype = exprShow stb
        docItem = SubtypeRelationDocItem {..}
    return $ defDocs MkDefDoc {..}

interpretModule :: ModuleName -> SyntaxModule -> QInterpreter QModule
interpretModule moduleName smod = do
    (docs, scope) <- runRefNotation $ interpretExpose smod
    return $ MkModule (MkDocTree (toText moduleName) "" $ docs []) scope
