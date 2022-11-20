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
import Pinafore.Language.Grammar.FreeVars
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

type instance EntryDoc QTypeSystem = DefDoc

interpretPatternConstructor :: SyntaxConstructor -> QInterpreter (Either QPatternConstructor QRecordPattern)
interpretPatternConstructor (SLNamedConstructor name) = lookupPatternConstructor name
interpretPatternConstructor (SLNumber v) =
    return $
    Left $
    qToPatternConstructor $
    ImpureFunction $ \v' ->
        if v == v'
            then Just ()
            else Nothing
interpretPatternConstructor (SLString v) =
    return $
    Left $
    qToPatternConstructor $
    ImpureFunction $ \v' ->
        if v == v'
            then Just ()
            else Nothing
interpretPatternConstructor SLUnit = return $ Left $ qToPatternConstructor $ PureFunction $ \() -> ()
interpretPatternConstructor SLPair =
    return $ Left $ qToPatternConstructor $ PureFunction $ \(a :: A, b :: B) -> (a, (b, ()))

recordNameWitnesses ::
       ListType (QSignature 'Positive) tt
    -> ScopeBuilder [SomeFor ((->) (ListVProduct tt)) (NameWitness VarID (QShimWit 'Positive))]
recordNameWitnesses lt =
    listTypeForList (pairListType lt $ listVProductGetters lt) $ \case
        MkPairType (ValueSignature name t) f -> do
            (_, vid) <- allocateVarScopeBuilder $ Just $ UnqualifiedFullNameRef name
            return $ MkSomeFor (MkNameWitness vid $ mkShimWit t) f

mkRecordPattern :: ListType (QSignature 'Positive) tt -> ScopeBuilder (QOpenPattern (Maybe (ListVProduct tt)) ())
mkRecordPattern ww = do
    sff <- recordNameWitnesses ww
    return $ MkPattern sff $ ImpureFunction $ fmap $ \a -> (a, ())

constructPattern :: Either QPatternConstructor QRecordPattern -> [QPattern] -> ScopeBuilder QPattern
constructPattern (Left pc) pats = lift $ liftRefNotation $ qConstructPattern pc pats
constructPattern (Right _) (_:_) = lift $ throw PatternTooManyConsArgsError
constructPattern (Right (MkRecordPattern sigs tt decode)) [] = do
    pat <- mkRecordPattern sigs
    return $ MkSealedPattern (MkExpressionWitness tt $ pure ()) $ pat . arr (decode . meet1)

interpretPattern' :: SyntaxPattern' -> ScopeBuilder QPattern
interpretPattern' AnySyntaxPattern = return qAnyPattern
interpretPattern' (VarSyntaxPattern n) = do
    (_, vid) <- allocateVarScopeBuilder $ Just $ UnqualifiedFullNameRef n
    return $ qVarPattern vid
interpretPattern' (BothSyntaxPattern spat1 spat2) = do
    pat1 <- interpretPattern spat1
    pat2 <- interpretPattern spat2
    lift $ liftRefNotation $ qBothPattern pat1 pat2
interpretPattern' (ConstructorSyntaxPattern scons spats) = do
    pc <- lift $ liftRefNotation $ interpretPatternConstructor scons
    pats <- for spats interpretPattern
    pat@(MkSealedPattern (MkExpressionWitness tw vexpr) patw) <- constructPattern pc pats
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
                MkSome tn -> do
                    tpw <- invertType tn
                    let
                        pc :: QPatternConstructor
                        pc =
                            toExpressionPatternConstructor $
                            toPatternConstructor (mkShimWit tn) (ConsListType tpw NilListType) $
                            PureFunction $ \a -> (a, ())
                    qConstructPattern pc [pat]
interpretPattern' (DynamicTypedSyntaxPattern spat stype) = do
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
interpretPattern' (NamespaceSyntaxPattern spat nref) = do
    close <- interpScopeBuilder $ withNamespace nref
    pat <- interpretPattern spat
    interpScopeBuilder close
    return pat

interpretPattern :: SyntaxPattern -> ScopeBuilder QPattern
interpretPattern (MkWithSourcePos spos pat) = do
    sourcePosScopeBuilder spos
    interpretPattern' pat

interpretExpression :: SyntaxExpression -> RefExpression
interpretExpression (MkWithSourcePos spos sexpr) = sourcePosRefNotation spos $ interpretExpression' sexpr

data SingleBinding = MkSingleBinding
    { sbName :: FullName
    , sbVarID :: VarID
    , sbType :: Maybe SyntaxType
    , sbBody :: SyntaxExpression
    , sbRefVars :: [FullName]
    , sbDoc :: Markdown
    }

sbDefDoc :: SingleBinding -> DefDoc
sbDefDoc MkSingleBinding {..} = let
    diName = sbName
    diType = fromMaybe "" $ fmap exprShow sbType
    docItem = ValueDocItem {..}
    docDescription = sbDoc
    in MkDefDoc {..}

buildSingleBinding ::
       Maybe FullNameRef -> Maybe SyntaxType -> SyntaxExpression -> Markdown -> ScopeBuilder (FullName, SingleBinding)
buildSingleBinding mname sbType sbBody sbDoc = do
    (sbName, sbVarID) <- allocateVarScopeBuilder mname
    nameResolve <- interpScopeBuilder getNameResolver
    let sbRefVars = fmap nameResolve $ syntaxExpressionFreeVariables sbBody
    return (sbName, MkSingleBinding {..})

typedSyntaxToSingleBindings ::
       SyntaxPattern -> Maybe SyntaxType -> SyntaxExpression -> Markdown -> ScopeBuilder [SingleBinding]
typedSyntaxToSingleBindings spat@(MkWithSourcePos spos pat) mstype sbody doc =
    case pat of
        VarSyntaxPattern name -> do
            (_, sb) <- buildSingleBinding (Just $ UnqualifiedFullNameRef name) mstype sbody doc
            return $ pure sb
        _ -> do
            (pvname, sb) <- buildSingleBinding Nothing mstype sbody doc
            let pvnameref = fullNameRef pvname
            sbs <-
                for (syntaxPatternBindingVariables spat) $ \pref -> let
                    wspos = MkWithSourcePos spos
                    funcsexpr = wspos $ SEAbstract $ MkSyntaxMatch spat $ wspos $ SEVar pref
                    valsexpr = wspos $ SEApply funcsexpr $ wspos $ SEVar pvnameref
                    in fmap snd $ buildSingleBinding (Just pref) Nothing valsexpr doc
            return $ sb : sbs

syntaxToSingleBindings :: SyntaxBinding -> Markdown -> ScopeBuilder [SingleBinding]
syntaxToSingleBindings (MkSyntaxBinding (MkWithSourcePos _ (TypedSyntaxPattern spat stype)) sbody) doc =
    typedSyntaxToSingleBindings spat (Just stype) sbody doc
syntaxToSingleBindings (MkSyntaxBinding spat sbody) doc = typedSyntaxToSingleBindings spat Nothing sbody doc

sbNode :: SingleBinding -> (SingleBinding, FullName, [FullName])
sbNode sb = (sb, sbName sb, sbRefVars sb)

-- | Group bindings into a topologically-sorted list of strongly-connected components
clumpBindings :: [SingleBinding] -> [[SingleBinding]]
clumpBindings sbb = fmap flattenSCC $ stronglyConnComp $ fmap sbNode sbb

mapVarID :: Map VarID (Markdown, expr) -> [(FullName, VarID)] -> [(FullNameRef, Markdown, expr)]
mapVarID mm =
    mapMaybe $ \(n, v) -> do
        (d, e) <- lookup v mm
        return (fullNameRef n, d, e)

interpretRecursiveLetBindingsClump :: [SingleBinding] -> ScopeBuilder ()
interpretRecursiveLetBindingsClump sbinds = do
    let nvs = fmap (\sb -> (sbName sb, sbVarID sb)) sbinds
    bl <- lift $ interpretBindings sbinds
    interpScopeBuilder $ do
        bmap <- lift $ qUncheckedBindingsRecursiveLetExpr bl
        registerLetBindings $ mapVarID bmap nvs

interpretRecursiveLetBindingss :: [[SingleBinding]] -> ScopeBuilder ()
interpretRecursiveLetBindingss bb = for_ bb interpretRecursiveLetBindingsClump

interpretRecursiveLetBindings :: [SingleBinding] -> ScopeBuilder ()
interpretRecursiveLetBindings sbinds = do
    case nonEmpty $ duplicates $ fmap sbName sbinds of
        Nothing -> return ()
        Just b -> lift $ throw $ InterpretBindingsDuplicateError b
    interpretRecursiveLetBindingss $ clumpBindings sbinds

interpretSequentialLetBinding :: SingleBinding -> ScopeBuilder ()
interpretSequentialLetBinding sbind = do
    b <- lift $ interpretBinding sbind
    interpScopeBuilder $ do
        bmap <- lift $ qBindingSequentialLetExpr b
        registerLetBindings $ mapVarID bmap $ pure (sbName sbind, sbVarID sbind)

interpretRecursiveDocDeclarations :: [SyntaxRecursiveDeclaration] -> ScopeBuilder Docs
interpretRecursiveDocDeclarations ddecls = do
    let
        interp (MkSyntaxWithDoc doc (MkWithSourcePos spos decl)) =
            case decl of
                TypeSyntaxDeclaration name defn -> let
                    diName = RootFullName name
                    diParams =
                        case defn of
                            ClosedEntitySyntaxTypeDeclaration params _ -> fmap exprShow params
                            DatatypeSyntaxTypeDeclaration params _ -> fmap exprShow params
                            _ -> []
                    docItem = TypeDocItem {..}
                    docDescription = doc
                    in return (pure (spos, name, doc, defn), mempty, mempty, pure $ EntryDocTreeEntry $ MkDefDoc {..})
                SubtypeSyntaxDeclaration trustme sta stb mbody ->
                    return
                        ( mempty
                        , sourcePosScopeBuilder spos >> interpretSubtypeRelation doc trustme sta stb mbody
                        , mempty
                        , mempty)
                BindingSyntaxDeclaration sbind -> do
                    binds <- syntaxToSingleBindings sbind doc
                    return (mempty, mempty, binds, fmap (EntryDocTreeEntry . sbDefDoc) binds)
    (typeDecls, subtypeSB, bindingDecls, docDecls) <- fmap mconcat $ for ddecls interp
    interpScopeBuilder $ interpretRecursiveTypeDeclarations typeDecls
    stDocDecls <- subtypeSB
    interpretRecursiveLetBindings bindingDecls
    return $ stDocDecls <> docDecls

partitionItem :: SyntaxExposeItem -> ([NamespaceRef], [FullNameRef])
partitionItem (NameSyntaxExposeItem n) = ([], [n])
partitionItem (NamespaceSyntaxExposeItem n) = ([n], [])

interpretExpose :: SyntaxExposeDeclaration -> RefNotation (Docs, QScope)
interpretExpose (MkSyntaxExposeDeclaration items sdecls) =
    runScopeBuilder (interpretDocDeclarations sdecls) $ \doc -> do
        let (namespaces, names) = mconcat $ fmap partitionItem items
        (bnames, scope) <- liftRefNotation $ exportScope namespaces names
        return (exposeDocs bnames doc, scope)

interpretImportDeclaration :: ModuleName -> QScopeInterpreter Docs
interpretImportDeclaration modname = do
    newmod <- lift $ getModule modname
    registerScope $ moduleScope newmod
    return [TreeDocTreeEntry $ moduleDoc newmod]

interpretDocDeclaration :: SyntaxDeclaration -> ScopeBuilder Docs
interpretDocDeclaration (MkSyntaxWithDoc doc (MkWithSourcePos spos decl)) = do
    sourcePosScopeBuilder spos
    case decl of
        ExposeSyntaxDeclaration expdecl ->
            refScopeBuilder $ do
                (docs, scope) <- interpretExpose expdecl
                return $ do
                    pureScopeBuilder scope
                    return docs
        ImportSyntaxDeclaration modname -> interpScopeBuilder $ interpretImportDeclaration modname
        DirectSyntaxDeclaration (TypeSyntaxDeclaration name defn) -> do
            interpScopeBuilder $ interpretSequentialTypeDeclaration name doc defn
            let
                diName = RootFullName name
                diParams =
                    case defn of
                        ClosedEntitySyntaxTypeDeclaration params _ -> fmap exprShow params
                        DatatypeSyntaxTypeDeclaration params _ -> fmap exprShow params
                        _ -> []
                docItem = TypeDocItem {..}
                docDescription = doc
            return $ defDocs MkDefDoc {..}
        DirectSyntaxDeclaration (SubtypeSyntaxDeclaration trustme sta stb mbody) ->
            interpretSubtypeRelation doc trustme sta stb mbody
        DirectSyntaxDeclaration (BindingSyntaxDeclaration sbind) -> do
            binds <- syntaxToSingleBindings sbind doc
            for_ binds interpretSequentialLetBinding
            return $ fmap (EntryDocTreeEntry . sbDefDoc) binds
        RecursiveSyntaxDeclaration rdecls -> interpretRecursiveDocDeclarations rdecls
        UsingSyntaxDeclaration nsn -> do
            interpScopeBuilder $ usingNamespace nsn
            return mempty
        NamespaceSyntaxDeclaration nsn decls -> do
            close <- interpScopeBuilder $ withNamespace nsn
            r <- interpretDocDeclarations decls
            interpScopeBuilder close
            return r

interpretDocDeclarations :: [SyntaxDeclaration] -> ScopeBuilder Docs
interpretDocDeclarations decls = mconcat $ fmap interpretDocDeclaration decls

interpretDeclarations :: [SyntaxDeclaration] -> RefNotation --> RefNotation
interpretDeclarations decls ma = runScopeBuilder (interpretDocDeclarations decls) $ \_ -> ma

interpretRecordConstructor :: QRecordConstructor -> RefExpression
interpretRecordConstructor (MkRecordConstructor items vtype conv) = do
    expr <-
        liftRefNotation $
        listTypeFor items $ \case
            ValueSignature iname itype -> do
                iexpr <- qName $ UnqualifiedFullNameRef iname
                typedSubsumeExpressionToOpen (freeTypeVariables vtype) itype iexpr
    return $ MkSealedExpression vtype $ fmap conv $ listVProductSequence $ listTypeToVType expr

interpretNamedConstructor :: FullNameRef -> RefExpression
interpretNamedConstructor n = do
    me <- liftRefNotation $ lookupLetBinding n
    case me of
        Just (ValueBoundValue e) -> return e
        Just (RecordBoundValue rc) -> interpretRecordConstructor rc
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

interpretSpecialForm :: FullNameRef -> NonEmpty SyntaxAnnotation -> QInterpreter QValue
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
        => NameWitness VarID w t
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

interpretBinding :: SingleBinding -> RefNotation QBinding
interpretBinding MkSingleBinding {..} = do
    mtype <- liftRefNotation $ for sbType interpretType
    expr <- interpretClosedExpression sbBody
    return $ qBindExpr sbVarID sbDoc mtype expr

interpretBindings :: [SingleBinding] -> RefNotation [QBinding]
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
                                convexpr <- lift $ typedSubsumeExpressionToOpen mempty funcType body
                                registerSubtypeConversion $
                                    subtypeConversionEntry trustme gta gtb $
                                    fmap
                                        (functionToShim "user-subtype" . (shimToFunction $ polarPolyIsoNegative iconv))
                                        convexpr
                    _ -> lift $ throw $ InterpretTypeNotGroundedError $ exprShow atb
            _ -> lift $ throw $ InterpretTypeNotGroundedError $ exprShow ata

nonpolarSimpleEntityType :: QNonpolarType t -> QInterpreter (QGroundType '[] t, EntityGroundType t)
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
    return $ MkModule (MkDocTree (toText moduleName) "" $ docs) scope
