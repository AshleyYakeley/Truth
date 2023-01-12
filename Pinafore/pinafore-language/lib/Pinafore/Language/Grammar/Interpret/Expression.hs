module Pinafore.Language.Grammar.Interpret.Expression
    ( interpretTopExpression
    , interpretModule
    , interpretTopDeclarations
    , interpretType
    , interpretImportDeclaration
    , interpretPattern
    ) where

import Data.Graph
import Pinafore.Base
import Pinafore.Language.Debug
import Pinafore.Language.DefDoc
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.FreeVars
import Pinafore.Language.Grammar.Interpret.RefNotation
import Pinafore.Language.Grammar.Interpret.ScopeBuilder
import Pinafore.Language.Grammar.Interpret.Type
import Pinafore.Language.Grammar.Interpret.TypeDecl
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.If
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Types
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
       Namespace
    -> ListType (QSignature 'Positive) tt
    -> QScopeInterpreter [SomeFor ((->) (ListVProduct tt)) (NameWitness VarID (QShimWit 'Positive))]
recordNameWitnesses ns lt =
    listTypeForList (pairListType lt $ listVProductGetters lt) $ \case
        MkPairType (ValueSignature name t) f -> do
            (_, vid) <- allocateVar $ Just $ MkFullName name ns
            return $ MkSomeFor (MkNameWitness vid $ mkShimWit t) f

mkRecordPattern ::
       Namespace -> ListType (QSignature 'Positive) tt -> QScopeInterpreter (QOpenPattern (Maybe (ListVProduct tt)) ())
mkRecordPattern ns ww = do
    sff <- recordNameWitnesses ns ww
    return $ MkPattern sff $ ImpureFunction $ fmap $ \a -> (a, ())

constructPattern :: Namespace -> Either QPatternConstructor QRecordPattern -> [QPattern] -> QScopeInterpreter QPattern
constructPattern _ (Left pc) pats = lift $ qConstructPattern pc pats
constructPattern _ (Right _) (_:_) = lift $ throw PatternTooManyConsArgsError
constructPattern ns (Right (MkRecordPattern sigs tt decode)) [] = do
    pat <- mkRecordPattern ns sigs
    return $ MkSealedPattern (MkExpressionWitness tt $ pure ()) $ pat . arr (decode . meet1)

interpretPattern' :: SyntaxPattern' -> QScopeInterpreter QPattern
interpretPattern' AnySyntaxPattern = return qAnyPattern
interpretPattern' (VarSyntaxPattern n) = do
    (_, vid) <- allocateVar $ Just n
    return $ qVarPattern vid
interpretPattern' (BothSyntaxPattern spat1 spat2) = do
    pat1 <- interpretPattern spat1
    pat2 <- interpretPattern spat2
    lift $ qBothPattern pat1 pat2
interpretPattern' (ConstructorSyntaxPattern ns scons spats) = do
    pc <- lift $ interpretPatternConstructor scons
    pats <- for spats interpretPattern
    pat@(MkSealedPattern (MkExpressionWitness tw vexpr) patw) <- constructPattern ns pc pats
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
    lift $ do
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
    lift $ do
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
interpretPattern' (NamespaceSyntaxPattern spat _) = interpretPattern spat

interpretPattern :: SyntaxPattern -> QScopeInterpreter QPattern
interpretPattern (MkWithSourcePos spos pat) = do
    scopeSourcePos spos
    interpretPattern' pat

interpretExpression :: SyntaxExpression -> RefExpression
interpretExpression (MkWithSourcePos spos sexpr) = sourcePosRefNotation spos $ interpretExpression' sexpr

data SingleBinding = MkSingleBinding
    { sbName :: FullName
    , sbVarID :: VarID
    , sbType :: Maybe SyntaxType
    , sbBody :: SyntaxExpression
    , sbRefVars :: [FullName]
    , sbDoc :: RawMarkdown
    }

sbDefDoc :: SingleBinding -> DefDoc
sbDefDoc MkSingleBinding {..} = let
    diName = fullNameRef sbName
    diType = fromMaybe "" $ fmap exprShow sbType
    docItem = ValueDocItem {..}
    docDescription = sbDoc
    in MkDefDoc {..}

buildSingleBinding ::
       Maybe FullName -> Maybe SyntaxType -> SyntaxExpression -> RawMarkdown -> ScopeBuilder (FullName, SingleBinding)
buildSingleBinding mname sbType sbBody sbDoc = do
    (sbName, sbVarID) <- allocateVarScopeBuilder mname
    let sbRefVars = syntaxExpressionFreeVariables sbBody
    return (sbName, MkSingleBinding {..})

typedSyntaxToSingleBindings ::
       SyntaxPattern -> Maybe SyntaxType -> SyntaxExpression -> RawMarkdown -> ScopeBuilder [SingleBinding]
typedSyntaxToSingleBindings spat@(MkWithSourcePos spos pat) mstype sbody doc =
    case pat of
        VarSyntaxPattern name -> do
            (_, sb) <- buildSingleBinding (Just name) mstype sbody doc
            return $ pure sb
        _ -> do
            (pvname, sb) <- buildSingleBinding Nothing mstype sbody doc
            sbs <-
                for (syntaxPatternBindingVariables spat) $ \pname -> let
                    wspos = MkWithSourcePos spos
                    funcsexpr = wspos $ SEAbstract $ MkSyntaxCase spat $ wspos $ SEVar RootNamespace $ fullNameRef pname
                    valsexpr = wspos $ SEApply funcsexpr $ wspos $ SEVar RootNamespace $ fullNameRef pvname
                    in fmap snd $ buildSingleBinding (Just pname) Nothing valsexpr doc
            return $ sb : sbs

syntaxToSingleBindings :: SyntaxBinding -> RawMarkdown -> ScopeBuilder [SingleBinding]
syntaxToSingleBindings (MkSyntaxBinding (MkWithSourcePos _ (TypedSyntaxPattern spat stype)) sbody) doc =
    typedSyntaxToSingleBindings spat (Just stype) sbody doc
syntaxToSingleBindings (MkSyntaxBinding spat sbody) doc = typedSyntaxToSingleBindings spat Nothing sbody doc

sbNode :: SingleBinding -> (SingleBinding, FullName, [FullName])
sbNode sb = (sb, sbName sb, sbRefVars sb)

-- | Group bindings into a topologically-sorted list of strongly-connected components
clumpBindings :: [SingleBinding] -> [[SingleBinding]]
clumpBindings sbb = fmap flattenSCC $ stronglyConnComp $ fmap sbNode sbb

mapVarID :: Map VarID (RawMarkdown, expr) -> [(FullName, VarID)] -> [(FullName, RawMarkdown, expr)]
mapVarID mm =
    mapMaybe $ \(n, v) -> do
        (d, e) <- lookup v mm
        return (n, d, e)

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

subtypeRelDocs :: SyntaxType -> SyntaxType -> RawMarkdown -> Docs
subtypeRelDocs sta stb docDescription = let
    diSubtype = exprShow sta
    diSupertype = exprShow stb
    docItem = SubtypeRelationDocItem {..}
    in pure $ pure MkDefDoc {..}

typeDeclDoc :: FullName -> SyntaxTypeDeclaration -> RawMarkdown -> Tree DefDoc
typeDeclDoc = let
    sigDoc :: SyntaxSignature -> DefDoc
    sigDoc (MkSyntaxWithDoc doc (MkWithSourcePos _ (ValueSyntaxSignature name stype))) =
        MkDefDoc (SignatureDocItem name $ exprShow stype) doc
    funcPNT :: PrecNamedText -> PrecNamedText -> PrecNamedText
    funcPNT ta tb = namedTextPrec 6 $ precNamedText 5 ta <> " -> " <> precNamedText 6 tb
    funcPNTList :: [PrecNamedText] -> PrecNamedText -> PrecNamedText
    funcPNTList [] t = t
    funcPNTList (a:aa) t = funcPNT a $ funcPNTList aa t
    consDoc :: FullName -> [NamedText] -> SyntaxConstructorOrSubtype extra -> (DocItem, [Tree DefDoc])
    consDoc tname _ (ConstructorSyntaxConstructorOrSubtype cname tt _) =
        ( ValuePatternDocItem (fullNameRef cname) $
          toNamedText $ funcPNTList (fmap exprShowPrec tt) (exprShowPrec tname)
        , [])
    consDoc _ tparams (SubtypeSyntaxConstructorOrSubtype tname tt) =
        (TypeDocItem (fullNameRef tname) tparams, typeConssDoc tname tparams tt)
    consDoc tname _ (RecordSyntaxConstructorOrSubtype cname sigs) =
        (ValuePatternDocItem (fullNameRef cname) (exprShow tname), fmap (pure . sigDoc) sigs)
    typeConsDoc :: FullName -> [NamedText] -> SyntaxWithDoc (SyntaxConstructorOrSubtype extra) -> Tree DefDoc
    typeConsDoc tname tparams (MkSyntaxWithDoc cdoc scs) = let
        (item, rest) = consDoc tname tparams scs
        in Node (MkDefDoc item cdoc) rest
    typeConssDoc :: FullName -> [NamedText] -> [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)] -> [Tree DefDoc]
    typeConssDoc tname tparams = fmap $ typeConsDoc tname tparams
    in \name defn doc -> let
           diName = fullNameRef name
           (diParams, items) =
               case defn of
                   StorableDatatypeSyntaxTypeDeclaration params conss -> let
                       tparams = fmap exprShow params
                       in (tparams, typeConssDoc name tparams conss)
                   PlainDatatypeSyntaxTypeDeclaration params conss -> let
                       tparams = fmap exprShow params
                       in (tparams, typeConssDoc name tparams conss)
                   _ -> mempty
           docItem = TypeDocItem {..}
           docDescription = doc
           in Node MkDefDoc {..} items

interpretRecursiveDocDeclarations :: [SyntaxRecursiveDeclaration] -> ScopeBuilder Docs
interpretRecursiveDocDeclarations ddecls = do
    let
        interp (MkSyntaxWithDoc doc (MkWithSourcePos spos decl)) =
            case decl of
                TypeSyntaxDeclaration name defn ->
                    return (pure (spos, name, doc, defn), mempty, mempty, pure $ typeDeclDoc name defn doc)
                SubtypeSyntaxDeclaration trustme sta stb mbody ->
                    return
                        ( mempty
                        , sourcePosScopeBuilder spos >> interpretSubtypeRelation trustme sta stb mbody
                        , mempty
                        , subtypeRelDocs sta stb doc)
                BindingSyntaxDeclaration sbind -> do
                    binds <- syntaxToSingleBindings sbind doc
                    return (mempty, mempty, binds, fmap (pure . sbDefDoc) binds)
    (typeDecls, subtypeSB, bindingDecls, docDecls) <- fmap mconcat $ for ddecls interp
    interpScopeBuilder $ interpretRecursiveTypeDeclarations typeDecls
    subtypeSB
    interpretRecursiveLetBindings bindingDecls
    return docDecls

partitionItem :: Namespace -> SyntaxNameRefItem -> ([Namespace], [FullNameRef])
partitionItem _ (NameSyntaxNameRefItem n) = ([], [n])
partitionItem curns (NamespaceSyntaxNameRefItem n) = ([namespaceConcatRef curns n], [])

interpretExpose :: SyntaxExposeDeclaration -> RefNotation (Docs, QScope)
interpretExpose (MkSyntaxExposeDeclaration items sdecls) =
    runScopeBuilder (interpretDocDeclarations sdecls) $ \doc ->
        liftRefNotation $ do
            curns <- getCurrentNamespace
            let (namespaces, names) = mconcat $ fmap (partitionItem curns) items
            (bnames, scope) <- exportScope namespaces names
            return (exposeDocs bnames doc, scope)

interpretImportDeclaration :: ModuleName -> QScopeInterpreter Docs
interpretImportDeclaration modname = do
    newmod <- lift $ getModule modname
    registerScope $ moduleScope newmod
    return [moduleDoc newmod]

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
            return $ pure $ typeDeclDoc name defn doc
        DirectSyntaxDeclaration (SubtypeSyntaxDeclaration trustme sta stb mbody) -> do
            interpretSubtypeRelation trustme sta stb mbody
            return $ subtypeRelDocs sta stb doc
        DirectSyntaxDeclaration (BindingSyntaxDeclaration sbind) -> do
            binds <- syntaxToSingleBindings sbind doc
            for_ binds interpretSequentialLetBinding
            return $ fmap (pure . sbDefDoc) binds
        RecursiveSyntaxDeclaration rdecls -> interpretRecursiveDocDeclarations rdecls
        UsingSyntaxDeclaration sourcens mitems destns -> do
            let
                matchItem :: FullNameRef -> SyntaxNameRefItem -> Bool
                matchItem name (NameSyntaxNameRefItem name') = name == name'
                matchItem (MkFullNameRef _ nsr') (NamespaceSyntaxNameRefItem nsr) = let
                    ns = namespaceConcatRef sourcens nsr
                    ns' = namespaceConcatRef sourcens nsr'
                    in isJust $ namespaceWithinRef ns ns'
            interpScopeBuilder $
                usingNamespace sourcens destns $
                case mitems of
                    Nothing -> \_ -> True
                    Just (b, items) -> \name -> any (matchItem name) items == b
            return mempty
        NamespaceSyntaxDeclaration nsn decls -> do
            close <- interpScopeBuilder $ withNamespace nsn
            docs <- interpretDocDeclarations decls
            interpScopeBuilder close
            return $ pure $ Node (MkDefDoc (NamespaceDocItem $ AbsoluteNamespaceRef nsn) doc) docs
        DebugSyntaxDeclaration nameref -> do
            (fn, desc) <- interpScopeBuilder $ lift $ lookupDebugBindingInfo nameref
            liftIO $ debugMessage $ toText fn <> ": " <> pack desc
            return []

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
    me <- liftRefNotation lookupLetBinding
    case me n of
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

withMatch :: QMatch -> RefNotation QPartialExpression -> RefNotation QPartialExpression
withMatch match mexp =
    unTransformT (interpScopeBuilder $ registerMatchBindings match) $ \() -> do
        exp <- mexp
        liftRefNotation $ qMatchGate match exp

withAllocateNewVar :: Maybe FullName -> (VarID -> RefNotation a) -> RefNotation a
withAllocateNewVar mn call = runScopeBuilder (allocateVarScopeBuilder mn) $ \(_, varid) -> call varid

intepretLambdaMatch :: VarID -> SyntaxPattern -> QScopeInterpreter QMatch
intepretLambdaMatch varid spat = do
    pat <- interpretPattern spat
    return $ qLambdaPatternMatch varid pat

interpretSinglePattern :: VarID -> SyntaxPattern -> RefNotation QPartialExpression -> RefNotation QPartialExpression
interpretSinglePattern varid spat mexp =
    unTransformT (interpScopeBuilder $ intepretLambdaMatch varid spat) $ \pat -> withMatch pat mexp

interpretCase :: VarID -> SyntaxCase -> RefNotation QPartialExpression
interpretCase varid (MkSyntaxCase spat sbody) =
    interpretSinglePattern varid spat $ fmap sealedToPartialExpression $ interpretExpression sbody

specialCaseVarPatterns :: Bool
specialCaseVarPatterns = True

interpretMultiPattern ::
       FixedList n VarID
    -> FixedList n SyntaxPattern
    -> RefNotation QPartialExpression
    -> RefNotation QPartialExpression
interpretMultiPattern NilFixedList NilFixedList ma = ma
interpretMultiPattern (ConsFixedList v vv) (ConsFixedList spat spats) ma =
    interpretSinglePattern v spat $ interpretMultiPattern vv spats ma

interpretMulticase :: FixedList n VarID -> SyntaxMulticase n -> RefNotation QPartialExpression
interpretMulticase varids (MkSyntaxMulticase spats sbody) =
    interpretMultiPattern varids spats $ fmap sealedToPartialExpression $ interpretExpression sbody

multiAbstractExpr :: FixedList n VarID -> QExpression -> QInterpreter QExpression
multiAbstractExpr NilFixedList exp = return exp
multiAbstractExpr (ConsFixedList v vv) exp = do
    exp' <- multiAbstractExpr vv exp
    qAbstractExpr v exp'

interpretExpression' :: SyntaxExpression' -> RefExpression
interpretExpression' (SESubsume sexpr stype) = do
    expr <- interpretExpression sexpr
    liftRefNotation $ do
        t <- interpretType stype
        qSubsumeExpr t expr
interpretExpression' (SEAbstract (MkSyntaxCase (MkWithSourcePos _ AnySyntaxPattern) sbody))
    | specialCaseVarPatterns =
        withAllocateNewVar Nothing $ \varid -> do
            expr <- interpretExpression sbody
            liftRefNotation $ qAbstractExpr varid expr
interpretExpression' (SEAbstract (MkSyntaxCase (MkWithSourcePos _ (VarSyntaxPattern n)) sbody))
    | specialCaseVarPatterns =
        withAllocateNewVar (Just n) $ \varid -> do
            expr <- interpretExpression sbody
            liftRefNotation $ qAbstractExpr varid expr
interpretExpression' (SEAbstract match) =
    withAllocateNewVar Nothing $ \varid -> do
        pexpr <- interpretCase varid match
        liftRefNotation $ qAbstractExpr varid $ partialToSealedExpression pexpr
interpretExpression' (SEAbstracts (MkSome multimatch@(MkSyntaxMulticase spats _))) =
    runScopeBuilder (for spats $ \_ -> fmap snd $ allocateVarScopeBuilder Nothing) $ \varids -> do
        pexpr <- interpretMulticase varids multimatch
        liftRefNotation $ multiAbstractExpr varids $ partialToSealedExpression pexpr
interpretExpression' (SELet sdecls sbody) = interpretDeclarations sdecls $ interpretExpression sbody
interpretExpression' (SEMatch scases) =
    withAllocateNewVar Nothing $ \varid -> do
        pexprs <- for scases $ interpretCase varid
        pexpr <- liftRefNotation $ qPartialExpressionSumList pexprs
        liftRefNotation $ qAbstractExpr varid $ partialToSealedExpression pexpr
interpretExpression' (SEMatches (MkSyntaxMulticaseList nn scases)) =
    runScopeBuilder (fixedListGenerate nn $ fmap snd $ allocateVarScopeBuilder Nothing) $ \varids -> do
        pexprs <- for scases $ interpretMulticase varids
        pexpr <- liftRefNotation $ qPartialExpressionSumList pexprs
        liftRefNotation $ multiAbstractExpr varids $ partialToSealedExpression pexpr
interpretExpression' (SEApply sf sarg) = do
    f <- interpretExpression sf
    arg <- interpretExpression sarg
    liftRefNotation $ qApplyExpr f arg
interpretExpression' (SEConst c) = interpretConstant c
interpretExpression' (SEVar _ name) = liftRefNotation $ qName name
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
                    _ -> lift $ throwWithName $ \ntt -> InterpretTypeNotGroundedError $ ntt $ exprShow atb
            _ -> lift $ throwWithName $ \ntt -> InterpretTypeNotGroundedError $ ntt $ exprShow ata

nonpolarSimpleEntityType :: QNonpolarType t -> QInterpreter (QGroundType '[] t, EntityGroundType t)
nonpolarSimpleEntityType (GroundedNonpolarType t NilCCRArguments)
    | Just (NilListType, et) <- dolanToMonoGroundType t = return (t, et)
nonpolarSimpleEntityType t = throwWithName $ \ntt -> InterpretTypeNotSimpleEntityError $ ntt $ exprShow t

interpretOpenEntitySubtypeRelation :: SyntaxType -> SyntaxType -> ScopeBuilder ()
interpretOpenEntitySubtypeRelation sta stb =
    interpScopeBuilder $ do
        ata <- lift $ interpretNonpolarType sta
        atb <- lift $ interpretNonpolarType stb
        case (ata, atb) of
            (MkSome ta, MkSome tb) -> do
                (gta, tea@(MkEntityGroundType tfa _)) <- lift $ nonpolarSimpleEntityType ta
                (gtb, MkEntityGroundType tfb _) <- lift $ nonpolarSimpleEntityType tb
                case matchFamilyType openStorableFamilyWitness tfb of
                    Just (MkLiftedFamily _) ->
                        registerSubtypeConversion $
                        case matchFamilyType openStorableFamilyWitness tfa of
                            Just (MkLiftedFamily _) -> MkSubtypeConversionEntry Verify gta gtb coerceSubtypeConversion
                            Nothing ->
                                MkSubtypeConversionEntry TrustMe gta gtb $
                                nilSubtypeConversion $
                                coerceShim "open entity" .
                                (functionToShim "entityConvert" $
                                 storeAdapterConvert $ entityGroundTypeAdapter tea NilArguments)
                    Nothing -> lift $ throwWithName $ \ntt -> InterpretTypeNotOpenEntityError $ ntt $ exprShow tb

interpretSubtypeRelation :: TrustOrVerify -> SyntaxType -> SyntaxType -> Maybe SyntaxExpression -> ScopeBuilder ()
interpretSubtypeRelation trustme sta stb mbody =
    case mbody of
        Just body -> interpretGeneralSubtypeRelation trustme sta stb body
        Nothing -> interpretOpenEntitySubtypeRelation sta stb

interpretModule :: ModuleName -> SyntaxModule -> QInterpreter QModule
interpretModule moduleName smod = do
    (docs, scope) <- runRefNotation $ interpretExpose smod
    return $ MkModule (Node (MkDefDoc (HeadingDocItem (plainText $ toText moduleName)) "") docs) scope
