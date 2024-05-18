module Pinafore.Language.Grammar.Interpret.Expression
    ( interpretExpression
    , interpretModule
    , interpretDeclarationWith
    , interpretType
    , interpretImportDeclaration
    , interpretPattern
    ) where

import Data.Graph hiding (Forest, Tree)
import Pinafore.Base
import Pinafore.Language.Debug
import Pinafore.Language.DefDoc
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.FreeVars
import Pinafore.Language.Grammar.Interpret.AppNotation
import Pinafore.Language.Grammar.Interpret.Type
import Pinafore.Language.Grammar.Interpret.TypeDecl
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Grammar.SyntaxDoc
import Pinafore.Language.If
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Types
import Pinafore.Language.Name
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Pinafore.Language.Var
import Pinafore.Language.VarID
import Pinafore.Text
import Shapes

interpretPatternConstructor :: SyntaxConstructor -> QInterpreter (Either QPatternConstructor QRecordConstructor)
interpretPatternConstructor (SLNamedConstructor name _) = lookupPatternConstructor name
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
    -> QScopeBuilder [SomeFor ((->) (ListVProduct tt)) (NameWitness VarID (QShimWit 'Positive))]
recordNameWitnesses ns lt =
    listTypeForList (pairListType lt $ listVProductGetters lt) $ \case
        MkPairType (ValueSignature _ name t _) f -> do
            (_, vid) <- allocateVar $ Just $ MkFullName name ns
            return $ MkSomeFor (MkNameWitness vid $ mkShimWit t) f

mkRecordPattern ::
       Namespace -> ListVType (QSignature 'Positive) tt -> QScopeBuilder (QOpenPattern (Maybe (ListVProduct tt)) ())
mkRecordPattern ns ww = do
    sff <- recordNameWitnesses ns $ listVTypeToType ww
    return $ MkPattern sff $ ImpureFunction $ fmap $ \a -> (a, ())

constructPattern :: Namespace -> Either QPatternConstructor QRecordConstructor -> [QPattern] -> QScopeBuilder QPattern
constructPattern _ (Left pc) pats = builderLift $ qConstructPattern pc pats
constructPattern _ (Right _) (_:_) = throw PatternTooManyConsArgsError
constructPattern ns (Right (MkQRecordConstructor sigs _ tt codec)) [] = do
    pat <- mkRecordPattern ns sigs
    return $ MkSealedPattern (MkExpressionWitness (shimWitToDolan tt) $ pure ()) $ pat . arr (decode codec . meet1)

interpretPattern' :: SyntaxPattern' -> QScopeBuilder QPattern
interpretPattern' AnySyntaxPattern = return qAnyPattern
interpretPattern' (VarSyntaxPattern n) = do
    (_, vid) <- allocateVar $ Just n
    return $ qVarPattern vid
interpretPattern' (BothSyntaxPattern spat1 spat2) = do
    pat1 <- interpretPattern spat1
    pat2 <- interpretPattern spat2
    builderLift $ qBothPattern pat1 pat2
interpretPattern' (ConstructorSyntaxPattern ns scons spats) = do
    pc <- builderLift $ interpretPatternConstructor scons
    pats <- for spats interpretPattern
    pat@(MkSealedPattern (MkExpressionWitness tw vexpr) patw) <- constructPattern ns pc pats
    mstw <- builderLift $ getOptGreatestDynamicSupertypeSW tw
    return $
        case mstw of
            Nothing -> pat
            Just stw -> let
                ff (MkMeetType (mt, r)) = do
                    t <- mt
                    return (MkMeetType (t, r))
                in MkSealedPattern (MkExpressionWitness stw vexpr) $ contramap1Pattern (ImpureFunction ff) patw
interpretPattern' (TypedSyntaxPattern spat stype) = do
    pat <- interpretPattern spat
    builderLift $ do
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
    builderLift $ do
        mtn <- interpretType @'Negative stype
        case mtn of
            MkSome tn -> do
                mdtn <- getOptGreatestDynamicSupertype tn
                case mdtn of
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
interpretPattern' (DebugSyntaxPattern t spat) = do
    pat <- interpretPattern spat
    liftIO $ debugMessage $ t <> ": " <> pack (show pat)
    return pat

interpretPattern :: SyntaxPattern -> QScopeBuilder QPattern
interpretPattern (MkWithSourcePos spos pat) = do
    scopeSetSourcePos spos
    interpretPattern' pat

interpretExpression :: SyntaxExpression -> QInterpreter QExpression
interpretExpression (MkWithSourcePos spos sexpr) = paramWith sourcePosParam spos $ interpretExpression' sexpr

getPatternBindingVariables :: SyntaxPattern -> QInterpreter [FullName]
getPatternBindingVariables spat = do
    let
        (names, cs) = syntaxPatternBindingVariables spat
        signatureName :: forall a. Namespace -> QSignature 'Positive a -> FullName
        signatureName ns (ValueSignature _ name _ _) = MkFullName name ns
    namess <-
        for cs $ \(cns, cref) -> do
            pres <- lookupPatternConstructor cref
            case pres of
                Left _ -> return []
                Right (MkQRecordConstructor lt _ _ _) -> return $ listVTypeToList (signatureName cns) lt
    return $ names <> mconcat namess

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
    docItem = valueDocItem sbName sbType
    docDescription = sbDoc
    in MkDefDoc {..}

-- isRecursive flag is to fix issue #199
buildSingleBinding ::
       Maybe (FullName, Bool)
    -> Maybe SyntaxType
    -> SyntaxExpression
    -> RawMarkdown
    -> QScopeBuilder (FullName, SingleBinding)
buildSingleBinding mname sbType sbBody sbDoc = do
    (sbName, sbVarID) <-
        case mname of
            Nothing -> allocateVar Nothing
            Just (name, True) -> allocateVar $ Just name
            Just (name, False) -> do
                (_, varID) <- allocateVar Nothing
                return (name, varID)
    let sbRefVars = syntaxExpressionFreeVariables sbBody
    return (sbName, MkSingleBinding {..})

typedSyntaxToSingleBindings ::
       Bool -> SyntaxPattern -> Maybe SyntaxType -> SyntaxExpression -> RawMarkdown -> QScopeBuilder [SingleBinding]
typedSyntaxToSingleBindings isRecursive spat@(MkWithSourcePos spos pat) mstype sbody doc =
    case pat of
        VarSyntaxPattern name -> do
            (_, sb) <- buildSingleBinding (Just (name, isRecursive)) mstype sbody doc
            return $ pure sb
        _ -> do
            (pvname, sb) <- buildSingleBinding Nothing mstype sbody doc
            bvars <- builderLift $ getPatternBindingVariables spat
            sbs <-
                for bvars $ \pname -> let
                    wspos = MkWithSourcePos spos
                    funcsexpr = wspos $ SEAbstract $ MkSyntaxCase spat $ wspos $ SEVar RootNamespace $ fullNameRef pname
                    valsexpr = wspos $ SEApply funcsexpr $ wspos $ SEVar RootNamespace $ fullNameRef pvname
                    in fmap snd $ buildSingleBinding (Just (pname, isRecursive)) Nothing valsexpr doc
            return $ sb : sbs

syntaxToSingleBindings :: Bool -> SyntaxBinding -> RawMarkdown -> QScopeBuilder [SingleBinding]
syntaxToSingleBindings isRecursive (MkSyntaxBinding (MkWithSourcePos _ (TypedSyntaxPattern spat stype)) sbody) doc =
    typedSyntaxToSingleBindings isRecursive spat (Just stype) sbody doc
syntaxToSingleBindings isRecursive (MkSyntaxBinding spat sbody) doc =
    typedSyntaxToSingleBindings isRecursive spat Nothing sbody doc

sbNode :: SingleBinding -> (SingleBinding, FullName, [FullName])
sbNode sb = (sb, sbName sb, sbRefVars sb)

-- | Group bindings into a topologically-sorted list of strongly-connected components
clumpBindings :: [SingleBinding] -> [[SingleBinding]]
clumpBindings sbb = fmap flattenSCC $ stronglyConnComp $ fmap sbNode sbb

mapVarID :: Map VarID (DefDoc, expr) -> [(FullName, VarID)] -> [(FullName, DefDoc, expr)]
mapVarID mm =
    mapMaybe $ \(n, v) -> do
        (d, e) <- lookup v mm
        return (n, d, e)

interpretRecursiveLetBindingsClump :: [SingleBinding] -> QScopeBuilder ()
interpretRecursiveLetBindingsClump sbinds = do
    bmap <-
        builderLift $ do
            bl <- for sbinds interpretBinding
            qUncheckedBindingsRecursiveLetExpr bl
    let nvs = fmap (\sb -> (sbName sb, sbVarID sb)) sbinds
    registerLetBindings $ mapVarID bmap nvs

interpretRecursiveLetBindings :: [SingleBinding] -> QScopeBuilder ()
interpretRecursiveLetBindings sbinds = do
    case nonEmpty $ duplicates $ fmap sbName sbinds of
        Nothing -> return ()
        Just b -> throw $ InterpretBindingsDuplicateError b
    for_ (clumpBindings sbinds) interpretRecursiveLetBindingsClump
    for_ sbinds $ \sbind -> registerDocs $ pure $ sbDefDoc sbind

interpretSequentialLetBinding :: SingleBinding -> QScopeBuilder ()
interpretSequentialLetBinding sbind = do
    b <- builderLift $ interpretBinding sbind
    bmap <- builderLift $ qBindingSequentialLetExpr b
    registerLetBindings $ mapVarID bmap $ pure (sbName sbind, sbVarID sbind)
    registerDocs $ pure $ sbDefDoc sbind

interpretRecursiveDocDeclarations :: [SyntaxRecursiveDeclaration] -> QScopeBuilder ()
interpretRecursiveDocDeclarations ddecls = do
    let
        interp :: SyntaxRecursiveDeclaration -> QScopeBuilder (_, QScopeBuilder (), [SingleBinding])
        interp (MkSyntaxWithDoc doc (MkWithSourcePos spos decl)) =
            case decl of
                TypeSyntaxDeclaration name defn -> return (pure (spos, name, doc, defn), mempty, mempty)
                SubtypeSyntaxDeclaration trustme sta stb mbody ->
                    return
                        (mempty, scopeSetSourcePos spos >> interpretSubtypeRelation trustme sta stb mbody doc, mempty)
                BindingSyntaxDeclaration sbind -> do
                    binds <- syntaxToSingleBindings True sbind doc
                    return (mempty, mempty, binds)
    (typeDecls, subtypeSB, bindingDecls) <- fmap mconcat $ for ddecls interp
    let
        ?interpretExpression = interpretExpression
        in interpretRecursiveTypeDeclarations typeDecls
    subtypeSB
    interpretRecursiveLetBindings bindingDecls

interpretImportDeclaration :: ModuleName -> QInterpreter QScopeDocs
interpretImportDeclaration modname = do
    MkQModule {..} <- getModule modname
    scope <-
        case moduleGivens of
            [] -> return moduleScope
            givens -> do
                substs <-
                    for givens $ \(name, varid) -> do
                        expr <- qName $ fullNameRef name
                        return (varid, expr)
                let
                    substM :: VarID -> Maybe QExpression
                    substM v = lookup v substs
                unEndoM
                    (traverseExpressionsM @QTypeSystem (MkEndoM $ qSubstitute substM) (MkEndoM $ qOpenSubstitute substM))
                    moduleScope
    return $ MkQScopeDocs [scope] moduleDoc

interpretDeclaration :: SyntaxDeclaration -> QScopeBuilder ()
interpretDeclaration (MkSyntaxWithDoc doc (MkWithSourcePos spos decl)) = do
    scopeSetSourcePos spos
    case decl of
        DirectSyntaxDeclaration (TypeSyntaxDeclaration name defn) -> let
            ?interpretExpression = interpretExpression
            in interpretSequentialTypeDeclaration name doc defn
        DirectSyntaxDeclaration (SubtypeSyntaxDeclaration trustme sta stb mbody) -> do
            interpretSubtypeRelation trustme sta stb mbody doc
        DirectSyntaxDeclaration (BindingSyntaxDeclaration sbind) -> do
            binds <- syntaxToSingleBindings False sbind doc
            for_ binds interpretSequentialLetBinding
        DeclaratorSyntaxDeclaration declarator -> do
            sd <- builderLift $ interpretDeclarator declarator
            registerScopeDocs sd
        DeclaratorInSyntaxDeclaration declarator declaration -> do
            sd <- builderLift $ interpretDeclaratorWith declarator $ runScopeBuilder_ $ interpretDeclaration declaration
            registerScopeDocs sd
        ExposeDeclaration items -> do
            sd <- builderLift $ interpretExpose items
            outputScopeDocs sd
        NamespaceSyntaxDeclaration nsn decls ->
            withCurrentNamespaceScope nsn $
            prodCensor
                builderDocsProd
                (\docs -> pureForest $ MkTree (MkDefDoc (HeadingDocItem $ plainText $ showText nsn) doc) docs) $
            for_ decls interpretDeclaration
        DebugSyntaxDeclaration nameref -> do
            mfd <- builderLift $ lookupDebugBindingInfo nameref
            liftIO $
                debugMessage $
                case mfd of
                    Just (fn, desc) -> showText nameref <> " = " <> showText fn <> ": " <> pack desc
                    Nothing -> showText nameref <> " not found"

interpretRecordConstructor :: QRecordConstructor -> Maybe [(Name, SyntaxExpression)] -> QInterpreter QExpression
interpretRecordConstructor (MkQRecordConstructor items vtype _ codec) msarglist = do
    let
        getsigname :: forall a. QSignature 'Positive a -> Maybe Name
        getsigname (ValueSignature _ name _ _) = Just name
        signames :: [Name]
        signames = catMaybes $ listVTypeToList getsigname items
    margmap :: Maybe (Map Name QExpression) <-
        for msarglist $ \sarglist -> do
            arglist <-
                for sarglist $ \(n, sv) -> do
                    if elem n signames
                        then return ()
                        else throw $ RecordConstructorExtraName n
                    v <- interpretExpression sv
                    return (n, v)
            return $ mapFromList arglist
    let
        freeFixedVars = freeTypeVariables vtype
        freeFixedNames = fmap someTypeVarName $ toList freeFixedVars
        getName :: Maybe QExpression -> Name -> QInterpreter QExpression
        getName =
            case margmap of
                Nothing -> \mdefexpr iname -> qNameWithDefault mdefexpr $ UnqualifiedFullNameRef iname
                Just argmap ->
                    \mdefexpr iname ->
                        case lookup iname argmap of
                            Just arg -> return arg
                            Nothing ->
                                case mdefexpr of
                                    Just defexpr -> return defexpr
                                    Nothing -> throw $ RecordConstructorMissingName iname
    runRenamer @QTypeSystem [] freeFixedNames $ do
        sexpr <-
            listVTypeFor items $ \case
                ValueSignature _ iname itype mdefault -> do
                    iexpr <- lift $ getName (fmap (MkSealedExpression (mkShimWit itype)) mdefault) iname
                    itype' <- unEndoM (renameType @QTypeSystem freeFixedNames RigidName) itype
                    iexpr' <- renameMappable @QTypeSystem [] FreeName iexpr
                    subsumerExpressionTo @QTypeSystem itype' iexpr'
        (resultExpr, ssubs) <- solveSubsumerExpression @QTypeSystem $ listVProductSequence sexpr
        unEndoM (subsumerSubstitute @QTypeSystem ssubs) $
            MkSealedExpression (shimWitToDolan vtype) $ fmap (encode codec) resultExpr

interpretNamedConstructor :: FullNameRef -> Maybe [(Name, SyntaxExpression)] -> QInterpreter QExpression
interpretNamedConstructor name mvals = do
    bv <- lookupBoundValue name
    case (bv, mvals) of
        (ValueBoundValue e, Nothing) -> return e
        (ValueBoundValue _, Just _) -> throw $ LookupNotRecordConstructorError name
        (RecordBoundValue rc, _) -> interpretRecordConstructor rc mvals

interpretConstructor :: SyntaxConstructor -> QInterpreter QExpression
interpretConstructor (SLNumber n) =
    return $
    case decode safeRationalNumber n of
        Just r ->
            case decode integerSafeRational r of
                Just i -> qConstExprAny $ jmToValue i
                Nothing -> qConstExprAny $ jmToValue r
        Nothing -> qConstExprAny $ jmToValue n
interpretConstructor (SLString v) = return $ qConstExprAny $ jmToValue v
interpretConstructor (SLNamedConstructor v mvals) = interpretNamedConstructor v mvals
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

showSA :: SyntaxAnnotation -> NamedText
showSA (SAType _) = "type"
showSA (SAAnchor _) = "anchor"

showAnnotation :: QAnnotation a -> NamedText
showAnnotation AnnotAnchor = "anchor"
showAnnotation AnnotNonpolarType = "type"
showAnnotation AnnotPositiveType = "type"
showAnnotation AnnotNegativeType = "type"

interpretSpecialForm :: FullNameRef -> NonEmpty SyntaxAnnotation -> QInterpreter QValue
interpretSpecialForm name annotations = do
    MkQSpecialForm largs val <- lookupSpecialForm name
    margs <- unComposeInner $ specialFormArgs largs $ toList annotations
    case margs of
        Just args -> val args
        Nothing ->
            throw $
            SpecialFormWrongAnnotationsError
                name
                (listTypeToList showAnnotation largs)
                (fmap showSA $ toList annotations)

interpretConstant :: SyntaxConstant -> QInterpreter QExpression
interpretConstant SCIfThenElse = return $ qConstExprAny $ jmToValue qifthenelse
interpretConstant (SCConstructor lit) = interpretConstructor lit

withMatch :: QMatch -> QInterpreter QPartialExpression -> QInterpreter QPartialExpression
withMatch match mexp =
    withScopeBuilder (registerMatchBindings match) $ \() -> do
        exp <- mexp
        qMatchGate match exp

withAllocateNewVar :: Maybe FullName -> (VarID -> QInterpreter a) -> QInterpreter a
withAllocateNewVar mn call = withScopeBuilder (allocateVar mn) $ \(_, varid) -> call varid

intepretLambdaMatch :: VarID -> SyntaxPattern -> QScopeBuilder QMatch
intepretLambdaMatch varid spat = do
    pat <- interpretPattern spat
    return $ qLambdaPatternMatch varid pat

interpretSinglePattern :: VarID -> SyntaxPattern -> QInterpreter QPartialExpression -> QInterpreter QPartialExpression
interpretSinglePattern varid spat mexp = withScopeBuilder (intepretLambdaMatch varid spat) $ \pat -> withMatch pat mexp

interpretCase :: VarID -> SyntaxCase -> QInterpreter QPartialExpression
interpretCase varid (MkSyntaxCase spat sbody) =
    interpretSinglePattern varid spat $ fmap sealedToPartialExpression $ interpretExpression sbody

specialCaseVarPatterns :: Bool
specialCaseVarPatterns = True

interpretMultiPattern ::
       FixedList n VarID
    -> FixedList n SyntaxPattern
    -> QInterpreter QPartialExpression
    -> QInterpreter QPartialExpression
interpretMultiPattern NilFixedList NilFixedList ma = ma
interpretMultiPattern (ConsFixedList v vv) (ConsFixedList spat spats) ma =
    interpretSinglePattern v spat $ interpretMultiPattern vv spats ma

interpretMulticase :: FixedList n VarID -> SyntaxMulticase n -> QInterpreter QPartialExpression
interpretMulticase varids (MkSyntaxMulticase spats sbody) =
    interpretMultiPattern varids spats $ fmap sealedToPartialExpression $ interpretExpression sbody

multiAbstractExpr :: FixedList n VarID -> QExpression -> QInterpreter QExpression
multiAbstractExpr NilFixedList exp = return exp
multiAbstractExpr (ConsFixedList v vv) exp = do
    exp' <- multiAbstractExpr vv exp
    qAbstractExpr v exp'

interpretNamespaceWith :: SyntaxNamespaceWith -> QInterpreter QScope
interpretNamespaceWith (MkSyntaxNamespaceWith sourcens mitems destns) = do
    let
        matchItem :: FullNameRef -> SyntaxNameRefItem -> Bool
        matchItem name (NameSyntaxNameRefItem name') = name == name'
        matchItem (MkFullNameRef _ nsr') (NamespaceSyntaxNameRefItem nsr) = let
            ns = namespaceConcatRef sourcens nsr
            ns' = namespaceConcatRef sourcens nsr'
            in isJust $ namespaceWithinRef ns ns'
    getNamespaceWithScope sourcens destns $
        case mitems of
            Nothing -> \_ -> True
            Just (b, items) -> \name -> any (matchItem name) items == b

partitionItem :: Namespace -> SyntaxNameRefItem -> ([Namespace], [FullNameRef])
partitionItem _ (NameSyntaxNameRefItem n) = ([], [n])
partitionItem curns (NamespaceSyntaxNameRefItem n) = ([namespaceConcatRef curns n], [])

interpretExpose :: [SyntaxNameRefItem] -> QInterpreter QScopeDocs
interpretExpose items = do
    curns <- paramAsk currentNamespaceParam
    let (namespaces, names) = mconcat $ fmap (partitionItem curns) items
    (scope, docs) <- exportScope namespaces names
    return $ MkQScopeDocs {sdScopes = [scope], sdDocs = MkForest $ fmap pure docs}

interpretDeclarator :: SyntaxDeclarator -> QInterpreter QScopeDocs
interpretDeclarator (SDLetSeq sdecls) = runScopeBuilder_ $ for_ sdecls interpretDeclaration
interpretDeclarator (SDLetRec sdecls) = runScopeBuilder_ $ interpretRecursiveDocDeclarations sdecls
interpretDeclarator (SDWith swns) = do
    scopes <- for swns interpretNamespaceWith
    return $ MkQScopeDocs scopes mempty
interpretDeclarator (SDImport simps) = do
    scopedocs <- for simps $ \modname -> interpretImportDeclaration modname
    return $ mconcat scopedocs

interpretDeclaratorWith :: SyntaxDeclarator -> QInterpreter --> QInterpreter
interpretDeclaratorWith declarator ma = do
    sd <- interpretDeclarator declarator
    withScopeDocs sd ma

interpretExpression' :: SyntaxExpression' -> QInterpreter QExpression
interpretExpression' (SESubsume sexpr stype) = do
    expr <- interpretExpression sexpr
    t <- interpretType stype
    qSubsumeExpr t expr
interpretExpression' (SEAbstract (MkSyntaxCase (MkWithSourcePos _ AnySyntaxPattern) sbody))
    | specialCaseVarPatterns =
        withAllocateNewVar Nothing $ \varid -> do
            expr <- interpretExpression sbody
            qAbstractExpr varid expr
interpretExpression' (SEAbstract (MkSyntaxCase (MkWithSourcePos _ (VarSyntaxPattern n)) sbody))
    | specialCaseVarPatterns =
        withAllocateNewVar (Just n) $ \varid -> do
            expr <- interpretExpression sbody
            qAbstractExpr varid expr
interpretExpression' (SEAbstract match) =
    withAllocateNewVar Nothing $ \varid -> do
        pexpr <- interpretCase varid match
        qAbstractExpr varid $ partialToSealedExpression pexpr
interpretExpression' (SEAbstracts (MkSome multimatch@(MkSyntaxMulticase spats _))) =
    withScopeBuilder (for spats $ \_ -> fmap snd $ allocateVar Nothing) $ \varids -> do
        pexpr <- interpretMulticase varids multimatch
        multiAbstractExpr varids $ partialToSealedExpression pexpr
interpretExpression' (SEDecl declarator sbody) = interpretDeclaratorWith declarator $ interpretExpression sbody
interpretExpression' (SEMatch scases) =
    withAllocateNewVar Nothing $ \varid -> do
        pexprs <- for scases $ interpretCase varid
        pexpr <- qPartialExpressionSumList pexprs
        qAbstractExpr varid $ partialToSealedExpression pexpr
interpretExpression' (SEMatches (MkSyntaxMulticaseList nn scases)) =
    withScopeBuilder (fixedListGenerate nn $ fmap snd $ allocateVar Nothing) $ \varids -> do
        pexprs <- for scases $ interpretMulticase varids
        pexpr <- qPartialExpressionSumList pexprs
        multiAbstractExpr varids $ partialToSealedExpression pexpr
interpretExpression' (SEApply sf sarg) = do
    f <- interpretExpression sf
    arg <- interpretExpression sarg
    qApplyExpr f arg
interpretExpression' (SEConst c) = interpretConstant c
interpretExpression' (SEVar _ name) = qName name
interpretExpression' (SESpecialForm name annots) = do
    val <- interpretSpecialForm name annots
    return $ qConstExprAny val
interpretExpression' (SEAppQuote sexpr) = appNotationQuote $ interpretExpression sexpr
interpretExpression' (SEAppUnquote sexpr) = appNotationUnquote $ interpretExpression sexpr
interpretExpression' (SEList sexprs) = do
    exprs <- for sexprs interpretExpression
    qSequenceExpr exprs
interpretExpression' (SEDebug t sexpr) = do
    expr <- interpretExpression sexpr
    liftIO $ debugMessage $ t <> ": " <> pack (show expr)
    return expr

checkExprVars :: QExpression -> QInterpreter ()
checkExprVars (MkSealedExpression _ expr) = do
    let
        getBadVarErrors ::
               forall t.
               NameWitness VarID (QShimWit 'Negative) t
            -> QInterpreter (Maybe (SourcePos, Some (NameWitness VarID (QShimWit 'Negative))))
        getBadVarErrors w@(MkNameWitness (BadVarID spos _) _) =
            paramWith sourcePosParam spos $ return $ Just $ (spos, MkSome w)
        getBadVarErrors _ = return Nothing
    errorMessages <- sequenceA $ expressionFreeWitnesses getBadVarErrors expr
    case nonEmpty $ catMaybes errorMessages of
        Nothing -> return ()
        Just ww@((spos, _) :| _) ->
            paramWith sourcePosParam spos $ do
                em <- mkErrorMessage
                throw $ em $ nameWitnessErrorType $ fmap snd ww

interpretClosedExpression :: SyntaxExpression -> QInterpreter QExpression
interpretClosedExpression sexpr = do
    expr <- interpretExpression sexpr
    checkExprVars expr
    return expr

interpretBinding :: SingleBinding -> QInterpreter QBinding
interpretBinding sb@MkSingleBinding {..} = do
    mtype <- for sbType interpretType
    expr <- interpretClosedExpression sbBody
    return $ qBindExpr sbVarID (sbDefDoc sb) mtype expr

interpretDeclarationWith :: SyntaxDeclaration -> QInterpreter --> QInterpreter
interpretDeclarationWith sdecl ma = do
    sd <- runScopeBuilder_ $ interpretDeclaration sdecl
    withScopeDocs sd ma

interpretGeneralSubtypeRelation ::
       TrustOrVerify -> SyntaxType -> SyntaxType -> SyntaxExpression -> QInterpreter QSubtypeConversionEntry
interpretGeneralSubtypeRelation trustme sta stb sbody = do
    ata <- interpretType @'Negative sta
    atb <- interpretType @'Positive stb
    case ata of
        MkSome ta@(dolanToMaybeType -> Just gta) ->
            case atb of
                MkSome tb@(dolanToMaybeType -> Just gtb) -> do
                    let
                        funcWit :: QIsoShimWit 'Positive _
                        funcWit = funcShimWit (mkShimWit ta) (mkShimWit tb)
                    body <- interpretExpression sbody
                    case funcWit of
                        MkShimWit funcType iconv -> do
                            convexpr <- typedSubsumeExpressionToOpen mempty funcType body
                            return $
                                subtypeConversionEntry trustme Nothing gta gtb $
                                fmap
                                    (functionToShim "user-subtype" . (shimToFunction $ polarPolyIsoNegative iconv))
                                    convexpr
                _ -> throw $ InterpretTypeNotGroundedError $ exprShow atb
        _ -> throw $ InterpretTypeNotGroundedError $ exprShow ata

nonpolarSimpleEntityType :: QNonpolarType t -> QInterpreter (QGroundType '[] t, StorableGroundType t)
nonpolarSimpleEntityType (GroundedNonpolarType t NilCCRArguments)
    | Just (NilListType, et) <- dolanToMonoGroundType t = return (t, et)
nonpolarSimpleEntityType t = throw $ InterpretTypeNotSimpleEntityError $ exprShow t

interpretIdentitySubtypeRelation :: SyntaxType -> SyntaxType -> QInterpreter QSubtypeConversionEntry
interpretIdentitySubtypeRelation sta stb = do
    ata <- interpretNonpolarType sta
    atb <- interpretNonpolarType stb
    case (ata, atb) of
        (MkSome ta, MkSome tb) -> do
            (gta, tea) <- nonpolarSimpleEntityType ta
            (gtb, _) <- nonpolarSimpleEntityType tb
            case getGroundFamily openStorableFamilyWitness gtb of
                Just (MkOpenEntityFamily _) -> do
                    adapter <- storableGroundTypeAdapter tea NilArguments
                    return $
                        case getGroundFamily openStorableFamilyWitness gta of
                            Just (MkOpenEntityFamily _) ->
                                MkSubtypeConversionEntry Verify gta gtb coerceSubtypeConversion
                            Nothing ->
                                MkSubtypeConversionEntry TrustMe gta gtb $
                                singleSubtypeConversion Nothing $
                                coerceShim "open entity" .
                                (functionToShim "entityConvert" $ storeAdapterConvert adapter)
                Nothing ->
                    case getGroundFamily abstractDynamicStorableFamilyWitness gtb of
                        Just MkAbstractDynamicEntityFamily {} ->
                            case getGroundFamily abstractDynamicStorableFamilyWitness gta of
                                Just MkAbstractDynamicEntityFamily {} ->
                                    return $ MkSubtypeConversionEntry Verify gta gtb identitySubtypeConversion
                                Nothing ->
                                    case getGroundFamily concreteDynamicStorableFamilyWitness gta of
                                        Just (MkConcreteDynamicEntityFamily _ _) ->
                                            return $ MkSubtypeConversionEntry Verify gta gtb identitySubtypeConversion
                                        Nothing -> throw $ InterpretTypeNotDynamicEntityError $ exprShow ta
                        Nothing -> throw $ InterpretTypeNotOpenEntityError $ exprShow tb

interpretSubtypeRelation ::
       TrustOrVerify -> SyntaxType -> SyntaxType -> Maybe SyntaxExpression -> RawMarkdown -> QScopeBuilder ()
interpretSubtypeRelation trustme sta stb mbody docDescription = do
    sce <-
        builderLift $
        case mbody of
            Just body -> interpretGeneralSubtypeRelation trustme sta stb body
            Nothing -> interpretIdentitySubtypeRelation sta stb
    registerSubtypeConversion sce
    let
        diSubtype = exprShow sta
        diSupertype = exprShow stb
        docItem = SubtypeRelationDocItem {..}
    registerDocs $ pure MkDefDoc {..}

interpretModuleDeclaration :: SyntaxModuleDeclaration -> QScopeBuilder [(FullName, VarID)]
interpretModuleDeclaration (MkSyntaxModuleDeclaration givens decl) = do
    pairs <- for givens $ \(MkSyntaxGiven name _) -> allocateVar $ Just name
    interpretDeclaration decl
    return pairs

interpretModule :: SyntaxModule -> QInterpreter QModule
interpretModule (MkSyntaxModule sdecls) = do
    (pairss, MkQScopeDocs scopes docs) <- runScopeBuilder $ for sdecls interpretModuleDeclaration
    scope <- joinAllScopes scopes
    return $ MkQModule docs (mconcat pairss) scope
