module Pinafore.Language.Interpret.Expression
    ( interpretExpression
    , interpretModule
    , interpretDeclarationWith
    , interpretType
    , interpretImportDeclaration
    , interpretPattern
    ) where

import Data.Graph hiding (Forest, Tree)
import Import
import Pinafore.Language.Debug
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.If
import Pinafore.Language.Interpret.AppNotation
import Pinafore.Language.Interpret.FreeVars
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Interpret.TypeDecl
import Pinafore.Language.Interpret.Value
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Types
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Pinafore.Language.Var
import Pinafore.Language.VarID

interpretPatternConstructor :: SyntaxConstructor -> QInterpreter (Either QPatternConstructor QRecordConstructor)
interpretPatternConstructor (SLNamedConstructor name _) = lookupPatternConstructor name
interpretPatternConstructor (SLNumber v) =
    return $
    Left $
    qToPatternConstructor $
    ImpureFunction $
    pure $ \v' ->
        if v == v'
            then Just ()
            else Nothing
interpretPatternConstructor (SLString v) =
    return $
    Left $
    qToPatternConstructor $
    ImpureFunction $
    pure $ \v' ->
        if v == v'
            then Just ()
            else Nothing
interpretPatternConstructor SLUnit = return $ Left $ qToPatternConstructor $ PureFunction $ pure $ \() -> ()
interpretPatternConstructor SLPair =
    return $ Left $ qToPatternConstructor $ PureFunction $ pure $ \(a :: A, b :: B) -> (a, (b, ()))

recordNameWitnesses ::
       Namespace
    -> ListType (QSignature 'Positive) tt
    -> QScopeBuilder [SomeFor ((->) (ListVProduct tt)) (NameWitness VarID (QShimWit 'Positive))]
recordNameWitnesses ns lt =
    listTypeForList (pairListType lt $ listVProductGetters lt) $ \case
        MkPairType (ValueSignature _ name t _) f -> do
            (_, vid) <- allocateLambdaVar $ Just $ MkFullName name ns
            return $ MkSomeFor (MkNameWitness vid $ mkShimWit t) f

mkRecordPattern ::
       Namespace -> ListVType (QSignature 'Positive) tt -> QScopeBuilder (QOpenPattern (Maybe (ListVProduct tt)) ())
mkRecordPattern ns ww = do
    sff <- recordNameWitnesses ns $ listVTypeToType ww
    return $ MkPattern sff $ ImpureFunction $ pure $ fmap $ \a -> (a, ())

constructPattern :: Namespace -> Either QPatternConstructor QRecordConstructor -> [QPattern] -> QScopeBuilder QPattern
constructPattern _ (Left pc) pats = builderLift $ qConstructPattern pc pats
constructPattern _ (Right _) (_:_) = throw PatternTooManyConsArgsError
constructPattern ns (Right (MkQRecordConstructor sigs _ tt codec)) [] = do
    pat <- mkRecordPattern ns sigs
    return $ MkSealedPattern (shimWitToDolan tt) $ pat . arr (decode codec)

interpretPattern' :: SyntaxPattern' -> QScopeBuilder QPattern
interpretPattern' AnySyntaxPattern = return qAnyPattern
interpretPattern' (VarSyntaxPattern n) = do
    (_, vid) <- allocateLambdaVar $ Just n
    return $ qVarPattern vid
interpretPattern' (BothSyntaxPattern spat1 spat2) = do
    pat1 <- interpretPattern spat1
    pat2 <- interpretPattern spat2
    builderLift $ qBothPattern pat1 pat2
interpretPattern' (ConstructorSyntaxPattern ns scons spats) = do
    pc <- builderLift $ interpretPatternConstructor scons
    pats <- for spats interpretPattern
    pat@(MkSealedPattern tw patw) <- constructPattern ns pc pats
    mstw <- builderLift $ getOptGreatestDynamicSupertypeSW tw
    return $
        case mstw of
            Nothing -> pat
            Just (MkShimWit stw (MkPolarShim (MkComposeShim convexpr))) ->
                MkSealedPattern (mkShimWit stw) $ patw . impureFuncPattern (fmap shimToFunction convexpr)
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
                        toPatternConstructor (mkShimWit tn) (ConsListType tpw NilListType) $
                        PureFunction $ pure $ \a -> (a, ())
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
                    Just (MkShimWit dtn (MkPolarShim (MkComposeShim dtconvexpr))) -> do
                        tpw <- invertType tn
                        let
                            pc :: QPatternConstructor
                            pc =
                                toPatternConstructor (mkShimWit dtn) (ConsListType tpw NilListType) $
                                ImpureFunction $ fmap (\conv -> fmap (\a -> (a, ())) . shimToFunction conv) dtconvexpr
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
            Nothing -> allocateLambdaVar Nothing
            Just (name, True) -> allocateLambdaVar $ Just name
            Just (name, False) -> do
                (_, varID) <- allocateLambdaVar Nothing
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
                    funcsexpr =
                        wspos $ SEAbstract $ MkSyntaxCase spat $ wspos $ SEVar RootNamespace (fullNameRef pname) Nothing
                    valsexpr = wspos $ SEApply funcsexpr $ wspos $ SEVar RootNamespace (fullNameRef pvname) Nothing
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

mapVarID :: Map VarID (DefDoc, QExpression) -> [(FullName, VarID)] -> [(FullName, DefDoc, QExpression)]
mapVarID mm =
    mapMaybe $ \(n, v) -> do
        (d, e) <- lookup v mm
        return (n, d, e)

registerMapSingleBindings :: Map VarID (DefDoc, QExpression) -> [SingleBinding] -> QScopeBuilder ()
registerMapSingleBindings bmap sbinds =
    registerLetBindingsDocs $ mapVarID bmap $ fmap (\sb -> (sbName sb, sbVarID sb)) sbinds

interpretRecursiveLetBindingsClump :: [SingleBinding] -> QScopeBuilder ()
interpretRecursiveLetBindingsClump sbinds = do
    bmap <-
        builderLift $ do
            bb <- for sbinds interpretBinding
            qUncheckedBindingsRecursiveLetExpr bb
    registerMapSingleBindings bmap sbinds

interpretRecursiveLetBindings :: [SingleBinding] -> QScopeBuilder ()
interpretRecursiveLetBindings sbinds = do
    case nonEmpty $ duplicates $ fmap sbName sbinds of
        Nothing -> return ()
        Just b -> throw $ InterpretBindingsDuplicateError b
    for_ (clumpBindings sbinds) interpretRecursiveLetBindingsClump

interpretSequentialLetBinding :: SingleBinding -> QScopeBuilder ()
interpretSequentialLetBinding sbind = do
    bmap <-
        builderLift $ do
            b <- interpretBinding sbind
            qBindingSequentialLetExpr b
    registerMapSingleBindings bmap $ pure sbind

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
    newmod <- getModule modname
    return $ MkQScopeDocs [moduleScope newmod] $ moduleDoc newmod

sectionHeading :: Text -> RawMarkdown -> QScopeBuilder --> QScopeBuilder
sectionHeading heading doc =
    prodCensor builderDocsProd $ \docs -> pureForest $ MkTree (MkDefDoc (HeadingDocItem $ plainText heading) doc) docs

allocateQRV :: SyntaxSignature -> QScopeBuilder (VarID, Some (QSignature 'Positive))
allocateQRV (MkSyntaxWithDoc _ (MkWithSourcePos _ (ValueSyntaxSignature name stype msdef))) = do
    curns <- builderLift $ paramAsk currentNamespaceParam
    (_, varid) <- allocatePolymorphicVar $ MkFullName name curns
    builderLift $ do
        sqtype <- interpretType @'Positive stype
        case sqtype of
            MkSome (qtype :: _ t) -> do
                mqdef <-
                    for msdef $ \sdef -> do
                        qdef <- interpretExpression sdef
                        qSubsumeExpressionToOpen qtype qdef
                return (varid, MkSome $ ValueSignature Nothing name qtype mqdef)
allocateQRV (MkSyntaxWithDoc _ (MkWithSourcePos _ (SupertypeConstructorSyntaxSignature {}))) =
    builderLift $ throw RecordFunctionSupertype

recordValueAddSig :: VarID -> Some (QSignature 'Positive) -> QRecordValue -> QInterpreter QRecordValue
recordValueAddSig varid (MkSome qsig@(ValueSignature _ _ qtype _)) (MkQRecordValue tt fexpr) = do
    fexpr' <- qPolyAbstractF varid (mkShimWit qtype) fexpr
    return $ MkQRecordValue (ConsListType qsig tt) fexpr'

subsumeRecordValue :: Some (QType Positive) -> QRecordValue -> QInterpreter QRecordValue
subsumeRecordValue qtype (MkQRecordValue sigs fexpr) = do
    fexpr' <- qSubsumeFExpr qtype fexpr
    return $ MkQRecordValue sigs fexpr'

interpretRecordValueDecl :: [SyntaxSignature] -> Maybe SyntaxType -> SyntaxExpression -> QInterpreter QRecordValue
interpretRecordValueDecl sigs mstype sexpr =
    withScopeBuilder (for sigs allocateQRV) $ \qrvsigs -> do
        expr <- interpretExpression sexpr
        let
            rv0 :: QRecordValue
            rv0 = MkQRecordValue NilListType $ toSealedFExpression expr
        rv <- unEndoM (concatmap (\(varid, qsig) -> MkEndoM $ recordValueAddSig varid qsig) qrvsigs) rv0
        case mstype of
            Nothing -> return rv
            Just stype -> do
                qtype <- interpretType @'Positive stype
                subsumeRecordValue qtype rv

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
        RecordSyntaxDeclaration name sigs mtype expr -> do
            let
                docItem = valueDocItem name mtype
                docDescription = doc
            rv <- builderLift $ interpretRecordValueDecl sigs mtype expr
            registerRecordValue name MkDefDoc {..} rv
        DeclaratorSyntaxDeclaration declarator -> do
            sd <- builderLift $ interpretDeclarator declarator
            registerScopeDocs sd
        DeclaratorInSyntaxDeclaration declarator declaration -> do
            sd <- builderLift $ interpretDeclaratorWith declarator $ runScopeBuilder $ interpretDeclaration declaration
            registerScopeDocs sd
        ExposeDeclaration items -> do
            sd <- builderLift $ interpretExpose items
            outputScopeDocs sd
        NamespaceSyntaxDeclaration makeSection nsn decls ->
            withCurrentNamespaceScope nsn $
            (if makeSection
                 then sectionHeading (showText nsn) doc
                 else id) $
            for_ decls interpretDeclaration
        DocSectionSyntaxDeclaration heading decls -> sectionHeading heading doc $ for_ decls interpretDeclaration
        DebugSyntaxDeclaration nameref -> do
            mfd <- builderLift $ lookupDebugBindingInfo nameref
            liftIO $
                debugMessage $
                case mfd of
                    Just (fn, desc) -> showText nameref <> " = " <> showText fn <> ": " <> pack desc
                    Nothing -> showText nameref <> " not found"

interpretRecordArgList :: [(Name, SyntaxExpression)] -> QInterpreter [(Name, QExpression)]
interpretRecordArgList sarglist =
    for sarglist $ \(n, sv) -> do
        v <- interpretExpression sv
        return (n, v)

interpretConstructor :: SyntaxConstructor -> QInterpreter QExpression
interpretConstructor (SLNumber n) =
    return $
    case decode safeRationalNumber n of
        Just r ->
            case decode integerSafeRational r of
                Just i -> qConstValue $ jmToValue i
                Nothing -> qConstValue $ jmToValue r
        Nothing -> qConstValue $ jmToValue n
interpretConstructor (SLString v) = return $ qConstValue $ jmToValue v
interpretConstructor (SLNamedConstructor v mvals) = do
    marglist <- for mvals interpretRecordArgList
    interpretValue v marglist
interpretConstructor SLPair = return $ qConstValue $ jmToValue ((,) :: A -> B -> (A, B))
interpretConstructor SLUnit = return $ qConstValue $ jmToValue ()

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

interpretSpecialForm :: FullNameRef -> NonEmpty SyntaxAnnotation -> QInterpreter QExpression
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
interpretConstant SCIfThenElse = return $ qConstValue $ jmToValue qifthenelse
interpretConstant (SCConstructor lit) = interpretConstructor lit

withMatch :: QMatch -> QInterpreter QPartialExpression -> QInterpreter QPartialExpression
withMatch match mexp =
    withScopeBuilder (registerMatchBindings match) $ \() -> do
        exp <- mexp
        qMatchGate match exp

withAllocateNewVar :: Maybe FullName -> (VarID -> QInterpreter a) -> QInterpreter a
withAllocateNewVar mn call = withScopeBuilder (allocateLambdaVar mn) $ \(_, varid) -> call varid

intepretLambdaMatch :: VarID -> SyntaxPattern -> QScopeBuilder QMatch
intepretLambdaMatch varid spat = do
    pat <- interpretPattern spat
    return $ qLambdaPatternMatch varid pat

interpretSinglePattern :: VarID -> SyntaxPattern -> QInterpreter QPartialExpression -> QInterpreter QPartialExpression
interpretSinglePattern varid spat mexp = withScopeBuilder (intepretLambdaMatch varid spat) $ \pat -> withMatch pat mexp

interpretCase :: VarID -> SyntaxCase -> QInterpreter QPartialExpression
interpretCase varid (MkSyntaxCase spat sbody) =
    interpretSinglePattern varid spat $ fmap sealedToPartialExpression $ interpretExpression sbody

specialCaseVarPatternsINTERNAL :: Bool
specialCaseVarPatternsINTERNAL = True

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
    let (namespaces, names) = concatmap (partitionItem curns) items
    (scope, docs) <- exportScope namespaces names
    return $ MkQScopeDocs {sdScopes = [scope], sdDocs = MkForest $ fmap pure docs}

interpretDeclarator :: SyntaxDeclarator -> QInterpreter QScopeDocs
interpretDeclarator (SDLetSeq sdecls) = runScopeBuilder $ for_ sdecls interpretDeclaration
interpretDeclarator (SDLetRec sdecls) = runScopeBuilder $ interpretRecursiveDocDeclarations sdecls
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
    | specialCaseVarPatternsINTERNAL =
        withAllocateNewVar Nothing $ \varid -> do
            expr <- interpretExpression sbody
            qAbstractExpr varid expr
interpretExpression' (SEAbstract (MkSyntaxCase (MkWithSourcePos _ (VarSyntaxPattern n)) sbody))
    | specialCaseVarPatternsINTERNAL =
        withAllocateNewVar (Just n) $ \varid -> do
            expr <- interpretExpression sbody
            qAbstractExpr varid expr
interpretExpression' (SEAbstract match) =
    withAllocateNewVar Nothing $ \varid -> do
        pexpr <- interpretCase varid match
        qAbstractExpr varid $ partialToSealedExpression pexpr
interpretExpression' (SEAbstracts (MkSome multimatch@(MkSyntaxMulticase spats _))) =
    withScopeBuilder (for spats $ \_ -> fmap snd $ allocateLambdaVar Nothing) $ \varids -> do
        pexpr <- interpretMulticase varids multimatch
        multiAbstractExpr varids $ partialToSealedExpression pexpr
interpretExpression' (SEDecl declarator sbody) = interpretDeclaratorWith declarator $ interpretExpression sbody
interpretExpression' (SEImply binds sbody) = do
    bexpr <- interpretExpression sbody
    substs <-
        for binds $ \(n, mtype, sexpr) -> do
            expr <- interpretExpression sexpr
            expr' <-
                case mtype of
                    Nothing -> return expr
                    Just stype -> do
                        t <- interpretType stype
                        qSubsumeExpr t expr
            return (n, expr')
    let
        substf (ImplicitVarID vn) = lookup vn substs
        substf _ = Nothing
    qSubstitute substf bexpr
interpretExpression' (SEMatch scases) =
    withAllocateNewVar Nothing $ \varid -> do
        pexprs <- for scases $ interpretCase varid
        pexpr <- qPartialExpressionSumList pexprs
        qAbstractExpr varid $ partialToSealedExpression pexpr
interpretExpression' (SEMatches (MkSyntaxMulticaseList nn scases)) =
    withScopeBuilder (fixedListGenerate nn $ fmap snd $ allocateLambdaVar Nothing) $ \varids -> do
        pexprs <- for scases $ interpretMulticase varids
        pexpr <- qPartialExpressionSumList pexprs
        multiAbstractExpr varids $ partialToSealedExpression pexpr
interpretExpression' (SEApply sf sarg) = do
    f <- interpretExpression sf
    arg <- interpretExpression sarg
    qApplyExpr f arg
interpretExpression' (SEConst c) = interpretConstant c
interpretExpression' (SEVar _ name mvals) = do
    marglist <- for mvals interpretRecordArgList
    interpretValue name marglist
interpretExpression' (SEImplicitVar name) = return $ qVar $ ImplicitVarID name
interpretExpression' (SESpecialForm name annots) = interpretSpecialForm name annots
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
    errorMessages <- sequenceA $ freeWitnesses getBadVarErrors expr
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
interpretBinding MkSingleBinding {..} = do
    let
        bdf fexpr = let
            docItem = valueDocItem sbName $ Just fexpr
            docDescription = sbDoc
            in MkDefDoc {..}
    mtype <- for sbType interpretType
    expr <- interpretClosedExpression sbBody
    return $ qBindExpr sbVarID bdf mtype expr

interpretDeclarationWith :: SyntaxDeclaration -> QInterpreter --> QInterpreter
interpretDeclarationWith sdecl ma = do
    sd <- runScopeBuilder $ interpretDeclaration sdecl
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
                    convexpr <- qSubsumeExpressionToOpenWit funcWit body
                    return $
                        subtypeConversionEntry trustme Nothing gta gtb $ fmap (functionToShim "user-subtype") convexpr
                _ -> throw $ InterpretTypeNotGroundedError $ exprShow atb
        _ -> throw $ InterpretTypeNotGroundedError $ exprShow ata

nonpolarSimpleEntityType :: QNonpolarType t -> QInterpreter (QGroundType '[] t, StorableGroundType t)
nonpolarSimpleEntityType (GroundedNonpolarType (MkNonpolarGroundedType t NilCCRArguments))
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
                    adapterexpr <- storableGroundTypeAdapter tea NilArguments
                    return $
                        case getGroundFamily openStorableFamilyWitness gta of
                            Just (MkOpenEntityFamily _) ->
                                MkSubtypeConversionEntry Verify gta gtb coerceSubtypeConversion
                            Nothing ->
                                MkSubtypeConversionEntry TrustMe gta gtb $
                                singleSubtypeConversion Nothing $
                                fmap
                                    (\adapter ->
                                         coerceShim "open entity" .
                                         (functionToShim "entityConvert" $ storeAdapterConvert adapter))
                                    adapterexpr
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

interpretModule :: SyntaxModule -> QInterpreter QModule
interpretModule (MkSyntaxModule sdecls) = do
    MkQScopeDocs scopes docs <- runScopeBuilder $ for_ sdecls interpretDeclaration
    scope <- joinAllScopes scopes
    return $ MkQModule docs scope
