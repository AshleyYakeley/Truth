module Pinafore.Language.Grammar.Interpret.TypeDecl
    ( interpretTypeDeclaration
    , interpretRecursiveTypeDeclarations
    ) where

import Data.Graph
import qualified Data.List as List
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Interpret.Type
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes

data Constructor w t =
    forall a. MkConstructor Name
                            (ListType w a)
                            (HList a -> t)
                            (t -> Maybe (HList a))

extendConstructor :: Constructor w t -> Constructor w (Either a t)
extendConstructor (MkConstructor n lt at tma) = MkConstructor n lt (Right . at) (\t -> eitherRight t >>= tma)

data ClosedEntityBox =
    forall t. MkClosedEntityBox (ClosedEntityType t)
                                [Constructor MonoEntityType t]

assembleClosedEntityType :: [(Name, Anchor, AnyW (ListType MonoEntityType))] -> ClosedEntityBox
assembleClosedEntityType [] = MkClosedEntityBox NilClosedEntityType []
assembleClosedEntityType ((n, a, MkAnyW el):cc) =
    case assembleClosedEntityType cc of
        MkClosedEntityBox ct conss ->
            MkClosedEntityBox (ConsClosedEntityType a el ct) $
            (MkConstructor n el Left eitherLeft) : fmap extendConstructor conss

data DataBox =
    forall t. MkDataBox (PinaforeDataType t)
                        [Constructor (PinaforeNonpolarType '[]) t]

assembleDataType :: [(Name, AnyW (ListType (PinaforeNonpolarType '[])))] -> DataBox
assembleDataType [] = MkDataBox NilDataType []
assembleDataType ((n, MkAnyW el):cc) =
    case assembleDataType cc of
        MkDataBox ct conss ->
            MkDataBox (ConsDataType el ct) $ (MkConstructor n el Left eitherLeft) : fmap extendConstructor conss

datatypeIOWitness :: IOWitness ('MkWitKind IdentifiedFamily)
datatypeIOWitness = $(iowitness [t|'MkWitKind IdentifiedFamily|])

constructorFreeVariables :: Constructor (PinaforeNonpolarType '[]) t -> [AnyW SymbolType]
constructorFreeVariables (MkConstructor _ lt _ _) = mconcat $ listTypeToList nonpolarTypeFreeVariables lt

interpretClosedEntityTypeConstructor ::
       SyntaxClosedEntityConstructor -> PinaforeSourceInterpreter (Name, Anchor, AnyW (ListType MonoEntityType))
interpretClosedEntityTypeConstructor (MkSyntaxClosedEntityConstructor consName stypes anchor) = do
    etypes <- for stypes interpretMonoEntityType
    return (consName, anchor, assembleListType etypes)

interpretDataTypeConstructor ::
       SyntaxDatatypeConstructor -> PinaforeSourceInterpreter (Name, AnyW (ListType (PinaforeNonpolarType '[])))
interpretDataTypeConstructor (MkSyntaxDatatypeConstructor consName stypes) = do
    etypes <- for stypes interpretNonpolarType
    return (consName, assembleListType etypes)

intepretSyntaxDynamicEntityConstructor ::
       SyntaxDynamicEntityConstructor -> SourceInterpreter PinaforeTypeSystem [DynamicType]
intepretSyntaxDynamicEntityConstructor (AnchorSyntaxDynamicEntityConstructor a) = return $ pure $ mkDynamicType a
intepretSyntaxDynamicEntityConstructor (NameSyntaxDynamicEntityConstructor name) = do
    MkBoundType t <- lookupBoundType name
    case matchFamilyType aDynamicEntityFamilyWitness $ pgtFamilyType t of
        Just (MkADynamicEntityFamily _ dt) -> return $ toList dt
        Nothing -> throw $ InterpretTypeNotDynamicEntityError $ exprShow name

makeOpenEntityType :: Name -> TypeID -> AnyW OpenEntityType
makeOpenEntityType n tid = valueToWitness tid $ \tidsym -> MkAnyW $ MkOpenEntityType n tidsym

typeDeclarationTypeBox ::
       SourcePos
    -> Name
    -> Markdown
    -> SyntaxTypeDeclaration
    -> PinaforeInterpreter (TypeFixBox PinaforeTypeSystem (WMFunction PinaforeInterpreter PinaforeInterpreter))
typeDeclarationTypeBox spos name doc OpenEntitySyntaxTypeDeclaration = do
    tid <- newTypeID
    let
        mktype _ =
            case makeOpenEntityType name tid of
                MkAnyW t -> MkBoundType $ openEntityGroundType t
    return $ mkTypeFixBox spos name doc mktype $ return ((), id)
typeDeclarationTypeBox spos name doc (ClosedEntitySyntaxTypeDeclaration sconss) = do
    tid <- newTypeID
    return $
        valueToWitness tid $ \(tidsym :: TypeIDType n) -> let
            mktype t = MkBoundType $ closedEntityGroundType $ MkClosedEntityFamily name tidsym t
            in mkTypeFixBox spos name doc mktype $
               runSourcePos spos $ do
                   tconss <- for sconss interpretClosedEntityTypeConstructor
                   MkClosedEntityBox (ct :: ClosedEntityType t) conss <- return $ assembleClosedEntityType tconss
                   tident :: Identified n :~: t <- unsafeIdentify
                   let
                       cti :: ClosedEntityType (Identified n)
                       cti = (reflId $ applyRefl id $ invert tident) ct
                       ctf :: forall polarity. Is PolarityType polarity
                           => PinaforeShimWit polarity (Identified n)
                       ctf =
                           singleDolanShimWit $
                           mkPolarShimWit $
                           GroundedDolanSingularType
                               (closedEntityGroundType $ MkClosedEntityFamily name tidsym cti)
                               NilDolanArguments
                   patts <-
                       for conss $ \(MkConstructor cname lt at tma) -> do
                           ltp <- return $ mapListType monoToPositiveDolanType lt
                           ltn <- mapMListType monoEntityToNegativePinaforeType lt
                           let
                               expr =
                                   qConstExprAny $
                                   MkAnyValue
                                       (qFunctionPosWitnesses ltn (mapPolarShimWit (reflId $ invert tident) ctf))
                                       at
                               pc = toPatternConstructor ctf ltp $ tma . reflId tident
                           withNewPatternConstructor cname doc expr pc
                   return (cti, compAll patts)
typeDeclarationTypeBox spos name doc (DatatypeSyntaxTypeDeclaration sconss) = do
    tid <- newTypeID
    return $
        valueToWitness tid $ \tidsym -> let
            gt = singleGroundType' @'[] (MkFamilyType datatypeIOWitness $ MkIdentifiedFamily tidsym) $ exprShowPrec name
            mktype _ = MkBoundType gt
            in mkTypeFixBox spos name doc mktype $
               runSourcePos spos $ do
                   tconss <- for sconss interpretDataTypeConstructor
                   MkDataBox _dt conss <- return $ assembleDataType tconss
                   let
                       freevars :: [AnyW SymbolType]
                       freevars = nub $ mconcat $ fmap constructorFreeVariables conss
                       declaredvars :: [AnyW SymbolType]
                       declaredvars = [] -- ISSUE #41
                       unboundvars :: [AnyW SymbolType]
                       unboundvars = freevars List.\\ declaredvars
                   case nonEmpty unboundvars of
                       Nothing -> return ()
                       Just vv ->
                           throw $ InterpretUnboundTypeVariablesError $ fmap (\(MkAnyW s) -> symbolTypeToName s) vv
                   let
                       ctf :: forall polarity. Is PolarityType polarity
                           => PinaforeShimWit polarity _
                       ctf = singleDolanShimWit $ mkPolarShimWit $ GroundedDolanSingularType gt NilDolanArguments
                   tident <- unsafeIdentify
                   let tiso = reflId tident
                   patts <-
                       for conss $ \(MkConstructor cname lt at tma) -> do
                           ltp <- return $ mapListType nonpolarToDolanType lt
                           ltn <- return $ mapListType nonpolarToDolanType lt
                           let
                               expr =
                                   qConstExprAny $
                                   MkAnyValue (qFunctionPosWitnesses ltn ctf) $ \hl -> isoBackwards tiso $ at hl
                               pc = toPatternConstructor ctf ltp $ \t -> tma $ isoForwards tiso t
                           withNewPatternConstructor cname doc expr pc
                   return ((), compAll patts)
typeDeclarationTypeBox spos name doc (DynamicEntitySyntaxTypeDeclaration stcons) =
    return $ let
        mktype :: DynamicEntityType -> PinaforeBoundType
        mktype t = MkBoundType $ aDynamicEntityGroundType name t
        in mkTypeFixBox spos name doc mktype $
           runSourcePos spos $ do
               dt <- for stcons intepretSyntaxDynamicEntityConstructor
               let
                   dts = setFromList $ mconcat $ toList dt
                   tp = aDynamicEntityGroundType name dts
               return $
                   (,) dts $
                   MkWMFunction $
                   withSubtypeConversions $
                   pure $
                   MkSubypeConversionEntry tp $ \t -> do
                       Refl <- testEquality (pgtVarianceType t) NilListType
                       MkADynamicEntityFamily _ dts' <- matchFamilyType aDynamicEntityFamilyWitness $ pgtFamilyType t
                       ifpure (isSubsetOf dts' dts) idSubtypeConversion

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
           (nn@((spos, _) :| _):_) -> runSourcePos spos $ throw $ DeclareDynamicTypeCycleError $ fmap snd nn

interpretTypeDeclaration ::
       SourcePos -> Name -> Markdown -> SyntaxTypeDeclaration -> PinaforeInterpreter --> PinaforeInterpreter
interpretTypeDeclaration spos name doc tdecl ma = do
    tbox <- typeDeclarationTypeBox spos name doc tdecl
    (wtt, wcc) <- registerTypeName tbox
    runWMFunction (wtt . compAll wcc) ma

interpretRecursiveTypeDeclarations ::
       [(SourcePos, Name, Markdown, SyntaxTypeDeclaration)] -> PinaforeInterpreter --> PinaforeInterpreter
interpretRecursiveTypeDeclarations decls ma = do
    checkDynamicTypeCycles decls
    wfs <- for decls $ \(spos, name, doc, tdecl) -> typeDeclarationTypeBox spos name doc tdecl
    (wtt, wcc) <- registerRecursiveTypeNames wfs
    runWMFunction (wtt . compAll wcc) ma
