module Pinafore.Language.Interpret.TypeDecl
    ( monoEntityToNegativePinaforeType
    , interpretTypeDeclarations
    ) where

import Data.Graph
import qualified Data.List as List
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpret.Interpreter
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Name
import Pinafore.Language.Syntax
import Pinafore.Language.Type
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

datatypeIOWitness :: IOWitness ('MkWitKind IdentifiedType)
datatypeIOWitness = $(iowitness [t|'MkWitKind IdentifiedType|])

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

monoEntityToNegativePinaforeType ::
       forall m t. MonadThrow ErrorType m
    => MonoEntityType t
    -> m (PinaforeShimWit 'Negative t)
monoEntityToNegativePinaforeType et =
    case monoToMaybeNegativeDolanType et of
        Just wit -> return wit
        Nothing -> throw InterpretTypeNoneNotNegativeEntityError

intepretSyntaxDynamicEntityConstructor ::
       SyntaxDynamicEntityConstructor -> SourceInterpreter PinaforeTypeSystem [DynamicType]
intepretSyntaxDynamicEntityConstructor (AnchorSyntaxDynamicEntityConstructor a) = return $ pure $ mkDynamicType a
intepretSyntaxDynamicEntityConstructor (NameSyntaxDynamicEntityConstructor name) = do
    t <- lookupBoundType name
    case t of
        DynamicEntityBoundType dt -> return $ toList dt
        _ -> throw $ InterpretTypeNotDynamicEntityError $ exprShow name

interpretTypeDeclaration ::
       SourcePos
    -> Name
    -> TypeID
    -> SyntaxTypeDeclaration
    -> PinaforeTypeBox (WMFunction PinaforeInterpreter PinaforeInterpreter)
interpretTypeDeclaration _ name tid OpenEntitySyntaxTypeDeclaration =
    MkTypeBox name (\_ -> OpenEntityBoundType tid) $ return ((), id)
interpretTypeDeclaration spos name tid (ClosedEntitySyntaxTypeDeclaration sconss) =
    valueToWitness tid $ \(tidsym :: TypeIDType n) -> let
        mktype t = ClosedEntityBoundType tidsym t
        in MkTypeBox name mktype $
           runSourcePos spos $ do
               tconss <- for sconss interpretClosedEntityTypeConstructor
               MkClosedEntityBox (ct :: ClosedEntityType t) conss <- return $ assembleClosedEntityType tconss
               tident :: Identified n :~: t <- unsafeGetIdentification
               let
                   cti :: ClosedEntityType (Identified n)
                   cti = (reflId $ applyRefl id $ invert tident) ct
                   ctf :: forall polarity. Is PolarityType polarity
                       => PinaforeShimWit polarity (Identified n)
                   ctf =
                       singleDolanShimWit $
                       mkShimWit $
                       GroundDolanSingularType
                           (EntityPinaforeGroundType NilListType $ ClosedEntityGroundType name tidsym cti)
                           NilDolanArguments
               patts <-
                   for conss $ \(MkConstructor cname lt at tma) -> do
                       ltp <- return $ mapListType monoToPositiveDolanType lt
                       ltn <- mapMListType monoEntityToNegativePinaforeType lt
                       let
                           expr =
                               qConstExprAny $
                               MkAnyValue (qFunctionPosWitnesses ltn (mapShimWit (reflId $ invert tident) ctf)) at
                           pc = toPatternConstructor ctf ltp $ tma . reflId tident
                       withNewPatternConstructor cname expr pc
               return (cti, compAll patts)
interpretTypeDeclaration spos name tid (DatatypeSyntaxTypeDeclaration sconss) =
    valueToWitness tid $ \tidsym -> let
        pt = MkProvidedType datatypeIOWitness $ MkIdentifiedType tidsym
        mktype _ = SimpleBoundType NilListType NilDolanVarianceMap (exprShowPrec name) pt
        in MkTypeBox name mktype $
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
                   Just vv -> throw $ InterpretUnboundTypeVariablesError $ fmap (\(MkAnyW s) -> symbolTypeToName s) vv
               let
                   ctf :: forall polarity. Is PolarityType polarity
                       => PinaforeShimWit polarity _
                   ctf =
                       singleDolanShimWit $
                       mkShimWit $
                       GroundDolanSingularType
                           (SimpleGroundType NilListType NilDolanVarianceMap (exprShowPrec name) pt)
                           NilDolanArguments
               tident <- unsafeGetIdentification
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
                       withNewPatternConstructor cname expr pc
               return ((), compAll patts)
interpretTypeDeclaration spos name _ (DynamicEntitySyntaxTypeDeclaration stcons) =
    MkTypeBox name DynamicEntityBoundType $
    runSourcePos spos $ do
        dt <- for stcons intepretSyntaxDynamicEntityConstructor
        let
            dts = setFromList $ mconcat $ toList dt
            tp = EntityPinaforeGroundType NilListType (ADynamicEntityGroundType name dts)
        return $
            (,) dts $
            MkWMFunction $
            withSubtypeConversions $
            pure $
            MkSubypeConversionEntry tp $ \case
                EntityPinaforeGroundType NilListType (ADynamicEntityGroundType _ dts')
                    | isSubsetOf dts' dts -> Just idSubtypeConversion
                _ -> Nothing

checkDynamicTypeCycles :: [(SourcePos, Name, SyntaxTypeDeclaration)] -> PinaforeInterpreter ()
checkDynamicTypeCycles decls = let
    constructorName :: SyntaxDynamicEntityConstructor -> Maybe Name
    constructorName (NameSyntaxDynamicEntityConstructor n) = Just n
    constructorName (AnchorSyntaxDynamicEntityConstructor _) = Nothing
    getDynamicTypeReferences :: (SourcePos, Name, SyntaxTypeDeclaration) -> Maybe ((SourcePos, Name), Name, [Name])
    getDynamicTypeReferences (spos, n, DynamicEntitySyntaxTypeDeclaration cs) =
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

interpretTypeDeclarations ::
       [(SourcePos, Name, SyntaxTypeDeclaration)] -> MFunction PinaforeInterpreter PinaforeInterpreter
interpretTypeDeclarations decls ma = do
    checkDynamicTypeCycles decls
    wfs <-
        for decls $ \(spos, name, tdecl) ->
            runSourcePos spos $ do
                tid <- liftSourcePos newTypeID
                return $ (spos, interpretTypeDeclaration spos name tid tdecl)
    (wtt, wcc) <- registerTypeNames wfs
    runWMFunction (wtt . compAll wcc) ma
