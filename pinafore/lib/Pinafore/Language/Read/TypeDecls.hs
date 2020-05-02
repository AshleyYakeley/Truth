module Pinafore.Language.Read.TypeDecls
    ( readTypeDeclaration
    ) where

import qualified Data.List as List
import Data.Shim
import Language.Expression.Sealed
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Name
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Token
import Pinafore.Language.Read.Type
import Pinafore.Language.Scope
import Pinafore.Language.Syntax
import Pinafore.Language.Type.Data
import Pinafore.Language.Type.TypeID
import Pinafore.Language.TypeSystem
import Pinafore.Language.TypeSystem.Nonpolar
import Pinafore.Language.TypeSystem.Show
import Shapes hiding (try)

readOpenTypeDeclaration :: forall baseupdate. Parser (TypeDecls baseupdate)
readOpenTypeDeclaration = do
    spos <- getPosition
    readThis TokOpenType
    name <- readTypeName
    return $
        typeTypeDecl $
        MkWMFunction $ \rn -> do
            withnt <-
                runSourcePos spos $ do
                    tid <- newTypeID
                    registerTypeName name $ OpenEntityNamedType tid
            runWMFunction withnt rn

readSubtypeDeclaration :: forall baseupdate. Parser (TypeDecls baseupdate)
readSubtypeDeclaration = do
    spos <- getPosition
    readThis TokSubtype
    sta <- readType
    readExactlyThis TokOperator "<="
    stb <- readType
    return $
        relationTypeDecl $
        MkWMFunction $ \rn -> do
            smap <- runSourcePos spos $ interpretSubtypeRelation sta stb
            runWMFunction smap rn

readDataTypeConstructor ::
       Parser (PinaforeScoped baseupdate (Name, AnyW (ListType (PinaforeNonpolarType baseupdate '[]))))
readDataTypeConstructor = do
    consName <- readThis TokUName
    mtypes <-
        many $ do
            spos <- getPosition
            st <- readType3
            return $ runSourcePos spos $ interpretNonpolarType st
    return $ do
        etypes <- for mtypes id
        return (consName, assembleListType etypes)

readClosedTypeConstructor :: Parser (PinaforeScoped baseupdate (Name, Anchor, AnyW (ListType ConcreteEntityType)))
readClosedTypeConstructor = do
    consName <- readThis TokUName
    mtypes <-
        many $ do
            spos <- getPosition
            st <- readType3
            return $ runSourcePos spos $ interpretConcreteEntityType st
    anchor <- readThis TokAnchor
    return $ do
        etypes <- for mtypes id
        return (consName, anchor, assembleListType etypes)

data Constructor w t =
    forall a. MkConstructor Name
                            (ListType w a)
                            (HList a -> t)
                            (t -> Maybe (HList a))

extendConstructor :: Constructor w t -> Constructor w (Either a t)
extendConstructor (MkConstructor n lt at tma) = MkConstructor n lt (Right . at) (\t -> eitherRight t >>= tma)

data ClosedEntityBox =
    forall t. MkClosedEntityBox (ClosedEntityType t)
                                [Constructor ConcreteEntityType t]

assembleClosedEntityType :: [(Name, Anchor, AnyW (ListType ConcreteEntityType))] -> ClosedEntityBox
assembleClosedEntityType [] = MkClosedEntityBox NilClosedEntityType []
assembleClosedEntityType ((n, a, MkAnyW el):cc) =
    case assembleClosedEntityType cc of
        MkClosedEntityBox ct conss ->
            MkClosedEntityBox (ConsClosedEntityType a el ct) $
            (MkConstructor n el Left eitherLeft) : fmap extendConstructor conss

data DataBox baseupdate =
    forall t. MkDataBox (PinaforeDataType baseupdate t)
                        [Constructor (PinaforeNonpolarType baseupdate '[]) t]

assembleDataType :: [(Name, AnyW (ListType (PinaforeNonpolarType baseupdate '[])))] -> DataBox baseupdate
assembleDataType [] = MkDataBox NilDataType []
assembleDataType ((n, MkAnyW el):cc) =
    case assembleDataType cc of
        MkDataBox ct conss ->
            MkDataBox (ConsDataType el ct) $ (MkConstructor n el Left eitherLeft) : fmap extendConstructor conss

datatypeIOWitness :: IOWitness ('MkWitKind (IdentifiedType PinaforeDataType))
datatypeIOWitness = $(iowitness [t|'MkWitKind (IdentifiedType PinaforeDataType)|])

constructorFreeVariables :: Constructor (PinaforeNonpolarType baseupdate '[]) t -> [AnyW SymbolType]
constructorFreeVariables (MkConstructor _ lt _ _) = mconcat $ listTypeToList nonPolarTypeFreeVariables lt

readDataTypeDeclaration :: forall baseupdate. Parser (PinaforeScoped baseupdate (TypeDecls baseupdate))
readDataTypeDeclaration = do
    spos <- getPosition
    readThis TokDataType
    name <- readTypeName
    mcons <-
        optional $ do
            readThis TokAssign
            readSeparated1 (readExactlyThis TokOperator "|") $ fmap pure readDataTypeConstructor
    return $ do
        sconss <- sequence $ fromMaybe mempty mcons
        MkDataBox dt conss <- return $ assembleDataType sconss
        runSourcePos spos $ do
            let
                freevars :: [AnyW SymbolType]
                freevars = nub $ mconcat $ fmap constructorFreeVariables conss
                declaredvars :: [AnyW SymbolType]
                declaredvars = [] -- ISSUE #41
                unboundvars :: [AnyW SymbolType]
                unboundvars = freevars List.\\ declaredvars
            case nonEmpty unboundvars of
                Nothing -> return ()
                Just vv -> throw $ InterpretUnboundTypeVariables $ fmap (\(MkAnyW s) -> symbolTypeToName s) vv
            tid <- newTypeID
            valueToWitness tid $ \tidsym -> do
                let pt = MkProvidedType datatypeIOWitness $ MkIdentifiedType tidsym dt
                withnt <- registerTypeName name $ SimpleNamedType NilListType NilDolanVarianceMap (exprShowPrec name) pt
                let
                    ctf :: forall polarity. Is PolarityType polarity
                        => PinaforeShimWit baseupdate polarity _
                    ctf =
                        singlePinaforeShimWit $
                        mkJMShimWit $
                        GroundPinaforeSingularType
                            (SimpleGroundType NilListType NilDolanVarianceMap (exprShowPrec name) pt)
                            NilDolanArguments
                patts <-
                    for conss $ \(MkConstructor cname lt at tma) -> do
                        ltp <- return $ mapListType nonpolarToPinaforeType lt
                        patt <-
                            withNewPatternConstructor cname $
                            toPatternConstructor ctf ltp $ \(MkIdentifiedValue t) -> tma t
                        ltn <- return $ mapListType nonpolarToPinaforeType lt
                        bind <-
                            return $
                            MkWMFunction $
                            withNewBindings $
                            singletonMap cname $
                            qConstExprAny $
                            MkAnyValue (qFunctionPosWitnesses ltn ctf) $ \hl -> MkIdentifiedValue $ at hl
                        return $ patt . bind
                return $ typeTypeDecl $ withnt . compAll patts

readClosedTypeDeclaration :: forall baseupdate. Parser (PinaforeScoped baseupdate (TypeDecls baseupdate))
readClosedTypeDeclaration = do
    spos <- getPosition
    readThis TokClosedType
    name <- readTypeName
    mcons <-
        optional $ do
            readThis TokAssign
            readSeparated1 (readExactlyThis TokOperator "|") $ fmap pure readClosedTypeConstructor
    return $ do
        sconss <- sequence $ fromMaybe mempty mcons
        MkClosedEntityBox ct conss <- return $ assembleClosedEntityType sconss
        runSourcePos spos $ do
            tid <- newTypeID
            withnt <- registerTypeName name $ ClosedEntityNamedType tid (MkAnyW ct)
            valueToWitness tid $ \tidsym -> do
                let
                    ctf :: forall polarity. Is PolarityType polarity
                        => PinaforeShimWit baseupdate polarity (IdentifiedValue _ _)
                    ctf =
                        singlePinaforeShimWit $
                        mkJMShimWit $
                        GroundPinaforeSingularType
                            (EntityPinaforeGroundType NilListType $ ClosedEntityGroundType name tidsym ct)
                            NilDolanArguments
                patts <-
                    for conss $ \(MkConstructor cname lt at tma) -> do
                        ltp <- return $ mapListType (concreteEntityToPositivePinaforeType @baseupdate) lt
                        patt <- withNewPatternConstructor cname $ toPatternConstructor ctf ltp $ tma . unIdentifiedValue
                        ltn <- mapMListType concreteEntityToNegativePinaforeType lt
                        bind <-
                            return $
                            MkWMFunction $
                            withNewBindings $
                            singletonMap cname $
                            qConstExprAny $
                            MkAnyValue (qFunctionPosWitnesses ltn (mapShimWit (coerceEnhanced "consval") ctf)) at
                        return $ patt . bind
                return $ typeTypeDecl $ withnt . compAll patts

readTypeDeclaration :: Parser (PinaforeScoped baseupdate (TypeDecls baseupdate))
readTypeDeclaration =
    (fmap return $ readOpenTypeDeclaration <|> readSubtypeDeclaration) <|> readDataTypeDeclaration <|>
    readClosedTypeDeclaration
