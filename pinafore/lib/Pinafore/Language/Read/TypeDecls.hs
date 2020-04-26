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
import Pinafore.Language.Read.RefNotation
import Pinafore.Language.Read.Token
import Pinafore.Language.Read.Type
import Pinafore.Language.Scope
import Pinafore.Language.Syntax
import Pinafore.Language.Type.Data
import Pinafore.Language.TypeSystem
import Pinafore.Language.TypeSystem.Nonpolar
import Pinafore.Language.TypeSystem.Show
import Shapes hiding (try)

readOpenTypeDeclaration :: forall baseupdate. Parser (TypeDecls baseupdate)
readOpenTypeDeclaration = do
    spos <- getPosition
    readThis TokOpenType
    n <- readTypeName
    let
        tdTypes :: forall a. RefNotation baseupdate a -> RefNotation baseupdate a
        tdTypes rn = do
            (_, withnt) <- liftRefNotation $ runSourcePos spos $ withNewTypeName n $ \tid -> OpenEntityNamedType tid
            remonadRefNotation withnt rn
        tdRelations :: forall a. RefNotation baseupdate a -> RefNotation baseupdate a
        tdRelations = id
    return MkTypeDecls {..}

readSubtypeDeclaration :: forall baseupdate. Parser (TypeDecls baseupdate)
readSubtypeDeclaration = do
    spos <- getPosition
    readThis TokSubtype
    sta <- readType
    readExactlyThis TokOperator "<="
    stb <- readType
    let
        tdTypes :: forall a. RefNotation baseupdate a -> RefNotation baseupdate a
        tdTypes = id
        tdRelations :: forall a. RefNotation baseupdate a -> RefNotation baseupdate a
        tdRelations rn = do
            smap <- liftRefNotation $ runSourcePos spos $ interpretSubtypeRelation sta stb
            remonadRefNotation smap rn
    return MkTypeDecls {..}

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

datatypeIOWitness :: IOWitness ('MkWitKind PinaforeDataType)
datatypeIOWitness = $(iowitness [t|'MkWitKind PinaforeDataType|])

constructorFreeVariables :: Constructor (PinaforeNonpolarType baseupdate '[]) t -> [AnyW SymbolType]
constructorFreeVariables (MkConstructor _ lt _ _) = mconcat $ listTypeToList nonPolarTypeFreeVariables lt

readDataTypeDeclaration :: forall baseupdate. Parser (PinaforeScoped baseupdate (TypeDecls baseupdate))
readDataTypeDeclaration = do
    spos <- getPosition
    readThis TokDataType
    n <- readTypeName
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
                Just vv -> throwError $ InterpretUnboundTypeVariables $ fmap (\(MkAnyW s) -> symbolTypeToName s) vv
            (_, withnt) <-
                withNewTypeName n $ \_ ->
                    SimpleNamedType NilListType NilDolanVarianceMap (exprShowPrec n) $
                    MkProvidedType datatypeIOWitness dt
            let
                ctf :: forall polarity. Is PolarityType polarity
                    => PinaforeShimWit baseupdate polarity _
                ctf =
                    singlePinaforeShimWit $
                    mkJMShimWit $
                    GroundPinaforeSingularType
                        (SimpleGroundType NilListType NilDolanVarianceMap (exprShowPrec n) $
                         MkProvidedType datatypeIOWitness dt)
                        NilDolanArguments
            patts <-
                for conss $ \(MkConstructor cname lt at tma) -> do
                    ltp <- return $ mapListType nonpolarToPinaforeType lt
                    patt <- withNewPatternConstructor cname $ toPatternConstructor ctf ltp tma
                    ltn <- return $ mapListType nonpolarToPinaforeType lt
                    bind <-
                        return $
                        MkWMFunction $
                        withNewBindings $
                        singletonMap cname $ qConstExprAny $ MkAnyValue (qFunctionPosWitnesses ltn ctf) at
                    return $ patt . bind
            let
                tdTypes :: forall a. RefNotation baseupdate a -> RefNotation baseupdate a
                tdTypes = remonadRefNotation $ withnt . compAll patts
                tdRelations :: forall a. RefNotation baseupdate a -> RefNotation baseupdate a
                tdRelations = id
            return $ MkTypeDecls {..}

readClosedTypeDeclaration :: forall baseupdate. Parser (PinaforeScoped baseupdate (TypeDecls baseupdate))
readClosedTypeDeclaration = do
    spos <- getPosition
    readThis TokClosedType
    n <- readTypeName
    mcons <-
        optional $ do
            readThis TokAssign
            readSeparated1 (readExactlyThis TokOperator "|") $ fmap pure readClosedTypeConstructor
    return $ do
        sconss <- sequence $ fromMaybe mempty mcons
        MkClosedEntityBox ct conss <- return $ assembleClosedEntityType sconss
        runSourcePos spos $ do
            (tid, withnt) <- withNewTypeName n $ \tid -> ClosedEntityNamedType tid (MkAnyW ct)
            valueToWitness tid $ \tidsym -> do
                let
                    ctf :: forall polarity. Is PolarityType polarity
                        => PinaforeShimWit baseupdate polarity (IdentifiedValue _ _)
                    ctf =
                        singlePinaforeShimWit $
                        mkJMShimWit $
                        GroundPinaforeSingularType
                            (EntityPinaforeGroundType NilListType $ ClosedEntityGroundType n tidsym ct)
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
                let
                    tdTypes :: forall a. RefNotation baseupdate a -> RefNotation baseupdate a
                    tdTypes = remonadRefNotation $ withnt . compAll patts
                    tdRelations :: forall a. RefNotation baseupdate a -> RefNotation baseupdate a
                    tdRelations = id
                return $ MkTypeDecls {..}

readTypeDeclaration :: Parser (PinaforeScoped baseupdate (TypeDecls baseupdate))
readTypeDeclaration =
    (fmap return $ readOpenTypeDeclaration <|> readSubtypeDeclaration) <|> readDataTypeDeclaration <|>
    readClosedTypeDeclaration
