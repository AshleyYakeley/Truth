module Pinafore.Language.Read.TypeDecls
    ( readTypeDeclaration
    ) where

import Data.Shim
import Language.Expression.Sealed
import Pinafore.Base
import Pinafore.Language.Expression
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Name
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.RefNotation
import Pinafore.Language.Read.Token
import Pinafore.Language.Read.Type
import Pinafore.Language.Scope
import Pinafore.Language.Syntax
import Pinafore.Language.TypeSystem
import Shapes hiding (try)

readOpenTypeDeclaration :: forall baseupdate. Parser (TypeDecls baseupdate)
readOpenTypeDeclaration = do
    spos <- getPosition
    readThis TokOpenType
    n <- readTypeName
    let
        tdTypes :: forall a. RefNotation baseupdate a -> RefNotation baseupdate a
        tdTypes rn = do
            (_, withnt) <- liftRefNotation $ runSourcePos spos $ withNewTypeName n $ OpenEntityNamedType
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

makeConstructorPattern ::
       forall baseupdate s t lt.
       PinaforeShimWit baseupdate 'Negative (IdentifiedValue s t)
    -> ListType (PinaforeShimWit baseupdate 'Positive) lt
    -> (t -> Maybe (HList lt))
    -> PinaforePatternConstructor baseupdate
makeConstructorPattern pct lt tma = toPatternConstructor pct lt $ tma . unIdentifiedValue

makeConstructorValue ::
       forall baseupdate s t a.
       PinaforeShimWit baseupdate 'Positive (IdentifiedValue s t)
    -> ListType (PinaforeShimWit baseupdate 'Negative) a
    -> PinaforeShimWit baseupdate 'Positive (HList a -> t)
makeConstructorValue ctf lt = qFunctionPosWitnesses lt (mapShimWit (coerceEnhanced "consval") ctf)

readDataTypeDeclaration :: forall baseupdate. Parser (PinaforeScoped baseupdate (TypeDecls baseupdate))
readDataTypeDeclaration = empty

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
            (tid, withnt) <- withNewTypeName n $ ClosedEntityNamedType (MkAnyW ct)
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
                        patt <- withNewPatternConstructor cname $ makeConstructorPattern ctf ltp tma
                        ltn <- mapMListType concreteEntityToNegativePinaforeType lt
                        bind <-
                            return $
                            MkWMFunction $
                            withNewBindings $
                            singletonMap cname $ qConstExprAny $ MkAnyValue (makeConstructorValue ctf ltn) at
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
