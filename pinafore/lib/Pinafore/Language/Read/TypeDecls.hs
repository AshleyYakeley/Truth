module Pinafore.Language.Read.TypeDecls
    ( readTypeDeclaration
    ) where

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
import Pinafore.Language.Type
import Shapes hiding (try)

readOpenTypeDeclaration :: forall baseedit. Parser (TypeDecls baseedit)
readOpenTypeDeclaration = do
    spos <- getPosition
    readThis TokOpenType
    n <- readTypeName
    let
        tdTypes :: forall a. RefNotation baseedit a -> RefNotation baseedit a
        tdTypes rn = do
            (_, withnt) <- liftRefNotation $ runSourcePos spos $ withNewTypeName n $ OpenEntityNamedType
            remonadRefNotation withnt rn
        tdRelations :: forall a. RefNotation baseedit a -> RefNotation baseedit a
        tdRelations = id
    return MkTypeDecls {..}

readSubtypeDeclaration :: forall baseedit. Parser (TypeDecls baseedit)
readSubtypeDeclaration = do
    spos <- getPosition
    readThis TokSubtype
    na <- readTypeName
    readExactlyThis TokOperator "<="
    nb <- readTypeName
    let
        tdTypes :: forall a. RefNotation baseedit a -> RefNotation baseedit a
        tdTypes = id
        tdRelations :: forall a. RefNotation baseedit a -> RefNotation baseedit a
        tdRelations rn = do
            smap <- liftRefNotation $ runSourcePos spos $ withEntitySubtype (na, nb)
            remonadRefNotation smap rn
    return MkTypeDecls {..}

readClosedTypeConstructor :: Parser (PinaforeScoped baseedit (Name, Anchor, AnyW (ListType EntityType)))
readClosedTypeConstructor = do
    consName <- readThis TokUName
    mtypes <-
        many $ do
            spos <- getPosition
            st <- readType3
            return $ runSourcePos spos $ interpretEntityType st
    anchor <- readThis TokAnchor
    return $ do
        etypes <- for mtypes id
        return (consName, anchor, assembleListType etypes)

data Constructor t =
    forall a. MkConstructor Name
                            (ListType EntityType a)
                            (HList a -> t)
                            (t -> Maybe (HList a))

extendConstructor :: Constructor t -> Constructor (Either a t)
extendConstructor (MkConstructor n lt at tma) = MkConstructor n lt (Right . at) (\t -> eitherRight t >>= tma)

data Box =
    forall t. MkBox (ClosedEntityType t)
                    [Constructor t]

assembleClosedEntityType :: [(Name, Anchor, AnyW (ListType EntityType))] -> Box
assembleClosedEntityType [] = MkBox NilClosedEntityType []
assembleClosedEntityType ((n, a, MkAnyW el):cc) =
    case assembleClosedEntityType cc of
        MkBox ct conss ->
            MkBox (ConsClosedEntityType a el ct) $ (MkConstructor n el Left eitherLeft) : fmap extendConstructor conss

makeConstructorPattern ::
       forall baseedit s t lt.
       PinaforeShimWit baseedit 'Negative (ClosedEntity s t)
    -> ListType EntityType lt
    -> (t -> Maybe (HList lt))
    -> PinaforePatternConstructor baseedit
makeConstructorPattern pct lt tma =
    case mapListType (entityToPositivePinaforeType @baseedit) lt of
        lt' -> toPatternConstructor pct lt' $ tma . unClosedEntity

makeConstructorValue ::
       forall baseedit m s t a. MonadError ErrorType m
    => PinaforeShimWit baseedit 'Positive (ClosedEntity s t)
    -> ListType EntityType a
    -> m (PinaforeShimWit baseedit 'Positive (HList a -> t))
makeConstructorValue ctf lt = do
    lt' <- mapMListType entityToNegativePinaforeType lt
    return $ qFunctionPosWitnesses lt' (mapShimWit coerceEnhanced ctf)

readClosedTypeDeclaration :: forall baseedit. Parser (PinaforeScoped baseedit (TypeDecls baseedit))
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
        MkBox ct conss <- return $ assembleClosedEntityType sconss
        runSourcePos spos $ do
            (tid, withnt) <- withNewTypeName n $ ClosedEntityNamedType (MkAnyW ct)
            valueToWitness tid $ \tidsym -> do
                let
                    ctf :: forall polarity. Is PolarityType polarity
                        => PinaforeShimWit baseedit polarity (ClosedEntity _ _)
                    ctf =
                        singlePinaforeShimWit $
                        mkJMShimWit $
                        GroundPinaforeSingularType
                            (EntityPinaforeGroundType NilListType $ ClosedEntityGroundType n tidsym ct)
                            NilDolanArguments
                patts <-
                    for conss $ \(MkConstructor cname lt at tma) -> do
                        patt <- withNewPatternConstructor cname $ makeConstructorPattern ctf lt tma
                        valt <- makeConstructorValue ctf lt
                        bind <-
                            return $
                            MkTransform $ withNewBindings $ singletonMap cname $ qConstExprAny $ MkAnyValue valt at
                        return $ patt . bind
                let
                    tdTypes :: forall a. RefNotation baseedit a -> RefNotation baseedit a
                    tdTypes = remonadRefNotation $ withnt . compAll patts
                    tdRelations :: forall a. RefNotation baseedit a -> RefNotation baseedit a
                    tdRelations = id
                return $ MkTypeDecls {..}

readTypeDeclaration :: Parser (PinaforeScoped baseedit (TypeDecls baseedit))
readTypeDeclaration = (fmap return $ readOpenTypeDeclaration <|> readSubtypeDeclaration) <|> readClosedTypeDeclaration
