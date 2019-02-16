module Pinafore.Language.Read.TypeDecls
    ( readTypeDeclaration
    ) where

import Language.Expression.Pattern
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
import Pinafore.Language.Type
import Pinafore.Language.TypeID
import Shapes hiding (try)

readOpenTypeDeclaration :: Parser (PinaforeScoped baseedit (TypeDecls baseedit))
readOpenTypeDeclaration = do
    spos <- getPosition
    readThis TokOpenType
    n <- readTypeName
    return $
        runSourcePos spos $ do
            (_, withnt) <- withNewTypeName n $ OpenEntityNamedType
            return $ MkTypeDecls $ remonadRefNotation withnt

readSubtypeDeclaration :: Parser (PinaforeScoped baseedit (TypeDecls baseedit))
readSubtypeDeclaration = do
    spos <- getPosition
    readThis TokSubtype
    na <- readTypeName
    readExactlyThis TokOperator "<="
    nb <- readTypeName
    return $
        runSourcePos spos $ do
            smap <- withEntitySubtype (na, nb)
            return $ MkTypeDecls $ remonadRefNotation smap

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
       forall baseedit s t a.
       PinaforeTypeF baseedit 'Negative (ClosedEntity s t)
    -> ListType EntityType a
    -> (t -> Maybe (HList a))
    -> PinaforePatternConstructor baseedit
makeConstructorPattern (MkTypeF pct conv) lt tma =
    case hlistTypeF $ mapListType (entityToPositivePinaforeType @baseedit) lt of
        MkTypeF (MkHListWit lt') conv' ->
            MkPatternConstructor pct lt' $ ClosedPattern $ fmap conv' . tma . unClosedEntity . conv

makeConstructorValue ::
       forall baseedit m s t a. MonadFail m
    => PinaforeTypeF baseedit 'Positive (ClosedEntity s t)
    -> ListType EntityType a
    -> m (PinaforeTypeF baseedit 'Positive (HList a -> t))
makeConstructorValue ctf lt = do
    lt' <- mapMListType entityToNegativePinaforeType lt
    return $ qFunctionPosWitnesses lt' (contramap MkClosedEntity ctf)

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
            typeIdToSymbolType tid $ \tidsym -> do
                let
                    ctf :: forall polarity. Is PolarityType polarity
                        => PinaforeTypeF baseedit polarity (ClosedEntity _ _)
                    ctf =
                        singlePinaforeTypeF $
                        mkTypeF $
                        GroundPinaforeSingularType
                            (EntityPinaforeGroundType NilListType $ ClosedEntityGroundType n tidsym ct)
                            NilDolanArguments
                patts <-
                    for conss $ \(MkConstructor cname lt at tma) -> do
                        patt <- withNewPatternConstructor cname $ makeConstructorPattern ctf lt tma
                        valt <- makeConstructorValue ctf lt
                        bind <-
                            return $
                            MkTransform $ withNewBindings $ singletonMap cname $ qConstExprAny $ toTypeFAnyValue valt at
                        return $ patt . bind
                return $ MkTypeDecls $ remonadRefNotation $ withnt . compAll patts

readTypeDeclaration :: Parser (PinaforeScoped baseedit (TypeDecls baseedit))
readTypeDeclaration = readOpenTypeDeclaration <|> readSubtypeDeclaration <|> readClosedTypeDeclaration
