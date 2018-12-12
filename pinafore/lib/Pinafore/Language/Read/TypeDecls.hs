module Pinafore.Language.Read.TypeDecls where

import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.RefNotation
import Pinafore.Language.Read.Token
import Pinafore.Language.Read.Type
import Pinafore.Language.Scope
import Pinafore.Language.Type
import Shapes hiding (try)

newtype TypeDecls baseedit =
    MkTypeDecls (forall a. RefNotation baseedit a -> RefNotation baseedit a)

instance Semigroup (TypeDecls baseedit) where
    (MkTypeDecls a) <> (MkTypeDecls b) = MkTypeDecls (a . b)

instance Monoid (TypeDecls baseedit) where
    mempty = MkTypeDecls id
    mappend = (<>)

readOpenTypeDeclaration :: Parser (PinaforeScoped baseedit (TypeDecls baseedit))
readOpenTypeDeclaration = do
    spos <- getPosition
    readThis TokOpenType
    n <- readTypeName
    return $
        runSourcePos spos $ do
            MkTransform withnt <- withNewTypeName n $ EntityNamedType $ toSymbolWitness (unpack n) MkAnyW
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
            MkTransform smap <- withEntitySubtype (na, nb)
            return $ MkTypeDecls $ remonadRefNotation smap

readTypeDeclaration :: Parser (PinaforeScoped baseedit (TypeDecls baseedit))
readTypeDeclaration = readOpenTypeDeclaration <|> readSubtypeDeclaration
