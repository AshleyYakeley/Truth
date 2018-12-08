module Pinafore.Language.Read.TypeDecls where

import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.RefNotation
import Pinafore.Language.Read.Token
import Pinafore.Language.Read.Type
import Pinafore.Language.Scope
import Shapes hiding (try)

newtype TypeDecls baseedit =
    MkTypeDecls (forall a. RefNotation baseedit a -> RefNotation baseedit a)

instance Semigroup (TypeDecls baseedit) where
    (MkTypeDecls a) <> (MkTypeDecls b) = MkTypeDecls (a . b)

instance Monoid (TypeDecls baseedit) where
    mempty = MkTypeDecls id
    mappend = (<>)

readEntityDeclaration :: Parser (TypeDecls baseedit)
readEntityDeclaration = do
    spos <- getPosition
    readThis TokEntity
    n <- readTypeName
    return $
        MkTypeDecls $
        remonadRefNotation $ mapSourcePos spos $ withNewTypeName n $ EntityNamedType $ toSymbolWitness (unpack n) MkAnyW

readSubtypeDeclaration :: Parser (TypeDecls baseedit)
readSubtypeDeclaration = do
    spos <- getPosition
    readThis TokSubtype
    na <- readTypeName
    readExactlyThis TokOperator "<="
    nb <- readTypeName
    return $ MkTypeDecls $ remonadRefNotation $ mapSourcePos spos $ withEntitySubtype (na, nb)

readTypeDeclaration :: Parser (TypeDecls baseedit)
readTypeDeclaration = readEntityDeclaration <|> readSubtypeDeclaration
