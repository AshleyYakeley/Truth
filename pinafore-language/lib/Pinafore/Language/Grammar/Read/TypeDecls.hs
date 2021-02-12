module Pinafore.Language.Grammar.Read.TypeDecls
    ( readTypeDeclaration
    ) where

import Pinafore.Language.Grammar.Read.Parser
import Pinafore.Language.Grammar.Read.Token
import Pinafore.Language.Grammar.Read.Type
import Pinafore.Language.Grammar.Syntax
import Shapes hiding (try)

readOpenEntityTypeDeclaration :: Parser SyntaxDeclaration
readOpenEntityTypeDeclaration = do
    spos <- getPosition
    readThis TokOpenType
    name <- readTypeNewName
    return $ TypeSyntaxDeclaration spos name OpenEntitySyntaxTypeDeclaration

readSubtypeDeclaration :: Parser SyntaxDeclaration
readSubtypeDeclaration = do
    spos <- getPosition
    readThis TokSubtype
    sta <- readType
    readExactlyThis TokOperator "<:"
    stb <- readType
    return $ SubtypeDeclaration spos sta stb

readDataTypeConstructor :: Parser SyntaxDatatypeConstructor
readDataTypeConstructor = do
    consName <- readThis TokUName
    mtypes <- many readType3
    return $ MkSyntaxDatatypeConstructor consName mtypes

readClosedTypeConstructor :: Parser SyntaxClosedEntityConstructor
readClosedTypeConstructor = do
    consName <- readThis TokUName
    mtypes <- many readType3
    anchor <- readThis TokAnchor
    return $ MkSyntaxClosedEntityConstructor consName mtypes anchor

readDataTypeDeclaration :: Parser SyntaxDeclaration
readDataTypeDeclaration = do
    spos <- getPosition
    readThis TokDataType
    name <- readTypeNewName
    mcons <-
        optional $ do
            readThis TokAssign
            readSeparated1 (readExactlyThis TokOperator "|") $ fmap pure readDataTypeConstructor
    return $ TypeSyntaxDeclaration spos name $ DatatypeSyntaxTypeDeclaration $ fromMaybe mempty mcons

readClosedTypeDeclaration :: Parser SyntaxDeclaration
readClosedTypeDeclaration = do
    spos <- getPosition
    readThis TokClosedType
    name <- readTypeNewName
    mcons <-
        optional $ do
            readThis TokAssign
            readSeparated1 (readExactlyThis TokOperator "|") $ fmap pure readClosedTypeConstructor
    return $ TypeSyntaxDeclaration spos name $ ClosedEntitySyntaxTypeDeclaration $ fromMaybe mempty mcons

readDynamicTypeConstructor :: Parser SyntaxDynamicEntityConstructor
readDynamicTypeConstructor =
    fmap AnchorSyntaxDynamicEntityConstructor (readThis TokAnchor) <|>
    fmap NameSyntaxDynamicEntityConstructor readTypeReferenceName

readDynamicTypeDeclaration :: Parser SyntaxDeclaration
readDynamicTypeDeclaration = do
    spos <- getPosition
    readThis TokDynamicType
    name <- readTypeNewName
    readThis TokAssign
    tcons <- readSeparated1 (readExactlyThis TokOperator "|") $ fmap pure readDynamicTypeConstructor
    return $ TypeSyntaxDeclaration spos name $ DynamicEntitySyntaxTypeDeclaration tcons

readTypeDeclaration :: Parser SyntaxDeclaration
readTypeDeclaration =
    readOpenEntityTypeDeclaration <|> readSubtypeDeclaration <|> readDataTypeDeclaration <|> readClosedTypeDeclaration <|>
    readDynamicTypeDeclaration