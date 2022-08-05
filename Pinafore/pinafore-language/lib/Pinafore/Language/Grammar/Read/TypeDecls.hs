module Pinafore.Language.Grammar.Read.TypeDecls
    ( readTypeDeclaration
    ) where

import Pinafore.Language.Grammar.Read.Parser
import Pinafore.Language.Grammar.Read.Token
import Pinafore.Language.Grammar.Read.Type
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Name
import Shapes hiding (try)

readOpenEntityTypeDeclaration :: Parser SyntaxRecursiveDeclaration
readOpenEntityTypeDeclaration = do
    spos <- getPosition
    readThis TokOpenType
    name <- readTypeNewName
    return $ TypeSyntaxDeclaration spos name OpenEntitySyntaxTypeDeclaration

readSubtypeDeclaration :: Parser SyntaxRecursiveDeclaration
readSubtypeDeclaration = do
    spos <- getPosition
    readThis TokSubtype
    sta <- readType
    readThis TokSubtypeOf
    stb <- readType
    return $ SubtypeSyntaxDeclaration spos sta stb

readDataTypeConstructor :: Parser SyntaxDatatypeConstructorOrSubtype
readDataTypeConstructor =
    (do
         readThis TokSubtype
         readThis TokDataType
         name <- readTypeNewName
         readThis TokOf
         constructors <- readLines readDataTypeConstructor
         readThis TokEnd
         return $ SubtypeSyntaxConstructorOrSubtype name constructors) <|>
    (do
         consName <- readThis TokUName
         mtypes <- many readType3
         return $ ConstructorSyntaxConstructorOrSubtype consName mtypes ())

readClosedTypeConstructor :: Parser SyntaxClosedEntityConstructorOrSubtype
readClosedTypeConstructor =
    (do
         readThis TokSubtype
         readThis TokClosedType
         name <- readTypeNewName
         readThis TokOf
         constructors <- readLines readClosedTypeConstructor
         readThis TokEnd
         return $ SubtypeSyntaxConstructorOrSubtype name constructors) <|>
    (do
         consName <- readThis TokUName
         mtypes <- many readType3
         anchor <- readThis TokAnchor
         return $ ConstructorSyntaxConstructorOrSubtype consName mtypes anchor)

readPositiveParameter :: Parser Name
readPositiveParameter = do
    readExactlyThis TokOperator "+"
    readTypeVar

readNegativeParameter :: Parser Name
readNegativeParameter = do
    readExactlyThis TokOperator "-"
    readTypeVar

readTypeParameter :: Parser SyntaxTypeParameter
readTypeParameter =
    fmap PositiveSyntaxTypeParameter readPositiveParameter <|> fmap NegativeSyntaxTypeParameter readNegativeParameter <|>
    (readBracketed TokOpenBrace TokCloseBrace $
     (do
          varp <- readNegativeParameter
          readThis TokComma
          varq <- readPositiveParameter
          return $ RangeSyntaxTypeParameter varp varq) <|>
     (do
          varq <- readPositiveParameter
          readThis TokComma
          varp <- readNegativeParameter
          return $ RangeSyntaxTypeParameter varp varq))

readDataTypeDeclaration :: Parser SyntaxRecursiveDeclaration
readDataTypeDeclaration = do
    spos <- getPosition
    readThis TokDataType
    name <- readTypeNewName
    parameters <- many readTypeParameter
    readThis TokOf
    constructors <- readLines readDataTypeConstructor
    readThis TokEnd
    return $ TypeSyntaxDeclaration spos name $ DatatypeSyntaxTypeDeclaration parameters constructors

readClosedTypeDeclaration :: Parser SyntaxRecursiveDeclaration
readClosedTypeDeclaration = do
    spos <- getPosition
    readThis TokClosedType
    name <- readTypeNewName
    parameters <- many readTypeParameter
    readThis TokOf
    constructors <- readLines readClosedTypeConstructor
    readThis TokEnd
    return $ TypeSyntaxDeclaration spos name $ ClosedEntitySyntaxTypeDeclaration parameters constructors

readDynamicTypeConstructor :: Parser SyntaxDynamicEntityConstructor
readDynamicTypeConstructor =
    fmap AnchorSyntaxDynamicEntityConstructor (readThis TokAnchor) <|>
    fmap NameSyntaxDynamicEntityConstructor readTypeReferenceName

readDynamicTypeDeclaration :: Parser SyntaxRecursiveDeclaration
readDynamicTypeDeclaration = do
    spos <- getPosition
    readThis TokDynamicType
    name <- readTypeNewName
    readThis TokAssign
    tcons <- readSeparated1 (readThis TokOr) $ fmap pure readDynamicTypeConstructor
    return $ TypeSyntaxDeclaration spos name $ DynamicEntitySyntaxTypeDeclaration tcons

readTypeDeclaration :: Parser SyntaxRecursiveDeclaration
readTypeDeclaration =
    readOpenEntityTypeDeclaration <|> readSubtypeDeclaration <|> readDataTypeDeclaration <|> readClosedTypeDeclaration <|>
    readDynamicTypeDeclaration
