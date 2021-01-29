module Pinafore.Language.Grammar.Read.Type
    ( readTypeReferenceName
    , readTypeNewName
    , readType
    , readType3
    ) where

import Pinafore.Language.Grammar.Read.Parser
import Pinafore.Language.Grammar.Read.Token
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Name
import Shapes hiding (try)

readType :: Parser SyntaxType
readType = do
    spos <- getPosition
    (do
         readThis TokRec
         n <- readTypeVar
         readExactlyThis TokOperator "."
         t <- readType
         return $ MkWithSourcePos spos $ RecursiveSyntaxType n t) <|>
        readType0

readType0 :: Parser SyntaxType
readType0 = do
    spos <- getPosition
    t1 <- readType1
    (do
         readExactlyThis TokOperator "|"
         t2 <- readType
         return $ MkWithSourcePos spos $ OrSyntaxType t1 t2) <|>
        (do
             readExactlyThis TokOperator "&"
             t2 <- readType
             return $ MkWithSourcePos spos $ AndSyntaxType t1 t2) <|>
        (return t1)

readInfix :: Parser SyntaxGroundType
readInfix =
    (do
         readThis TokPropMap
         return MorphismSyntaxGroundType) <|>
    (do
         readThis TokMap
         return FunctionSyntaxGroundType)

readType1 :: Parser SyntaxType
readType1 =
    (try $
     readSourcePos $ do
         t1 <- readTypeArgument readType2
         tc <- readInfix
         t2 <- readTypeArgument readType1
         return $ SingleSyntaxType tc [t1, t2]) <|>
    readType2

readTypeReferenceName :: Parser ReferenceName
readTypeReferenceName = readReferenceUName

readTypeNewName :: Parser Name
readTypeNewName = readThis TokUName

readTypeConstant :: Parser SyntaxGroundType
readTypeConstant = do
    name <- readTypeReferenceName
    return $ ConstSyntaxGroundType name

readTypeArgument :: Parser SyntaxType -> Parser SyntaxTypeArgument
readTypeArgument r = fmap SimpleSyntaxTypeArgument r <|> readTypeRange

readType2 :: Parser SyntaxType
readType2 =
    (try $
     readSourcePos $ do
         tc <- readTypeConstant
         tt <- some $ readTypeArgument readType3
         return $ SingleSyntaxType tc tt) <|>
    readType3

readType3 :: Parser SyntaxType
readType3 =
    (readSourcePos $ do
         t1 <- readBracket $ readType
         return $ SingleSyntaxType ListSyntaxGroundType [SimpleSyntaxTypeArgument t1]) <|>
    (readSourcePos $ do
         name <- readTypeVar
         return $ VarSyntaxType name) <|>
    readTypeLimit <|>
    (readSourcePos $ do
         tc <- readTypeConstant
         return $ SingleSyntaxType tc []) <|>
    (readParen $ do
         mt1 <- optional $ readType
         case mt1 of
             Just t1 -> do
                 comma <- optional $ readThis TokComma
                 case comma of
                     Just () ->
                         readSourcePos $ do
                             t2 <- readType
                             return $
                                 SingleSyntaxType
                                     PairSyntaxGroundType
                                     [SimpleSyntaxTypeArgument t1, SimpleSyntaxTypeArgument t2]
                     Nothing -> return t1
             Nothing -> readSourcePos $ return $ SingleSyntaxType UnitSyntaxGroundType [])

readTypeRange :: Parser SyntaxTypeArgument
readTypeRange =
    (readBracketed TokOpenBrace TokCloseBrace $ do
         items <- readCommaList readTypeRangeItem
         return $ RangeSyntaxTypeArgument items) <|> do
        (sv, t) <- readSignedType
        return $ RangeSyntaxTypeArgument [(Just sv, t)]

readTypeRangeItem :: Parser [(Maybe SyntaxVariance, SyntaxType)]
readTypeRangeItem =
    (do
         (sv, t) <- readSignedType
         return [(Just sv, t)]) <|>
    (do
         t1 <- readType3
         return [(Nothing, t1)])

readSignedType :: Parser (SyntaxVariance, SyntaxType)
readSignedType =
    (do
         readExactlyThis TokOperator "+"
         t1 <- readType3
         return (CoSyntaxVariance, t1)) <|>
    (do
         readExactlyThis TokOperator "-"
         t1 <- readType3
         return (ContraSyntaxVariance, t1))

readTypeVar :: Parser Name
readTypeVar = readThis TokLName

readTypeLimit :: Parser SyntaxType
readTypeLimit =
    readSourcePos $
    (do
         readExactlyThis TokUName "Any"
         return TopSyntaxType) <|>
    (do
         readExactlyThis TokUName "None"
         return BottomSyntaxType)
