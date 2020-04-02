module Pinafore.Language.Read.Type
    ( readTypeName
    , readType
    , readType3
    ) where

import Pinafore.Language.Name
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Token
import Pinafore.Language.Syntax
import Shapes hiding (try)

readType :: Parser SyntaxType
readType = do
    t1 <- readType1
    (do
         readExactlyThis TokOperator "|"
         t2 <- readType
         return $ OrSyntaxType t1 t2) <|>
        (do
             readExactlyThis TokOperator "&"
             t2 <- readType
             return $ AndSyntaxType t1 t2) <|>
        return t1

readInfix :: Parser SyntaxGroundType
readInfix =
    (do
         readThis TokPropMap
         return MorphismSyntaxGroundType) <|>
    (do
         readThis TokMap
         return FunctionSyntaxGroundType)

readType1 :: Parser SyntaxType
readType1 = do
    (try $ do
         t1 <- readTypeArgument readType2
         tc <- readInfix
         t2 <- readTypeArgument readType1
         return $ SingleSyntaxType tc [t1, t2]) <|>
        readType2

readTypeName :: Parser Name
readTypeName = readThis TokUName

readTypeConstant :: Parser SyntaxGroundType
readTypeConstant = do
    name <- readTypeName
    return $ ConstSyntaxGroundType name

readTypeArgument :: Parser SyntaxType -> Parser SyntaxTypeArgument
readTypeArgument r = fmap SimpleSyntaxTypeArgument r <|> readTypeRange

readType2 :: Parser SyntaxType
readType2 =
    (try $ do
         tc <- readTypeConstant
         tt <- some $ readTypeArgument readType3
         return $ SingleSyntaxType tc tt) <|>
    readType3

readType3 :: Parser SyntaxType
readType3 =
    (do
         t1 <- readBracket $ readType
         return $ SingleSyntaxType ListSyntaxGroundType [SimpleSyntaxTypeArgument t1]) <|>
    (do
         name <- readTypeVar
         return $ VarSyntaxType name) <|>
    readTypeLimit <|>
    (do
         tc <- readTypeConstant
         return $ SingleSyntaxType tc []) <|>
    (readParen $ do
         mt1 <- optional $ readType
         case mt1 of
             Just t1 -> do
                 comma <- optional $ readThis TokComma
                 case comma of
                     Just () -> do
                         t2 <- readType
                         return $
                             SingleSyntaxType
                                 PairSyntaxGroundType
                                 [SimpleSyntaxTypeArgument t1, SimpleSyntaxTypeArgument t2]
                     Nothing -> return t1
             Nothing -> return $ SingleSyntaxType UnitSyntaxGroundType [])

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
    (do
         readExactlyThis TokUName "Any"
         return TopSyntaxType) <|>
    (do
         readExactlyThis TokUName "None"
         return BottomSyntaxType)
