module Pinafore.Language.Grammar.Read.Type
    ( readTypeReferenceName
    , readTypeNewName
    , readType
    , readType3
    , readTypeVar
    ) where

import Pinafore.Language.ExprShow
import Pinafore.Language.Grammar.Read.Infix
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
         readThis TokOr
         t2 <- readType
         return $ MkWithSourcePos spos $ OrSyntaxType t1 t2) <|>
        (do
             readThis TokAnd
             t2 <- readType
             return $ MkWithSourcePos spos $ AndSyntaxType t1 t2) <|>
        (return t1)

allowedTypeOperatorName :: Name -> Bool
allowedTypeOperatorName "+" = False
allowedTypeOperatorName "-" = False
allowedTypeOperatorName "|" = False
allowedTypeOperatorName "&" = False
allowedTypeOperatorName _ = True

readTypeOperatorName :: Parser Name
readTypeOperatorName =
    (readThis TokMap >> return "->") <|> do
        n <- readThis TokOperator
        ifpure (allowedTypeOperatorName n) n

readInfix :: Parser (Name, Fixity, SyntaxTypeArgument -> SyntaxTypeArgument -> SyntaxTypeArgument)
readInfix = do
    spos <- getPosition
    name <- readTypeOperatorName
    return
        ( name
        , typeOperatorFixity name
        , \t1 t2 ->
              SimpleSyntaxTypeArgument $
              MkWithSourcePos spos $ SingleSyntaxType (ConstSyntaxGroundType $ UnqualifiedReferenceName name) [t1, t2])

typeFixityReader :: FixityReader SyntaxTypeArgument
typeFixityReader = MkFixityReader {efrReadInfix = readInfix, efrMaxPrecedence = 3}

readType1 :: Parser SyntaxType
readType1 = do
    arg <- readInfixed typeFixityReader $ readTypeArgument readType2
    case arg of
        SimpleSyntaxTypeArgument t -> return t
        _ -> empty

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
         items <- readCommaM readTypeRangeItem
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
