module Pinafore.Language.Grammar.Read.Type
    ( readTypeFullNameRef
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
readTypeOperatorName = do
    n <- readThis TokOperator
    altIf $ allowedTypeOperatorName n
    return n

readInfix :: Parser (Name, Fixity, SyntaxTypeArgument -> SyntaxTypeArgument -> SyntaxTypeArgument)
readInfix = do
    spos <- getPosition
    name <- readTypeOperatorName
    return
        ( name
        , typeOperatorFixity name
        , \t1 t2 ->
              SimpleSyntaxTypeArgument $
              MkWithSourcePos spos $ SingleSyntaxType (ConstSyntaxGroundType $ UnqualifiedFullNameRef name) [t1, t2])

typeFixityReader :: FixityReader SyntaxTypeArgument
typeFixityReader = MkFixityReader {efrReadInfix = readInfix, efrMaxPrecedence = 3}

readType1 :: Parser SyntaxType
readType1 = do
    arg <- readInfixed typeFixityReader $ readTypeArgument readType2
    case arg of
        SimpleSyntaxTypeArgument t -> return t
        _ -> empty

readTypeFullNameRef :: Parser FullNameRef
readTypeFullNameRef = readFullUName

readTypeNewName :: Parser Name
readTypeNewName = readUName

readTypeConstant :: Parser SyntaxGroundType
readTypeConstant = do
    name <- readTypeFullNameRef
    return $ ConstSyntaxGroundType name

readTypeArgument :: Parser SyntaxType -> Parser SyntaxTypeArgument
readTypeArgument r =
    asum
        [ fmap SimpleSyntaxTypeArgument r
        , readBracketed TokOpenBrace TokCloseBrace $ do
              items <- readCommaM readTypeRangeItem
              return $ RangeSyntaxTypeArgument items
        , do
              (sv, t) <- readSignedType readType3
              return $ RangeSyntaxTypeArgument [(Just sv, t)]
        ]

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
         name <- readTypeVar
         return $ VarSyntaxType name) <|>
    readTypeLimit <|>
    (readSourcePos $ do
         tc <- readTypeConstant
         return $ SingleSyntaxType tc []) <|>
    (readParen readType)

readTypeRangeItem :: Parser [(Maybe SyntaxVariance, SyntaxType)]
readTypeRangeItem =
    (do
         (sv, t) <- readSignedType readType
         return [(Just sv, t)]) <|>
    (do
         t1 <- readType
         return [(Nothing, t1)])

readSignedType :: Parser SyntaxType -> Parser (SyntaxVariance, SyntaxType)
readSignedType rtype =
    (do
         readExactlyThis TokOperator "+"
         t1 <- rtype
         return (CoSyntaxVariance, t1)) <|>
    (do
         readExactlyThis TokOperator "-"
         t1 <- rtype
         return (ContraSyntaxVariance, t1))

readTypeVar :: Parser Name
readTypeVar = readLName

readTypeLimit :: Parser SyntaxType
readTypeLimit =
    readSourcePos $
    (do
         readExactly readUName "Any"
         return TopSyntaxType) <|>
    (do
         readExactly readUName "None"
         return BottomSyntaxType)
