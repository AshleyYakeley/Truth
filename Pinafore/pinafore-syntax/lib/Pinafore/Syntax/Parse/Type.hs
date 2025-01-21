module Pinafore.Syntax.Parse.Type
    ( readTypeFullNameRef
    , readTypeNewName
    , readType
    , readType3
    , readTypeVar
    )
where

import Shapes hiding (try)

import Pinafore.Syntax.Name
import Pinafore.Syntax.Parse.Basic
import Pinafore.Syntax.Parse.Infix
import Pinafore.Syntax.Parse.Parser
import Pinafore.Syntax.Parse.Token
import Pinafore.Syntax.Syntax

readType :: Parser SyntaxType
readType = do
    spos <- getPosition
    ( do
            readThis TokRec
            n <- readTypeVar
            readThis TokComma
            t <- readType
            return $ MkWithSourcePos spos $ RecursiveSyntaxType n t
        )
        <|> readType0

readType0 :: Parser SyntaxType
readType0 = do
    spos <- getPosition
    t1 <- readType1
    ( do
            readThis TokOr
            t2 <- readType
            return $ MkWithSourcePos spos $ OrSyntaxType t1 t2
        )
        <|> ( do
                readThis TokAnd
                t2 <- readType
                return $ MkWithSourcePos spos $ AndSyntaxType t1 t2
            )
        <|> (return t1)

allowedTypeOperatorName :: Name -> Bool
allowedTypeOperatorName "+" = False
allowedTypeOperatorName "-" = False
allowedTypeOperatorName "|" = False
allowedTypeOperatorName "&" = False
allowedTypeOperatorName _ = True

readTypeOperatorName :: Parser (FullNameRef, Fixity)
readTypeOperatorName = do
    names <- readThis TokOperator
    let name = tnName names
    guard $ allowedTypeOperatorName name
    return (tokenNamesToFullNameRef names, typeOperatorFixity name)

readInfix :: Parser (FullNameRef, Fixity, SyntaxTypeArgument -> SyntaxTypeArgument -> SyntaxTypeArgument)
readInfix = do
    spos <- getPosition
    (name, fixity) <- readTypeOperatorName
    return
        ( name
        , fixity
        , \t1 t2 ->
            SimpleSyntaxTypeArgument $ MkWithSourcePos spos $ SingleSyntaxType (ConstSyntaxGroundType name) [t1, t2]
        )

typeFixityReader :: FixityReader SyntaxTypeArgument
typeFixityReader = MkFixityReader{efrReadInfix = readInfix, efrMaxPrecedence = 3}

readType1 :: Parser SyntaxType
readType1 = do
    arg <- readInfixed typeFixityReader $ readTypeArgument readType2
    case arg of
        SimpleSyntaxTypeArgument t -> return t
        _ -> empty

readTypeFullNameRef :: Parser FullNameRef
readTypeFullNameRef = readFullUName

readTypeNewName :: Parser FullName
readTypeNewName = readNewUName

readTypeConstant :: Parser SyntaxGroundType
readTypeConstant = do
    name <- readTypeFullNameRef
    return $ ConstSyntaxGroundType name

readTypeArgument :: Parser SyntaxType -> Parser SyntaxTypeArgument
readTypeArgument r =
    asum
        [ readParen $ do
            items <- readCommaM readTypeRangeItem
            return $ MkSyntaxTypeArgument items
        , do
            (sv, t) <- readSignedType readType3
            return $ MkSyntaxTypeArgument [(Just sv, t)]
        , fmap SimpleSyntaxTypeArgument r
        ]

readType2 :: Parser SyntaxType
readType2 =
    ( try
        $ readWithSourcePos
        $ do
            tc <- readTypeConstant
            tt <- some $ readTypeArgument readType3
            return $ SingleSyntaxType tc tt
    )
        <|> readType3

readType3 :: Parser SyntaxType
readType3 =
    ( readWithSourcePos $ do
        name <- readTypeVar
        return $ VarSyntaxType name
    )
        <|> readTypeLimit
        <|> ( readWithSourcePos $ do
                tc <- readTypeConstant
                return $ SingleSyntaxType tc []
            )
        <|> (readParen readType)

readTypeRangeItem :: Parser [(Maybe SyntaxVariance, SyntaxType)]
readTypeRangeItem =
    ( do
        (sv, t) <- readSignedType readType
        return [(Just sv, t)]
    )
        <|> ( do
                t1 <- readType
                return [(Nothing, t1)]
            )

readSignedType :: Parser SyntaxType -> Parser (SyntaxVariance, SyntaxType)
readSignedType rtype =
    ( do
        readExactlyThis TokOperator "+"
        t1 <- rtype
        return (CoSyntaxVariance, t1)
    )
        <|> ( do
                readExactlyThis TokOperator "-"
                t1 <- rtype
                return (ContraSyntaxVariance, t1)
            )

readTypeVar :: Parser Name
readTypeVar = readLName

readTypeLimit :: Parser SyntaxType
readTypeLimit =
    readWithSourcePos
        $ ( do
                readExactly readUName "Any"
                return TopSyntaxType
          )
        <|> ( do
                readExactly readUName "None"
                return BottomSyntaxType
            )
