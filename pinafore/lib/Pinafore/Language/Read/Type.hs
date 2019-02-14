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

readType1 :: Parser SyntaxType
readType1 = do
    t1 <- readType2
    (do
         readThis TokPropMap
         t2 <- readType3
         return $ MorphismSyntaxType t1 t2) <|>
        (do
             readThis TokMap
             t2 <- readType
             return $ FunctionSyntaxType t1 t2) <|>
        return t1

readType2 :: Parser SyntaxType
readType2 =
    (do
         readExactlyThis TokUName "Maybe"
         t1 <- readType3
         return $ MaybeSyntaxType t1) <|>
    (do
         readExactlyThis TokUName "Either"
         t1 <- readType3
         t2 <- readType3
         return $ EitherSyntaxType t1 t2) <|>
    (do
         readExactlyThis TokUName "Ref"
         t1 <- readType3
         return $ RefSyntaxType t1) <|>
    (do
         readExactlyThis TokUName "UI"
         t1 <- readType3
         return $ UISyntaxType t1) <|>
    (do
         readExactlyThis TokUName "Set"
         t1 <- readType3
         return $ SetSyntaxType t1) <|>
    (do
         readExactlyThis TokUName "Action"
         t1 <- readType3
         return $ ActionSyntaxType t1) <|>
    (do
         readExactlyThis TokUName "Order"
         t1 <- readType3
         return $ OrderSyntaxType t1) <|>
    readType3

readType3 :: Parser SyntaxType
readType3 =
    (do
         t1 <- readBracket $ readType
         return $ ListSyntaxType t1) <|>
    (do
         name <- readTypeVar
         return $ VarSyntaxType name) <|>
    readTypeConst <|>
    (readParen $ do
         mt1 <- optional $ readType
         case mt1 of
             Just t1 -> do
                 comma <- optional $ readThis TokComma
                 case comma of
                     Just () -> do
                         t2 <- readType
                         return $ PairSyntaxType t1 t2
                     Nothing -> return t1
             Nothing -> return UnitSyntaxType) <|>
    readTypeRange3

readTypeRange3 :: Parser SyntaxType
readTypeRange3 =
    fmap RangeSyntaxType $
    (readBracketed TokOpenBrace TokCloseBrace $ readCommaList readTypeRangeItem) <|> readSignedType

readTypeRangeItem :: Parser [(Maybe SyntaxVariance, SyntaxType)]
readTypeRangeItem =
    readSignedType <|>
    (do
         t1 <- readType3
         return [(Nothing, t1)])

readSignedType :: Parser [(Maybe SyntaxVariance, SyntaxType)]
readSignedType =
    fmap pure $
    (do
         readExactlyThis TokOperator "+"
         t1 <- readType3
         return (Just CoSyntaxVariance, t1)) <|>
    (do
         readExactlyThis TokOperator "-"
         t1 <- readType3
         return (Just ContraSyntaxVariance, t1))

readTypeVar :: Parser Name
readTypeVar = readThis TokLName

readTypeConst :: Parser SyntaxType
readTypeConst = do
    --spos <- getPosition
    name <- readTypeName
    case name of
        "Any" -> return TopSyntaxType
        "None" -> return BottomSyntaxType
        "Window" -> return WindowSyntaxType
        _ -> return $ ConstSyntaxType name

readTypeName :: Parser Name
readTypeName = readThis TokUName
