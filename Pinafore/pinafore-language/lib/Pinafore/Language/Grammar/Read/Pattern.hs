module Pinafore.Language.Grammar.Read.Pattern
    ( readPatterns
    , readPattern1
    ) where

import Pinafore.Language.Grammar.Read.Constructor
import Pinafore.Language.Grammar.Read.Parser
import Pinafore.Language.Grammar.Read.Token
import Pinafore.Language.Grammar.Read.Type
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Name
import Shapes hiding (try)

readPatterns :: Parser [SyntaxPattern]
readPatterns = many readPattern4

nilPattern :: SyntaxPattern'
nilPattern = ConstructorSyntaxPattern (SLNamedConstructor $ MkFullNameRef RootNamespaceRef "[]") []

consPattern :: SyntaxPattern -> SyntaxPattern -> SyntaxPattern'
consPattern pat1 pat2 = ConstructorSyntaxPattern (SLNamedConstructor $ MkFullNameRef RootNamespaceRef "::") [pat1, pat2]

modifyPattern1 :: SyntaxPattern -> Parser SyntaxPattern
modifyPattern1 pat =
    (do
         readThis TokTypeJudge
         t <- readType
         readSourcePos $ return $ TypedSyntaxPattern pat t) <|>
    (do
         readThis TokTypeDynamic
         t <- readType
         readSourcePos $ return $ DynamicTypedSyntaxPattern pat t) <|>
    (do
         readThis TokAs
         nn <- readNamespaceRef
         readSourcePos $ return $ NamespaceSyntaxPattern pat nn)

readPattern1 :: Parser SyntaxPattern
readPattern1 = do
    pat <- readPattern2
    chainModify modifyPattern1 pat

readPattern2 :: Parser SyntaxPattern
readPattern2 = do
    pat1 <- readPattern3
    mpat2 <-
        optional $ do
            readExactlyThis TokOperator "::"
            readPattern2
    case mpat2 of
        Nothing -> return pat1
        Just pat2 -> readSourcePos $ return $ consPattern pat1 pat2

readPattern3 :: Parser SyntaxPattern
readPattern3 =
    readSourcePos
        (do
             c <- readConstructor
             args <- readPatterns
             return $ ConstructorSyntaxPattern c args) <|>
    readPattern4

readPattern4 :: Parser SyntaxPattern
readPattern4 = do
    pat1 <- readPattern5
    mpat2 <-
        optional $ do
            readThis TokAt
            readPattern4
    case mpat2 of
        Nothing -> return pat1
        Just pat2 -> readSourcePos $ return $ BothSyntaxPattern pat1 pat2

listPattern :: [SyntaxPattern] -> SyntaxPattern'
listPattern [] = nilPattern
listPattern (p:pp) = consPattern p $ MkWithSourcePos (getSourcePos p) $ listPattern pp

readPattern5 :: Parser SyntaxPattern
readPattern5 =
    readSourcePos
        (do
             c <- readConstructor
             return $ ConstructorSyntaxPattern c []) <|>
    readSourcePos
        (do
             name <- readLName
             return $ VarSyntaxPattern name) <|>
    readSourcePos
        (do
             readThis TokUnderscore
             return AnySyntaxPattern) <|>
    (do
         pats <- readBracket $ readCommaList readPattern1
         readSourcePos $ return $ listPattern pats) <|>
    readParen
        (do
             spos <- getPosition
             mpat1 <- optional readPattern1
             case mpat1 of
                 Nothing -> readSourcePos $ return $ ConstructorSyntaxPattern SLUnit []
                 Just pat1 -> do
                     lpat2 <-
                         many $ do
                             readThis TokComma
                             readPattern1
                     return $
                         case lpat2 of
                             [] -> pat1
                             pat2:patr -> let
                                 appair :: SyntaxPattern -> SyntaxPattern -> SyntaxPattern
                                 appair p1 p2 = MkWithSourcePos spos $ ConstructorSyntaxPattern SLPair [p1, p2]
                                 aptuple :: SyntaxPattern -> SyntaxPattern -> [SyntaxPattern] -> SyntaxPattern
                                 aptuple p1 p2 [] = appair p1 p2
                                 aptuple p1 p2 (p3:pr) = appair p1 $ aptuple p2 p3 pr
                                 in aptuple pat1 pat2 patr) <?>
    "pattern"
