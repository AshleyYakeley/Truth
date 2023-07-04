module Pinafore.Language.Grammar.Read.Pattern
    ( readPatterns
    , readPattern
    ) where

import Pinafore.Language.Grammar.Read.Constructor
import Pinafore.Language.Grammar.Read.Parser
import Pinafore.Language.Grammar.Read.Token
import Pinafore.Language.Grammar.Read.Type
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Name
import Shapes hiding (try)

readPatterns :: Parser [SyntaxPattern]
readPatterns = do
    ns <- readAskNamespace
    nspats <- many readPattern4
    return $ fmap (\pat -> pat ns) nspats

nilPattern :: SyntaxPattern'
nilPattern =
    ConstructorSyntaxPattern RootNamespace (SLNamedConstructor (MkFullNameRef "[]" RootNamespaceRef) Nothing) []

consPattern :: SyntaxPattern -> SyntaxPattern -> SyntaxPattern'
consPattern pat1 pat2 =
    ConstructorSyntaxPattern
        RootNamespace
        (SLNamedConstructor (MkFullNameRef "::" RootNamespaceRef) Nothing)
        [pat1, pat2]

readWithSourcePos1 :: Parser (a -> t) -> Parser (a -> WithSourcePos t)
readWithSourcePos1 p = do
    MkWithSourcePos spos at <- readWithSourcePos p
    return $ \a -> MkWithSourcePos spos $ at a

modifyPattern1 :: (Namespace -> SyntaxPattern) -> Parser (Namespace -> SyntaxPattern)
modifyPattern1 patn =
    (do
         readThis TokTypeJudge
         t <- readType
         readWithSourcePos1 $ return $ \ns -> TypedSyntaxPattern (patn ns) t) <|>
    (do
         readThis TokTypeDynamic
         t <- readType
         readWithSourcePos1 $ return $ \ns -> DynamicTypedSyntaxPattern (patn ns) t) <|>
    (do
         readThis TokAs
         nn <- readNamespaceRef
         readWithSourcePos1 $ return $ \ns -> NamespaceSyntaxPattern (patn $ namespaceConcatRef ns nn) nn)

readPattern1 :: Parser (Namespace -> SyntaxPattern)
readPattern1 = do
    pat <- readPattern2
    chainModify modifyPattern1 pat

readPattern :: Parser SyntaxPattern
readPattern = do
    ns <- readAskNamespace
    nspat <- readPattern1
    return $ nspat ns

readPattern2 :: Parser (Namespace -> SyntaxPattern)
readPattern2 = do
    pat1n <- readPattern3
    mpat2n <-
        optional $ do
            readExactlyThis TokOperator "::"
            readPattern2
    case mpat2n of
        Nothing -> return pat1n
        Just pat2n -> readWithSourcePos1 $ return $ \ns -> consPattern (pat1n ns) (pat2n ns)

readPattern3 :: Parser (Namespace -> SyntaxPattern)
readPattern3 =
    readWithSourcePos1
        (try $ do
             c <- readConstructor Nothing
             args <- some readPattern4
             return $ \ns -> ConstructorSyntaxPattern ns c $ fmap (\pat -> pat ns) args) <|>
    readPattern4

readPattern4 :: Parser (Namespace -> SyntaxPattern)
readPattern4 = do
    pat1 <- readPattern5
    mpat2 <-
        optional $ do
            readThis TokAt
            readPattern4
    case mpat2 of
        Nothing -> return pat1
        Just pat2 -> readWithSourcePos1 $ return $ \ns -> BothSyntaxPattern (pat1 ns) (pat2 ns)

listPattern :: [SyntaxPattern] -> SyntaxPattern'
listPattern [] = nilPattern
listPattern (p:pp) = consPattern p $ MkWithSourcePos (getSourcePos p) $ listPattern pp

readPattern5 :: Parser (Namespace -> SyntaxPattern)
readPattern5 =
    readWithSourcePos1
        (do
             readThis TokDebug
             text <- readThis TokString
             pat <- readPattern5
             return $ \ns -> DebugSyntaxPattern text $ pat ns) <|>
    readWithSourcePos1
        (do
             c <- readConstructor Nothing
             return $ \ns -> ConstructorSyntaxPattern ns c []) <|>
    readWithSourcePos1
        (do
             name <- readLName
             return $ \ns -> VarSyntaxPattern $ MkFullName name ns) <|>
    readWithSourcePos1
        (do
             readThis TokUnderscore
             return $ \_ -> AnySyntaxPattern) <|>
    (do
         pats <- readBracket $ readCommaList readPattern1
         readWithSourcePos1 $ return $ \ns -> listPattern $ fmap (\pat -> pat ns) pats) <|>
    readParen
        (do
             spos <- getPosition
             mpat1 <- optional readPattern1
             case mpat1 of
                 Nothing -> readWithSourcePos1 $ return $ \_ -> ConstructorSyntaxPattern RootNamespace SLUnit []
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
                                 appair p1 p2 =
                                     MkWithSourcePos spos $ ConstructorSyntaxPattern RootNamespace SLPair [p1, p2]
                                 aptuple :: SyntaxPattern -> SyntaxPattern -> [SyntaxPattern] -> SyntaxPattern
                                 aptuple p1 p2 [] = appair p1 p2
                                 aptuple p1 p2 (p3:pr) = appair p1 $ aptuple p2 p3 pr
                                 in \ns -> aptuple (pat1 ns) (pat2 ns) $ fmap (\pat -> pat ns) patr) <?>
    "pattern"
