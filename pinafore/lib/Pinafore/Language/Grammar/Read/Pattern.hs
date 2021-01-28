module Pinafore.Language.Grammar.Read.Pattern
    ( readPatterns
    , readPattern2
    ) where

import Pinafore.Language.Grammar.Read.Constructor
import Pinafore.Language.Grammar.Read.Parser
import Pinafore.Language.Grammar.Read.Token
import Pinafore.Language.Grammar.Read.Type
import Pinafore.Language.Grammar.Syntax
import Shapes hiding (try)

readSourcePosPattern :: Parser SyntaxPattern' -> Parser SyntaxPattern
readSourcePosPattern p = do
    spos <- getPosition
    expr' <- p
    return $ MkWithSourcePos spos expr'

readPatterns :: Parser [SyntaxPattern]
readPatterns = many readPattern4

nilPattern :: SourcePos -> SyntaxPattern
nilPattern spos = MkWithSourcePos spos $ ConstructorSyntaxPattern (SLNamedConstructor "[]") []

consPattern :: SourcePos -> SyntaxPattern -> SyntaxPattern -> SyntaxPattern
consPattern spos pat1 pat2 = MkWithSourcePos spos $ ConstructorSyntaxPattern (SLNamedConstructor "::") [pat1, pat2]

readPattern1 :: Parser SyntaxPattern
readPattern1 = do
    spos <- getPosition
    pat <- readPattern2
    mt <-
        optional $ do
            readThis TokTypeJudge
            readType
    return $
        case mt of
            Just t -> MkWithSourcePos spos $ TypedSyntaxPattern pat t
            Nothing -> pat

readPattern2 :: Parser SyntaxPattern
readPattern2 = do
    spos <- getPosition
    pat1 <- readPattern3
    mpat2 <-
        optional $ do
            readExactlyThis TokOperator "::"
            readPattern2
    case mpat2 of
        Nothing -> return pat1
        Just pat2 -> return $ consPattern spos pat1 pat2

readPattern3 :: Parser SyntaxPattern
readPattern3 =
    readSourcePosPattern
        (do
             c <- readConstructor
             args <- readPatterns
             return $ ConstructorSyntaxPattern c args) <|>
    readPattern4

readPattern4 :: Parser SyntaxPattern
readPattern4 = do
    spos <- getPosition
    pat1 <- readPattern5
    mpat2 <-
        optional $ do
            readThis TokAt
            readPattern4
    case mpat2 of
        Nothing -> return pat1
        Just pat2 -> return $ MkWithSourcePos spos $ BothSyntaxPattern pat1 pat2

listPattern :: SourcePos -> [SyntaxPattern] -> SyntaxPattern
listPattern spos [] = nilPattern spos
listPattern spos (p:pp) = consPattern spos p $ listPattern spos pp

readPattern5 :: Parser SyntaxPattern
readPattern5 =
    readSourcePosPattern
        (do
             c <- readConstructor
             return $ ConstructorSyntaxPattern c []) <|>
    readSourcePosPattern
        (do
             name <- readThis TokLName
             return $ VarSyntaxPattern name) <|>
    readSourcePosPattern
        (do
             readThis TokUnderscore
             return AnySyntaxPattern) <|>
    (do
         spos <- getPosition
         pats <- readBracket $ readCommaList $ fmap pure readPattern1
         return $ listPattern spos pats) <|>
    readParen
        (do
             spos <- getPosition
             mpat1 <- optional readPattern1
             case mpat1 of
                 Nothing -> return $ MkWithSourcePos spos $ ConstructorSyntaxPattern SLUnit []
                 Just pat1 -> do
                     mpat2 <-
                         optional $ do
                             readThis TokComma
                             readPattern1
                     case mpat2 of
                         Nothing -> return pat1
                         Just pat2 -> return $ MkWithSourcePos spos $ ConstructorSyntaxPattern SLPair [pat1, pat2]) <?>
    "pattern"
