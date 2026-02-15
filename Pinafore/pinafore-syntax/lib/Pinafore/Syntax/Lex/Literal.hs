{-# OPTIONS -fno-warn-orphans #-}
module Pinafore.Syntax.Lex.Literal (LexLiteral (..), readLiteralMaybe) where

import Pinafore.Base
import Shapes hiding (try)
import Shapes.Numeric
import Text.Parsec hiding (many, optional, (<|>))

import Pinafore.Syntax.Lex.Lexer

class LexLiteral a where
    lexLiteral :: Lexer a

readLiteralMaybe :: forall a. LexLiteral a => String -> Maybe a
readLiteralMaybe = runLexerWhole lexLiteral

lexEscapedChar :: Lexer Char
lexEscapedChar = do
    lexChar '\\'
    c <- anyToken
    case c of
        'n' -> return '\n'
        't' -> return '\t'
        'r' -> return '\r'
        'f' -> return '\f'
        _ -> return c

lexQuotedChar :: Lexer Char
lexQuotedChar = lexEscapedChar <|> (satisfy ('"' /=))

instance LexLiteral String where
    lexLiteral = do
        lexChar '"'
        s <- many lexQuotedChar
        lexChar '"'
        return s

instance LexLiteral Text where
    lexLiteral = fmap pack lexLiteral

instance LexLiteral Rational where
    lexLiteral = let
        assembleDigits :: (Integer, Integer) -> String -> (Integer, Integer)
        assembleDigits it [] = it
        assembleDigits (i, t) (c : cc) = assembleDigits (i * 10 + toInteger (digitToInt c), t * 10) cc
        readDigits :: Lexer (Integer, Integer)
        readDigits = do
            s <- rMany $ rSatisfy isDigit
            return $ assembleDigits (0, 1) s
        readDigits1 :: Lexer (Integer, Integer)
        readDigits1 = do
            s <- rSome $ rSatisfy isDigit
            return $ assembleDigits (0, 1) s
        readDecimalPart :: Lexer Rational
        readDecimalPart = do
            rLiteral '.'
            (fixN, fixD) <- readDigits
            repR <-
                rOption 0 $ do
                    rLiteral '_'
                    (repN, repD) <- readDigits
                    return
                        $ if repD == 1
                            then 0
                            else repN % (fixD * (pred repD))
            return $ (fixN % fixD) + repR
        readFractionPart :: Lexer Integer
        readFractionPart = do
            rLiteral '/'
            (d, _) <- readDigits1
            return d
        in do
            sign <- (rLiteral '-' >> return negate) <++ return id
            (intPart, _) <- readDigits1
            remaining <- rOption (Left 0) $ fmap Left readDecimalPart <++ fmap Right readFractionPart
            return
                $ sign
                $ case remaining of
                    Left decPart -> toRational intPart + decPart
                    Right d -> intPart % d

instance LexLiteral SafeRational where
    lexLiteral = fmap SRNumber lexLiteral <++ (rLiterals "NaN" $> SRNaN)

instance Read SafeRational where
    readsPrec _ s = case readLiteralMaybe s of
        Just a -> [(a, "")]
        Nothing -> []

instance LexLiteral Double where
    lexLiteral =
        ( try $ do
            lexString "NaN"
            return $ 0 / 0
        )
            <|> ( try $ do
                    lexString "~Infinity"
                    return $ 1 / 0
                )
            <|> ( try $ do
                    lexString "~-Infinity"
                    return $ -1 / 0
                )
            <|> ( try $ do
                    lexChar '~'
                    text <- many1 $ satisfy $ \c -> elem c ("0123456789-.e_" :: String)
                    mpure $ runReadPrec readPrec text
                )

instance LexLiteral Number where
    lexLiteral =
        (try $ fmap InexactNumber lexLiteral)
            <|> ( try $ do
                    sr <- lexLiteral
                    return $ case sr of
                        SRNumber n -> ExactNumber n
                        SRNaN -> InexactNumber $ 0 / 0
                )

instance Read Number where
    readsPrec _ s = case readLiteralMaybe s of
        Just a -> [(a, "")]
        Nothing -> []
