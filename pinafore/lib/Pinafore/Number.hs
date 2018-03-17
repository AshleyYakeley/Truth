module Pinafore.Number
    ( Number(..)
    , numberToDouble
    ) where

import Data.List (head, iterate)
import Data.Ratio
import Prelude (Fractional(..), isNaN)
import Shapes hiding ((+++), option)
import Text.ParserCombinators.ReadP hiding (many)

data Number
    = ExactNumber Rational
    | InexactNumber Double

numberToDouble :: Number -> Double
numberToDouble (ExactNumber a) = fromRational a
numberToDouble (InexactNumber a) = a

arity1op ::
       forall c. (c Rational, c Double)
    => (forall t. c t =>
                      t -> t)
    -> Number
    -> Number
arity1op f (ExactNumber n) = ExactNumber $ f n
arity1op f (InexactNumber n) = InexactNumber $ f n

arity2op ::
       forall c. (c Rational, c Double)
    => (forall t. c t =>
                      t -> t -> t)
    -> Number
    -> Number
    -> Number
arity2op f (ExactNumber a) (ExactNumber b) = ExactNumber $ f a b
arity2op f a b = InexactNumber $ f (numberToDouble a) (numberToDouble b)

instance Eq Number where
    (ExactNumber a) == (ExactNumber b) = a == b
    a == b = (numberToDouble a) == (numberToDouble b)

instance Ord Number where
    compare (ExactNumber a) (ExactNumber b) = compare a b
    compare a b = compare (numberToDouble a) (numberToDouble b)

instance Num Number where
    (+) = arity2op @Num (+)
    (-) = arity2op @Num (-)
    (*) = arity2op @Num (*)
    abs = arity1op @Num abs
    signum = arity1op @Num signum
    negate = arity1op @Num negate
    fromInteger = ExactNumber . fromInteger

instance Fractional Number where
    (/) = arity2op @Fractional (+)
    recip = arity1op @Fractional recip
    fromRational = ExactNumber

instance Show Number where
    show (ExactNumber r) = let
        sn = numerator r
        d = denominator r
        sign =
            if sn < 0
                then "-"
                else ""
        n = abs sn
        (i', decimal') =
            if d == 1
                then (n, "")
                else let
                         factorCount f x
                             | mod x f > 0 = 0
                         factorCount f x = 1 + factorCount f (div x f)
                         fixedCount = max (factorCount 2 d) (factorCount 5 d)
                         i = div n d
                         fn = n - i * d
                         numbers = iterate (\fn10 -> (mod fn10 d) * 10) $ fn * 10
                         toDigit fn10 = head $ show $ div fn10 d
                         (preNumbers, repNumbers) = splitAt fixedCount numbers
                         repeating =
                             case repNumbers of
                                 0:_ -> ""
                                 c:rest -> '_' : (toDigit c) : (fmap toDigit $ takeWhile (/= c) rest)
                                 [] -> error "impossible"
                         predigits = fmap toDigit preNumbers
                         in (i, '.' : (predigits ++ repeating))
        in sign ++ show i' ++ decimal'
    show (InexactNumber d)
        | isNaN d = show d
    show (InexactNumber d) = '~' : (show d)

instance Read Number where
    readsPrec prec = let
        option' :: a -> ReadP a -> ReadP a
        option' x p = p <++ return x
        many' :: ReadP a -> ReadP [a]
        many' p = many1' p <++ return []
        many1' :: ReadP a -> ReadP [a]
        many1' p = liftA2 (:) p $ many' p
        readP ::
               forall a. Read a
            => ReadP a
        readP = readS_to_P $ readsPrec prec
        readInexact :: ReadP Double
        readInexact = do
            _ <- char '~'
            readP
        assembleDigits :: (Integer, Integer) -> String -> (Integer, Integer)
        assembleDigits it [] = it
        assembleDigits (i, t) (c:cc) = assembleDigits (i * 10 + toInteger (digitToInt c), t * 10) cc
        readDigits :: ReadP (Integer, Integer)
        readDigits = do
            s <- many' $ satisfy isDigit
            return $ assembleDigits (0, 1) s
        readDigits1 :: ReadP (Integer, Integer)
        readDigits1 = do
            s <- many1' $ satisfy isDigit
            return $ assembleDigits (0, 1) s
        readExact :: ReadP Rational
        readExact = do
            sign <- (char '-' >> return negate) <++ return id
            (intPart, _) <- readDigits1
            decPart <-
                option' 0 $ do
                    _ <- char '.'
                    (fixN, fixD) <- readDigits
                    repR <-
                        option' 0 $ do
                            _ <- char '_'
                            (repN, repD) <- readDigits
                            return $
                                if repD == 1
                                    then 0
                                    else repN % (fixD * (repD - 1))
                    return $ (fixN % fixD) + repR
            return $ sign $ toRational intPart + decPart
        readNumber :: ReadP Number
        readNumber = do
            skipSpaces
            n <- fmap InexactNumber readInexact <++ fmap ExactNumber readExact
            skipSpaces
            return n
        in \s ->
               case readP_to_S readNumber s of
                   ps@[(_, "")] -> ps
                   _ -> [(InexactNumber $ 0 / 0, "")]
