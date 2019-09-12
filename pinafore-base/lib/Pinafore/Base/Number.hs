module Pinafore.Base.Number
    ( Number(..)
    , showDecimalRational
    , numberToDouble
    , checkExactRational
    , rationalInteger
    , approximate
    , numberIsNaN
    , numberIsInfinite
    , numberIsNegativeZero
    , numberIsExact
    , readNumberLiteral
    ) where

import Data.List (head, iterate)
import Shapes hiding ((+++), option)
import Shapes.Numeric
import Text.ParserCombinators.ReadP hiding (many)

data Number
    = ExactNumber Rational
    | InexactNumber Double

numberToDouble :: Number -> Double
numberToDouble (ExactNumber a) = fromRational a
numberToDouble (InexactNumber a) = a

numberIsNaN :: Number -> Bool
numberIsNaN (InexactNumber n) = isNaN n
numberIsNaN _ = False

numberIsInfinite :: Number -> Bool
numberIsInfinite (InexactNumber n) = isInfinite n
numberIsInfinite _ = False

numberIsNegativeZero :: Number -> Bool
numberIsNegativeZero (InexactNumber n) = isNegativeZero n
numberIsNegativeZero _ = False

numberIsExact :: Number -> Bool
numberIsExact (ExactNumber _) = True
numberIsExact _ = False

checkExactRational :: Number -> Maybe Rational
checkExactRational (ExactNumber n) = Just n
checkExactRational _ = Nothing

rationalInteger :: Rational -> Maybe Integer
rationalInteger r
    | denominator r == 1 = Just $ numerator r
rationalInteger _ = Nothing

approximate :: Rational -> Number -> Rational
approximate res n = res * toRational (round (n / fromRational res) :: Integer)

arity1op ::
       forall c. (c Rational, c Double)
    => (forall t. c t => t -> t)
    -> Number
    -> Number
arity1op f (ExactNumber n) = ExactNumber $ f n
arity1op f (InexactNumber n) = InexactNumber $ f n

arity2op ::
       forall c. (c Rational, c Double)
    => (forall t. c t => t -> t -> t)
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
    (/) (ExactNumber n) (ExactNumber 0) =
        InexactNumber $
        case compare n 0 of
            GT -> 1 / 0
            EQ -> 0 / 0
            LT -> -1 / 0
    (/) p q = arity2op @Fractional (/) p q
    recip = arity1op @Fractional recip
    fromRational = ExactNumber

instance Real Number where
    toRational (ExactNumber n) = n
    toRational (InexactNumber n) = toRational n

instance RealFrac Number where
    properFraction (ExactNumber x) = let
        (n, f) = properFraction x
        in (n, ExactNumber f)
    properFraction (InexactNumber x) = let
        (n, f) = properFraction x
        in (n, InexactNumber f)

arity1DoubleOp :: (Double -> Double) -> Number -> Number
arity1DoubleOp f x = InexactNumber $ f $ numberToDouble x

arity2DoubleOp :: (Double -> Double -> Double) -> Number -> Number -> Number
arity2DoubleOp f a b = InexactNumber $ f (numberToDouble a) (numberToDouble b)

instance Floating Number where
    pi = InexactNumber pi
    exp = arity1DoubleOp exp
    log = arity1DoubleOp log
    sqrt = arity1DoubleOp sqrt
    (**) = arity2DoubleOp (**)
    logBase = arity2DoubleOp logBase
    sin = arity1DoubleOp sin
    cos = arity1DoubleOp cos
    tan = arity1DoubleOp tan
    asin = arity1DoubleOp asin
    acos = arity1DoubleOp acos
    atan = arity1DoubleOp atan
    sinh = arity1DoubleOp sinh
    cosh = arity1DoubleOp cosh
    tanh = arity1DoubleOp tanh
    asinh = arity1DoubleOp asinh
    acosh = arity1DoubleOp acosh
    atanh = arity1DoubleOp atanh

showDecimalRational :: Int -> Rational -> Text
showDecimalRational maxDigits r = let
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
                     in (i, '.' : (take maxDigits $ predigits ++ repeating))
    in pack $ sign ++ show i' ++ decimal'

instance Show Number where
    show (ExactNumber r) = let
        sn = numerator r
        d = denominator r
        sign =
            if sn < 0
                then "-"
                else ""
        n = abs sn
        in sign <>
           (if d == 1
                then show n
                else show n <> "/" <> show d)
    show (InexactNumber d)
        | isNaN d = show d
    show (InexactNumber d) = '~' : (show d)

runReadP :: ReadP a -> String -> Maybe a
runReadP r s = let
    pickdone (a, "") = Just a
    pickdone _ = Nothing
    in case mapMaybe pickdone $ readP_to_S r s of
           [a] -> Just a
           _ -> Nothing

readNumberLiteral :: String -> Maybe Number
readNumberLiteral = let
    option' :: a -> ReadP a -> ReadP a
    option' x p = p <++ return x
    many' :: ReadP a -> ReadP [a]
    many' p = many1' p <++ return []
    many1' :: ReadP a -> ReadP [a]
    many1' p = liftA2 (:) p $ many' p
    readP ::
           forall a. Read a
        => ReadP a
    readP = readS_to_P $ readsPrec 0
    readInexact :: ReadP Double
    readInexact = do
        void $ char '~'
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
                void $ char '.'
                (fixN, fixD) <- readDigits
                repR <-
                    option' 0 $ do
                        void $ char '_'
                        (repN, repD) <- readDigits
                        return $
                            if repD == 1
                                then 0
                                else repN % (fixD * (repD - 1))
                return $ (fixN % fixD) + repR
        return $ sign $ toRational intPart + decPart
    readNaN :: ReadP Number
    readNaN = do
        void $ string "NaN"
        return $ InexactNumber $ 0 / 0
    readNumber :: ReadP Number
    readNumber = fmap InexactNumber readInexact <++ fmap ExactNumber readExact <++ readNaN
    in runReadP readNumber

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
            void $ char '~'
            readP
        assembleDigits :: Integer -> String -> Integer
        assembleDigits i [] = i
        assembleDigits i (c:cc) = assembleDigits (i * 10 + toInteger (digitToInt c)) cc
        readDigits1 :: ReadP Integer
        readDigits1 = do
            s <- many1' $ satisfy isDigit
            return $ assembleDigits 0 s
        readExact :: ReadP Rational
        readExact = do
            sign <- (char '-' >> return negate) <++ return id
            n <- readDigits1
            d <-
                option' 1 $ do
                    void $ char '/'
                    readDigits1
            return $ sign $ n % d
        readNaN :: ReadP Number
        readNaN = do
            void $ string "NaN"
            return $ InexactNumber $ 0 / 0
        readNumber :: ReadP Number
        readNumber = do
            skipSpaces
            n <- fmap InexactNumber readInexact <++ fmap ExactNumber readExact <++ readNaN
            skipSpaces
            return n
        in readP_to_S readNumber
