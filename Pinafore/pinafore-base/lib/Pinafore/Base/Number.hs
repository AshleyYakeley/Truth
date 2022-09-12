module Pinafore.Base.Number
    ( Number(..)
    , showDecimalRational
    , numberToDouble
    , safeRationalNumber
    , approximate
    , numberIsNaN
    , numberIsInfinite
    , numberIsNegativeZero
    , numberIsExact
    , readNumberLiteral
    ) where

import Data.List (head)
import Pinafore.Base.SafeRational
import Pinafore.Base.Showable
import Shapes hiding ((+++))
import Shapes.Numeric

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

safeRationalNumber :: Codec Number SafeRational
safeRationalNumber = MkCodec numberCheckSafeRational safeRationalToNumber

safeRationalToNumber :: SafeRational -> Number
safeRationalToNumber (SRNumber n) = ExactNumber n
safeRationalToNumber SRNaN = InexactNumber $ 0 / 0

numberCheckSafeRational :: Number -> Maybe SafeRational
numberCheckSafeRational (ExactNumber n) = Just $ SRNumber n
numberCheckSafeRational (InexactNumber n)
    | isNaN n = Just $ SRNaN
numberCheckSafeRational _ = Nothing

approximate :: Rational -> Number -> Rational
approximate res n = res * toRational (round (n / fromRational res) :: Integer)

liftOp1R ::
       forall c. (c Rational, c Double)
    => (forall t. c t => t -> t)
    -> Number
    -> Number
liftOp1R f (ExactNumber n) = ExactNumber $ f n
liftOp1R f (InexactNumber n) = InexactNumber $ f n

liftOp2 ::
       forall c r. (c Rational, c Double)
    => (forall t. c t => t -> t -> r)
    -> Number
    -> Number
    -> r
liftOp2 f (ExactNumber a) (ExactNumber b) = f a b
liftOp2 f a b = f (numberToDouble a) (numberToDouble b)

liftOp2R ::
       forall c. (c Rational, c Double)
    => (forall t. c t => t -> t -> t)
    -> Number
    -> Number
    -> Number
liftOp2R f (ExactNumber a) (ExactNumber b) = ExactNumber $ f a b
liftOp2R f a b = InexactNumber $ f (numberToDouble a) (numberToDouble b)

instance Eq Number where
    (ExactNumber a) == (ExactNumber b) = a == b
    a == b = (numberToDouble a) == (numberToDouble b)

instance Ord Number where
    (>) = liftOp2 @Ord (>)
    (<) = liftOp2 @Ord (<)
    (>=) = liftOp2 @Ord (>=)
    (<=) = liftOp2 @Ord (<=)
    compare = liftOp2 @Ord compare

instance Num Number where
    (+) = liftOp2R @Num (+)
    (-) = liftOp2R @Num (-)
    (*) = liftOp2R @Num (*)
    abs = liftOp1R @Num abs
    signum = liftOp1R @Num signum
    negate = liftOp1R @Num negate
    fromInteger = ExactNumber . fromInteger

instance Fractional Number where
    (/) (ExactNumber n) (ExactNumber 0) =
        InexactNumber $
        case compare n 0 of
            GT -> 1 / 0
            EQ -> 0 / 0
            LT -> -1 / 0
    (/) p q = liftOp2R @Fractional (/) p q
    recip = liftOp1R @Fractional recip
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

liftDoubleOp1R :: (Double -> Double) -> Number -> Number
liftDoubleOp1R f x = InexactNumber $ f $ numberToDouble x

liftDoubleOp2R :: (Double -> Double -> Double) -> Number -> Number -> Number
liftDoubleOp2R f a b = InexactNumber $ f (numberToDouble a) (numberToDouble b)

instance Floating Number where
    pi = InexactNumber pi
    exp = liftDoubleOp1R exp
    log = liftDoubleOp1R log
    sqrt = liftDoubleOp1R sqrt
    (**) = liftDoubleOp2R (**)
    logBase = liftDoubleOp2R logBase
    sin = liftDoubleOp1R sin
    cos = liftDoubleOp1R cos
    tan = liftDoubleOp1R tan
    asin = liftDoubleOp1R asin
    acos = liftDoubleOp1R acos
    atan = liftDoubleOp1R atan
    sinh = liftDoubleOp1R sinh
    cosh = liftDoubleOp1R cosh
    tanh = liftDoubleOp1R tanh
    asinh = liftDoubleOp1R asinh
    acosh = liftDoubleOp1R acosh
    atanh = liftDoubleOp1R atanh

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

instance TextShow Number where
    textShow (ExactNumber r) = textShow (SRNumber r)
    textShow (InexactNumber d)
        | isNaN d = textShow SRNaN
    textShow (InexactNumber d) = "~" <> textShow d

instance Show Number where
    show v = unpack $ textShow v

readNumberLiteral :: String -> Maybe Number
readNumberLiteral = runReadPrec readPrec

instance Read Number where
    readPrec = let
        readInexact :: ReadPrec Double
        readInexact = do
            rLiteral '~'
            readPrec
        in fmap InexactNumber readInexact <++ do
               sr <- readPrec
               return $
                   case sr of
                       SRNumber n -> ExactNumber n
                       SRNaN -> InexactNumber $ 0 / 0
