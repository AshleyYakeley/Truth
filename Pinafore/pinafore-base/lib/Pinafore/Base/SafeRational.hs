module Pinafore.Base.SafeRational
    ( SafeRational(..)
    , toSafeRational
    , integerSafeRational
    ) where

import Pinafore.Base.Showable
import Shapes
import Shapes.Numeric

-- | A Rational plus a NaN value for x/0
data SafeRational
    = SRNumber Rational
    | SRNaN

instance Eq SafeRational where
    SRNumber a == SRNumber b = a == b
    _ == _ = False

instance TextShow SafeRational where
    textShow SRNaN = "NaN"
    textShow (SRNumber r) = let
        n = numerator r
        d = denominator r
        in if d == 1
               then textShow n
               else textShow n <> "/" <> textShow d

instance Show SafeRational where
    show v = unpack $ textShow v

instance Read SafeRational where
    readPrec = let
        option' :: a -> ReadPrec a -> ReadPrec a
        option' x p = p <++ return x
        many' :: ReadPrec a -> ReadPrec [a]
        many' p = many1' p <++ return []
        many1' :: ReadPrec a -> ReadPrec [a]
        many1' p = liftA2 (:) p $ many' p
        assembleDigits :: (Integer, Integer) -> String -> (Integer, Integer)
        assembleDigits it [] = it
        assembleDigits (i, t) (c:cc) = assembleDigits (i * 10 + toInteger (digitToInt c), t * 10) cc
        readDigits :: ReadPrec (Integer, Integer)
        readDigits = do
            s <- many' $ rSatisfy isDigit
            return $ assembleDigits (0, 1) s
        readDigits1 :: ReadPrec (Integer, Integer)
        readDigits1 = do
            s <- many1' $ rSatisfy isDigit
            return $ assembleDigits (0, 1) s
        readDecimalPart :: ReadPrec Rational
        readDecimalPart = do
            rLiteral '.'
            (fixN, fixD) <- readDigits
            repR <-
                option' 0 $ do
                    rLiteral '_'
                    (repN, repD) <- readDigits
                    return $
                        if repD == 1
                            then 0
                            else repN % (fixD * (pred repD))
            return $ (fixN % fixD) + repR
        readFractionPart :: ReadPrec Integer
        readFractionPart = do
            rLiteral '/'
            (d, _) <- readDigits1
            return d
        readRational :: ReadPrec Rational
        readRational = do
            sign <- (rLiteral '-' >> return negate) <++ return id
            (intPart, _) <- readDigits1
            remaining <- option' (Left 0) $ fmap Left readDecimalPart <++ fmap Right readFractionPart
            return $
                sign $
                case remaining of
                    Left decPart -> toRational intPart + decPart
                    Right d -> intPart % d
        readNaN :: ReadPrec SafeRational
        readNaN = do
            rLiterals "NaN"
            return SRNaN
        in fmap SRNumber readRational <++ readNaN

liftOp1R :: (Rational -> Rational) -> SafeRational -> SafeRational
liftOp1R f (SRNumber n) = SRNumber $ f n
liftOp1R _ _ = SRNaN

liftOp2 :: forall r. r -> (Rational -> Rational -> r) -> SafeRational -> SafeRational -> r
liftOp2 _ f (SRNumber a) (SRNumber b) = f a b
liftOp2 def _ _ _ = def

liftOp2R :: (Rational -> Rational -> Rational) -> SafeRational -> SafeRational -> SafeRational
liftOp2R f (SRNumber a) (SRNumber b) = SRNumber $ f a b
liftOp2R _ _ _ = SRNaN

instance Ord SafeRational where
    (>) = liftOp2 False (>)
    (<) = liftOp2 False (<)
    (>=) = liftOp2 False (>=)
    (<=) = liftOp2 False (<=)
    compare = liftOp2 GT compare

instance Num SafeRational where
    (+) = liftOp2R (+)
    (-) = liftOp2R (-)
    (*) = liftOp2R (*)
    abs = liftOp1R abs
    signum = liftOp1R signum
    negate = liftOp1R negate
    fromInteger = SRNumber . fromInteger

instance Fractional SafeRational where
    _ / (SRNumber 0) = SRNaN
    (SRNumber p) / (SRNumber q) = SRNumber $ p / q
    _ / _ = SRNaN
    recip (SRNumber 0) = SRNaN
    recip (SRNumber n) = SRNumber $ recip n
    recip SRNaN = SRNaN
    fromRational = SRNumber

instance Real SafeRational where
    toRational (SRNumber n) = n
    toRational SRNaN = 0 / 0 -- recover bottom value

instance RealFrac SafeRational where
    properFraction (SRNumber x) = let
        (n, f) = properFraction x
        in (n, SRNumber f)
    properFraction SRNaN = (0, SRNaN)

toSafeRational :: Real a => a -> SafeRational
toSafeRational = SRNumber . toRational

safeRationalCheckInteger :: SafeRational -> Maybe Integer
safeRationalCheckInteger (SRNumber r)
    | denominator r == 1 = Just $ numerator r
safeRationalCheckInteger _ = Nothing

integerSafeRational :: Codec SafeRational Integer
integerSafeRational = MkCodec safeRationalCheckInteger toSafeRational
