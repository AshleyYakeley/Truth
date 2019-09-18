module Pinafore.Base.SafeRational
    ( SafeRational(..)
    ) where

import Shapes
import Shapes.Numeric

-- | A Rational plus a NaN value for x/0
data SafeRational
    = SRNumber Rational
    | SRNaN

instance Eq SafeRational where
    SRNumber a == SRNumber b = a == b
    _ == _ = False

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
