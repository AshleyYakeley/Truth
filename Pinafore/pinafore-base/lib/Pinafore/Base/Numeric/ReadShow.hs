{-# OPTIONS -fno-warn-orphans #-}
module Pinafore.Base.Numeric.ReadShow where

import Shapes hiding ((+++))
import Shapes.Numeric

import Pinafore.Base.Numeric.Number
import Pinafore.Base.Numeric.SafeRational

showNatural :: Natural -> Text
showNatural = pack . show

showInteger :: ListProduct '[Bool] -> Integer -> Text
showInteger (True, ()) n | n > 0 = pack $ '+' : show n
showInteger _ n = pack $ show n

showImproperSafeRational :: SafeRational -> Text
showImproperSafeRational = \case
    SRNaN -> "NaN"
    SRNumber r -> let
        n = numerator r
        d = denominator r
        in if d == 1
            then showT n
            else showT n <> "/" <> showT d

showMixedRational :: Rational -> Text
showMixedRational r =
    if r < 0
        then "-" <> showMixedRational (negate r)
        else let
            n = numerator r
            d = denominator r
            in if d == 1
                then showT n
                else
                    if n > d
                        then showT (div n d) <> " " <> showT (mod n d) <> "/" <> showT d
                        else showT n <> "/" <> showT d

showMixedSafeRational :: SafeRational -> Text
showMixedSafeRational = \case
    SRNaN -> "NaN"
    SRNumber r -> showMixedRational r

instance Show SafeRational where
    show v = unpack $ showImproperSafeRational v

showSign :: Bool -> Ordering -> String
showSign _ LT = "-"
showSign True GT = "+"
showSign _ _ = ""

data DecomposedRational = DecomposedRational
    { drSign :: Ordering
    , drInteger :: Integer
    , drFixedDigits :: String
    , drRecurringDigits :: String
    }

decomposeRational :: Rational -> DecomposedRational
decomposeRational r = let
    sn = numerator r
    d = denominator r
    drSign = compare sn 0
    n = abs sn
    in if d == 1
        then DecomposedRational drSign n "" ""
        else let
            factorCount f x
                | mod x f > 0 = 0
            factorCount f x = 1 + factorCount f (div x f)
            fixedCount = max (factorCount 2 d) (factorCount 5 d)
            drInteger = div n d
            fn = n - drInteger * d
            numbers = iterate (\fn10 -> (mod fn10 d) * 10) $ fn * 10
            toDigit fn10 = fromMaybe (error "impossible") $ listToMaybe $ show $ div fn10 d
            (preNumbers, repNumbers) = splitAt fixedCount numbers
            drRecurringDigits =
                case repNumbers of
                    0 : _ -> ""
                    c : rest -> (toDigit c) : (fmap toDigit $ takeWhile (/= c) rest)
                    [] -> error "impossible"
            drFixedDigits = fmap toDigit preNumbers
            in DecomposedRational{..}

type DecimalOptions = ListProduct '[Bool, Natural, Bool, Bool, Bool]

showDecimalRational :: DecimalOptions -> Rational -> Text
showDecimalRational (plusSign, (maxDigits, (alwaysPoint, (separateRecurring, (impreciseEllipsis, ()))))) r = let
    DecomposedRational{..} = decomposeRational r
    repeating =
        case drRecurringDigits of
            "" -> ""
            _ ->
                if separateRecurring
                    then '_' : drRecurringDigits
                    else cycle drRecurringDigits
    (digits, rest) = splitAt (fromIntegral maxDigits) $ drFixedDigits <> repeating
    ee = case (rest, impreciseEllipsis) of
        (_ : _, True) -> "…"
        _ -> ""
    postPoint = digits <> ee
    dp = case (postPoint, alwaysPoint) of
        ("", False) -> ""
        _ -> "."
    in pack $ showSign plusSign drSign <> show drInteger <> dp <> postPoint

showDecimalSafeRational :: DecimalOptions -> SafeRational -> Text
showDecimalSafeRational opts = \case
    SRNumber r -> showDecimalRational opts r
    SRNaN -> "NaN"

showDecimalDouble :: DecimalOptions -> Double -> Text
showDecimalDouble _ d = pack $ show d -- NYI

showDecimalNumber :: DecimalOptions -> Number -> Text
showDecimalNumber opts = \case
    ExactNumber r -> showDecimalRational opts r
    InexactNumber d -> showDecimalDouble opts d
