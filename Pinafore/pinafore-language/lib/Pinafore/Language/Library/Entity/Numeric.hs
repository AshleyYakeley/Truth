module Pinafore.Language.Library.Entity.Numeric
    ( plainFormattingDef
    , numericEntityLibSection
    )
where

import Shapes.Numeric

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Convert.Types
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Entity.Literal
import Pinafore.Language.Library.Entity.Showable
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Optics ()
import Pinafore.Language.Type
import Pinafore.Language.Value

arithList :: (Num a, Ord a) => a -> a -> Maybe a -> [a]
arithList step a mb = let
    cond =
        case mb of
            Nothing -> \_ -> True
            Just b ->
                case compare step 0 of
                    GT -> \x -> x <= b
                    LT -> \x -> x >= b
                    EQ -> \_ -> True
    in takeWhile cond $ iterate (+ step) a

range :: (Num a, Ord a) => a -> a -> [a]
range a b = arithList 1 a $ Just b

asTextPrism ::
    forall a.
    (Read a, ShowText a) =>
    LangPrism '(Text, Text) '(a, a)
asTextPrism = prism (readMaybe . unpack) showText

plainFormattingDef ::
    forall t.
    (HasQType QPolyShim 'Positive t, HasQType QPolyShim 'Negative t, Read t, ShowText t) =>
    Text ->
    LibraryStuff
plainFormattingDef lname = valBDS "asText" ("Represent " <> plainText lname <> " as text.") $ asTextPrism @t

numericEntityLibSection :: LibraryStuff
numericEntityLibSection =
    headingBDS
        "Numeric"
        ""
        [ headingBDS "Natural" ""
            $ [ typeBDS "Natural" "" (MkSomeGroundType naturalGroundType) []
              , hasSubtypeRelationBDS @Natural @Integer Verify "" $ functionToShim "naturalToInteger" toInteger
              , namespaceBDS "Natural"
                    $ (ordEntries @Natural)
                    <> [ headingBDS
                            "Enum"
                            ""
                            [valBDS "pred" "Previous value." $ predNat, valBDS "succ" "Next value." $ succ @Natural]
                       , headingBDS
                            "Arithmetic"
                            ""
                            [ plainFormattingDef @Natural "a non-negative integer"
                            , valBDS "+" "Add." $ (+) @Natural
                            , valBDS "-" "Subtract." subtractNat
                            , valBDS "*" "Multiply." $ (*) @Natural
                            , valBDS "mod" "Modulus, leftover from `div`" $ mod' @Natural
                            , valBDS "gcd" "Greatest common divisor." $ gcd @Natural
                            , valBDS "lcm" "Least common multiple." $ lcm @Natural
                            , valBDS "^" "Raise to power." $ (^) @Natural @Natural
                            , valBDS "sum" "Sum." $ sum @[] @Natural
                            , valBDS "product" "Product." $ product @[] @Natural
                            , valBDS
                                "range"
                                "`range a b` is an arithmetic sequence starting from `a`, with all numbers `<= b`. Step is +1."
                                $ range @Natural
                            , valBDS
                                "arithList"
                                "`arithList step a (Just b)` is an arithmetic sequence starting from `a`, with all numbers `<= b`.\n\n\
                                \`arithList step a Nothing` is an infinite arithmetic sequence starting from `a`."
                                $ arithList @Natural
                            ]
                       ]
              ]
        , headingBDS "Integer" ""
            $ [ typeBDS "Integer" "" (MkSomeGroundType integerGroundType) []
              , hasSubtypeRelationBDS @Integer @SafeRational Verify ""
                    $ functionToShim "integerSafeRational"
                    $ encode integerSafeRational
              , namespaceBDS "Integer"
                    $ pickNamesInRootBDS ["lesser", "greater"] (ordEntries @Integer)
                    <> pickNamesInRootBDS ["succ", "pred"] (enumEntries @Integer)
                    <> [ headingBDS
                            "Arithmetic"
                            ""
                            [ plainFormattingDef @Integer "an integer"
                            , addNameInRootBDS $ valBDS "+" "Add." $ (+) @Integer
                            , addNameInRootBDS $ valBDS "-" "Subtract." $ (-) @Integer
                            , addNameInRootBDS $ valBDS "*" "Multiply." $ (*) @Integer
                            , addNameInRootBDS $ valBDS "negate" "Negate." $ negate @Integer
                            , addNameInRootBDS $ valBDS "abs" "Absolute value." $ abs @Integer
                            , addNameInRootBDS $ valBDS "signum" "Sign." $ signum @Integer
                            , addNameInRootBDS $ valBDS "mod" "Modulus, leftover from `div`" $ mod' @Integer
                            , addNameInRootBDS $ valBDS "even" "Is even?" $ even @Integer
                            , addNameInRootBDS $ valBDS "odd" "Is odd?" $ odd @Integer
                            , addNameInRootBDS $ valBDS "gcd" "Greatest common divisor." $ gcd @Integer
                            , addNameInRootBDS $ valBDS "lcm" "Least common multiple." $ lcm @Integer
                            , addNameInRootBDS $ valBDS "^" "Raise to power." ((^) :: Integer -> Natural -> Integer)
                            , addNameInRootBDS $ valBDS "sum" "Sum." $ sum @[] @Integer
                            , addNameInRootBDS $ valBDS "product" "Product." $ product @[] @Integer
                            , addNameInRootBDS
                                $ valBDS
                                    "range"
                                    "`range a b` is an arithmetic sequence starting from `a`, with all numbers `<= b`. Step is +1."
                                $ range @Integer
                            , addNameInRootBDS
                                $ valBDS
                                    "arithList"
                                    "`arithList step a (Just b)` is an arithmetic sequence starting from `a`, with all numbers `<= b` (for positive step) or `>= b` (for negative step).\n\n\
                                    \`arithList step a Nothing` is an infinite arithmetic sequence starting from `a`."
                                $ arithList @Integer
                            ]
                       ]
              ]
        , headingBDS "Rational" ""
            $ [ typeBDS "Rational" "" (MkSomeGroundType rationalGroundType) []
              , hasSubtypeRelationBDS @SafeRational @Number Verify ""
                    $ functionToShim "safeRationalNumber"
                    $ encode safeRationalNumber
              , namespaceBDS
                    "Rational"
                    [ plainFormattingDef @SafeRational "a rational"
                    , valBDS "lesser" "Lesser of two Rationals" $ min @SafeRational
                    , valBDS "greater" "Greater of two Rationals" $ max @SafeRational
                    , valBDS "+" "Add." $ (+) @SafeRational
                    , valBDS "-" "Subtract." $ (-) @SafeRational
                    , valBDS "*" "Multiply." $ (*) @SafeRational
                    , addNameInRootBDS $ valBDS "/" "Divide." $ (/) @SafeRational
                    , valBDS "negate" "Negate." $ negate @SafeRational
                    , valBDS "recip" "Reciprocal." $ recip @SafeRational
                    , valBDS "abs" "Absolute value." $ abs @SafeRational
                    , valBDS "signum" "Sign." $ signum @SafeRational
                    , addNameInRootBDS
                        $ valBDS
                            "div"
                            "Divide, truncate towards negative infinity."
                            (div' :: SafeRational -> SafeRational -> Integer)
                    , valBDS "mod" "Modulus, leftover from `div`" $ mod' @SafeRational
                    , valBDS "^" "Raise to power." $ ((^^) :: SafeRational -> Integer -> SafeRational)
                    , valBDS "sum" "Sum." $ sum @[] @SafeRational
                    , valBDS "mean" "Mean." $ \(vv :: [SafeRational]) -> sum vv / toSafeRational (length vv)
                    , valBDS "product" "Product." $ product @[] @SafeRational
                    ]
              ]
        , headingBDS "Number" ""
            $ [ typeBDS "Number" "" (MkSomeGroundType numberGroundType) []
              , literalSubtypeRelationEntry @Number
              , showableSubtypeRelationEntry @Number
              , namespaceBDS "Number"
                    $ pickNamesInRootBDS ["<", "<=", ">", ">="] (ordEntries @Number)
                    <> [ plainFormattingDef @Number "a number"
                       , valBDS "+" "Add." $ (+) @Number
                       , valBDS "-" "Subtract." $ (-) @Number
                       , valBDS "*" "Multiply." $ (*) @Number
                       , valBDS "/" "Divide." $ (/) @Number
                       , valBDS "negate" "Negate." $ negate @Number
                       , valBDS "recip" "Reciprocal." $ recip @Number
                       , addNameInRootBDS $ valBDS "pi" "Half the radians in a circle." $ pi @Number
                       , addNameInRootBDS $ valBDS "exp" "Exponent" $ exp @Number
                       , addNameInRootBDS $ valBDS "log" "Natural logarithm" $ log @Number
                       , addNameInRootBDS $ valBDS "sqrt" "Square root." $ sqrt @Number
                       , valBDS "^" "Raise to power." $ (**) @Number
                       , addNameInRootBDS $ valBDS "logBase" "" $ logBase @Number
                       , addNameInRootBDS $ valBDS "sin" "Sine of an angle in radians." $ sin @Number
                       , addNameInRootBDS $ valBDS "cos" "Cosine of an angle in radians." $ cos @Number
                       , addNameInRootBDS $ valBDS "tan" "Tangent of an angle in radians." $ tan @Number
                       , addNameInRootBDS $ valBDS "asin" "Radian angle of a sine." $ asin @Number
                       , addNameInRootBDS $ valBDS "acos" "Radian angle of a cosine." $ acos @Number
                       , addNameInRootBDS $ valBDS "atan" "Radian angle of a tangent." $ atan @Number
                       , addNameInRootBDS $ valBDS "sinh" "Hyperbolic sine." $ sinh @Number
                       , addNameInRootBDS $ valBDS "cosh" "Hyperbolic cosine." $ cosh @Number
                       , addNameInRootBDS $ valBDS "tanh" "Hyperbolic tangent." $ tanh @Number
                       , addNameInRootBDS $ valBDS "asinh" "Inverse hyperbolic sine." $ asinh @Number
                       , addNameInRootBDS $ valBDS "acosh" "Inverse hyperbolic cosine." $ acosh @Number
                       , addNameInRootBDS $ valBDS "atanh" "Inverse hyperbolic tangent." $ atanh @Number
                       , valBDS "abs" "Absolute value." $ abs @Number
                       , valBDS "signum" "Sign. Note this will be the same exact or inexact as the number." $ signum @Number
                       , addNameInRootBDS $ valBDS "floor" "Integer towards negative infinity." (floor :: Number -> Integer)
                       , addNameInRootBDS $ valBDS "ceiling" "Integer towards positive infinity." (ceiling :: Number -> Integer)
                       , addNameInRootBDS $ valBDS "round" "Closest Integer." (round :: Number -> Integer)
                       , addNameInRootBDS $ valBDS "inexact" "Convert a number to inexact." numberToDouble
                       , addNameInRootBDS
                            $ valBDS
                                "approximate"
                                "`approximate d x` gives the exact number that's a multiple of `d` that's closest to `x`."
                                approximate
                       , addNameInRootBDS
                            $ valBDS "div" "Division to Integer, towards negative infinity." (div' :: Number -> Number -> Integer)
                       , addNameInRootBDS $ valBDS "mod" "Modulus, leftover from `div`" $ mod' @Number
                       , addNameInRootBDS $ valBDS "isNaN" "Is not a number?" numberIsNaN
                       , addNameInRootBDS $ valBDS "isInfinite" "Is infinite?" numberIsInfinite
                       , addNameInRootBDS $ valBDS "isNegativeZero" "Is negative zero?" numberIsNegativeZero
                       , addNameInRootBDS $ valBDS "isExact" "Is exact?" numberIsExact
                       , valBDS "sum" "Sum." $ sum @[] @Number
                       , valBDS "mean" "Mean." $ \(vv :: [Number]) -> sum vv / (ExactNumber $ toRational $ length vv)
                       , valBDS "product" "Product." $ product @[] @Number
                       , addNameInRootBDS
                            $ valBDS "checkSafeRational" "Get the exact value of a Number, if it is one."
                            $ decode safeRationalNumber
                       , addNameInRootBDS
                            $ valBDS
                                "checkExactInteger"
                                "Get the exact Integer value of a Number, if it is one. Works as expected on Rationals."
                            $ decode
                            $ integerSafeRational
                            . safeRationalNumber
                       ]
              ]
        ]
