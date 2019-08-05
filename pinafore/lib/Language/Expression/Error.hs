module Language.Expression.Error where

import Shapes

data ExpressionError
    = UndefinedBindingsError [String]
    | PatternTooManyConsArgsError
    | PatternTooFewConsArgsError

instance Show ExpressionError where
    show (UndefinedBindingsError tt) = "undefined: " <> intercalate ", " tt
    show PatternTooManyConsArgsError = "too many arguments to constructor in pattern"
    show PatternTooFewConsArgsError = "not enough arguments to constructor in pattern"
