module Language.Expression.Common.Open.Error where

import Shapes

data ExpressionError (w :: Type -> Type)
    = UndefinedBindingsError (NonEmpty (Some w))

instance AllConstraint Show w => Show (ExpressionError w) where
    show (UndefinedBindingsError tt) = "undefined: " <> intercalate ", " (fmap show $ toList tt)

data PatternError
    = PatternTooManyConsArgsError
    | PatternTooFewConsArgsError

instance Show PatternError where
    show PatternTooManyConsArgsError = "too many arguments to constructor in pattern"
    show PatternTooFewConsArgsError = "not enough arguments to constructor in pattern"
