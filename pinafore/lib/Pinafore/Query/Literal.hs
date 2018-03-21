module Pinafore.Query.Literal where

import Pinafore.Query.Value
import Shapes
import Truth.Core

data Literal baseedit t
    = LiteralConstant t
    | LiteralFunction (QImLiteral baseedit t)

instance Functor (Literal baseedit) where
    fmap ab (LiteralConstant a) = LiteralConstant $ ab a
    fmap ab (LiteralFunction a) = LiteralFunction $ funcEditFunction (fmap ab) . a

instance Applicative (Literal baseedit) where
    pure = LiteralConstant
    LiteralConstant ab <*> LiteralConstant a = LiteralConstant $ ab a
    lab <*> la = let
        fab = literalToFunction lab
        fa = literalToFunction la
        in LiteralFunction $ funcEditFunction (\(mab, ma) -> mab <*> ma) . pairWholeEditFunction fab fa

literalToFunction :: Literal baseedit t -> QImLiteral baseedit t
literalToFunction (LiteralConstant t) = constEditFunction $ Just t
literalToFunction (LiteralFunction t) = t
