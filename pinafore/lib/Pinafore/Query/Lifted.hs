module Pinafore.Query.Lifted where

import Pinafore.Query.Types
import Shapes
import Truth.Core

data Lifted baseedit t
    = LiftedConstant t
    | LiftedFunction (QImLiteral baseedit t)

instance Functor (Lifted baseedit) where
    fmap ab (LiftedConstant a) = LiftedConstant $ ab a
    fmap ab (LiftedFunction a) = LiftedFunction $ funcEditFunction (fmap ab) . a

instance Applicative (Lifted baseedit) where
    pure = LiftedConstant
    LiftedConstant ab <*> LiftedConstant a = LiftedConstant $ ab a
    lab <*> la = let
        fab = liftedToFunction lab
        fa = liftedToFunction la
        in LiftedFunction $ funcEditFunction (\(mab, ma) -> mab <*> ma) . pairWholeEditFunction fab fa

liftedToFunction :: Lifted baseedit t -> QImLiteral baseedit t
liftedToFunction (LiftedConstant t) = constEditFunction $ Just t
liftedToFunction (LiftedFunction t) = t

ioLifted :: Lifted baseedit (IO t) -> Lifted baseedit t
ioLifted (LiftedConstant iot) = LiftedFunction $ ioConstEditFunction $ fmap Just iot
ioLifted (LiftedFunction litiot) = LiftedFunction $ ioWholeEditFunction sequence . litiot