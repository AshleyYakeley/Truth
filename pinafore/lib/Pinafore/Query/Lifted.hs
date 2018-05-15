module Pinafore.Query.Lifted where

import Pinafore.Query.Types
import Shapes
import Truth.Core

data Lifted baseedit t
    = LiftedConstant t
    | LiftedFunction (QLiteral baseedit t)

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

nullLifted :: forall baseedit t. Lifted baseedit t
nullLifted = LiftedFunction $ constEditFunction Nothing

liftedToFunction :: Lifted baseedit t -> QLiteral baseedit t
liftedToFunction (LiftedConstant t) = constEditFunction $ Just t
liftedToFunction (LiftedFunction t) = t

ioLifted :: Lifted baseedit (IO t) -> Lifted baseedit t
ioLifted (LiftedConstant iot) = LiftedFunction $ ioConstEditFunction $ fmap Just iot
ioLifted (LiftedFunction litiot) = LiftedFunction $ ioWholeEditFunction sequence . litiot

unLifted :: Lifted baseedit t -> QActionM baseedit (Maybe t)
unLifted (LiftedConstant t) = return $ Just t
unLifted (LiftedFunction lt) = qGetFunctionValue lt

maybeLifted :: Lifted baseedit (Maybe t) -> Lifted baseedit t
maybeLifted (LiftedConstant (Just t)) = LiftedConstant t
maybeLifted lmt = LiftedFunction $ funcEditFunction (\mmt -> mmt >>= id) . liftedToFunction lmt

liftedMaybe :: Lifted baseedit t -> Lifted baseedit (Maybe t)
liftedMaybe (LiftedConstant t) = LiftedConstant $ Just t
liftedMaybe (LiftedFunction ft) = LiftedFunction $ funcEditFunction Just . ft
