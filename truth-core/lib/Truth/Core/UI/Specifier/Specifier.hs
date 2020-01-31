module Truth.Core.UI.Specifier.Specifier where

import Truth.Core.Import

data UISpec (sel :: Type) where
    MkUISpec
        :: forall (t :: Type -> Type) (sel :: Type). (Show (t sel), UIType t)
        => t sel
        -> UISpec sel

instance Show (UISpec sel) where
    show (MkUISpec tedit) = show tedit

class UIType (t :: Type -> Type) where
    uiWitness :: IOWitness t

isUISpec ::
       forall t sel. UIType t
    => UISpec sel
    -> Maybe (t sel)
isUISpec (MkUISpec (tedit :: t' sel)) = do
    Refl <- testEquality (uiWitness @t) (uiWitness @t')
    return tedit

type LUISpec sel = LifeCycleIO (UISpec sel)

mkLUISpec ::
       forall (t :: Type -> Type) (sel :: Type). (Show (t sel), UIType t)
    => t sel
    -> LUISpec sel
mkLUISpec tsel = return $ MkUISpec tsel

type Aspect sel = LifeCycleIO (Maybe sel)

noAspect :: Aspect sel
noAspect = return Nothing
