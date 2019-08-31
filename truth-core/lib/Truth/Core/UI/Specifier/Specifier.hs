module Truth.Core.UI.Specifier.Specifier where

import Truth.Core.Import

data UISpec (sel :: Type) (update :: Type) where
    MkUISpec
        :: forall (t :: Type -> Type -> Type) (sel :: Type) (update :: Type). (Show (t sel update), UIType t)
        => t sel update
        -> UISpec sel update

instance Show (UISpec sel update) where
    show (MkUISpec tedit) = show tedit

class UIType (t :: Type -> Type -> Type) where
    uiWitness :: IOWitness t

isUISpec ::
       forall t sel update. UIType t
    => UISpec sel update
    -> Maybe (t sel update)
isUISpec (MkUISpec (tedit :: t' sel update)) = do
    Refl <- testEquality (uiWitness @t) (uiWitness @t')
    return tedit

type Aspect sel = IO (Maybe sel)

noAspect :: Aspect sel
noAspect = return Nothing
