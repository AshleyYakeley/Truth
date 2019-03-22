module Truth.Core.UI.Specifier.Specifier where

import Truth.Core.Import

data UISpec (sel :: Type) (edit :: Type) where
    MkUISpec
        :: forall (t :: Type -> Type -> Type) (sel :: Type) (edit :: Type). (Show (t sel edit), UIType t)
        => t sel edit
        -> UISpec sel edit

instance Show (UISpec sel edit) where
    show (MkUISpec tedit) = show tedit

class UIType (t :: Type -> Type -> Type) where
    uiWitness :: IOWitness t

isUISpec ::
       forall t sel edit. UIType t
    => UISpec sel edit
    -> Maybe (t sel edit)
isUISpec (MkUISpec (tedit :: t' sel edit)) = do
    Refl <- testEquality (uiWitness @t) (uiWitness @t')
    return tedit

type Aspect sel = IO (Maybe sel)

noAspect :: Aspect sel
noAspect = return Nothing
