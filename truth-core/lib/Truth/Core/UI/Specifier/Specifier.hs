module Truth.Core.UI.Specifier.Specifier where

import Truth.Core.Import
import Truth.Core.UI.View.CreateView

data UISpec where
    MkUISpec
        :: forall (t :: Type). (Show t, UIType t)
        => t
        -> UISpec

instance Show UISpec where
    show (MkUISpec uit) = show uit

class UIType (t :: Type) where
    uiWitness :: IOWitness t

isUISpec ::
       forall t. UIType t
    => UISpec
    -> Maybe t
isUISpec (MkUISpec (uit :: t')) = do
    Refl <- testEquality (uiWitness @t) (uiWitness @t')
    return uit

type CVUISpec = CreateView UISpec

mkCVUISpec ::
       forall (t :: Type). (Show t, UIType t)
    => t
    -> CVUISpec
mkCVUISpec uit = return $ MkUISpec uit
