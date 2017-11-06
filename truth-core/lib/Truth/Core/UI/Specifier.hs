module Truth.Core.UI.Specifier where

import Truth.Core.Import

type Aspect edit = IO (Maybe (String, UISpec edit))

data UISpec (edit :: *) where
    MkUISpec
        :: forall (t :: * -> *) (edit :: *). (Show (t edit), UIType t)
        => t edit
        -> UISpec edit

instance Show (UISpec edit) where
    show (MkUISpec tedit) = show tedit

class UIType (t :: * -> *) where
    uiWitness :: IOWitness t

isUISpec ::
       forall t edit. UIType t
    => UISpec edit
    -> Maybe (t edit)
isUISpec (MkUISpec (tedit :: t' edit)) = do
    Refl <- testEquality (uiWitness @t) (uiWitness @t')
    return tedit
