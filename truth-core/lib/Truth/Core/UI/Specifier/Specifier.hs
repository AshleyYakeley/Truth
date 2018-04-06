module Truth.Core.UI.Specifier.Specifier where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types

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

data UIWindow edit = MkUIWindow
    { uiTitle :: EditFunction edit (WholeEdit Text)
    , uiContent :: UISpec edit
    }

type Aspect edit = IO (Maybe (UIWindow edit))