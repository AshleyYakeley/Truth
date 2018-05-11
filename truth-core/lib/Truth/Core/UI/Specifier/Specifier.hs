module Truth.Core.UI.Specifier.Specifier where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types

data UISpec (seledit :: *) (edit :: *) where
    MkUISpec
        :: forall (t :: * -> * -> *) (seledit :: *) (edit :: *). (Show (t seledit edit), UIType t)
        => t seledit edit
        -> UISpec seledit edit

instance Show (UISpec seledit edit) where
    show (MkUISpec tedit) = show tedit

class UIType (t :: * -> * -> *) where
    uiWitness :: IOWitness t

isUISpec ::
       forall t seledit edit. UIType t
    => UISpec seledit edit
    -> Maybe (t seledit edit)
isUISpec (MkUISpec (tedit :: t' seledit edit)) = do
    Refl <- testEquality (uiWitness @t) (uiWitness @t')
    return tedit

data UIWindow edit = forall seledit. MkUIWindow
    { uiTitle :: EditFunction edit (WholeEdit Text)
    , uiContent :: UISpec seledit edit
    }

data UIAspect seledit edit = MkUIAspect
    { uiaWindow :: UIWindow edit
    , uiaLens :: EditLens edit seledit
    }

type Aspect seledit edit = IO (Maybe (UIAspect seledit edit))

noAspect :: Aspect seledit edit
noAspect = return Nothing
