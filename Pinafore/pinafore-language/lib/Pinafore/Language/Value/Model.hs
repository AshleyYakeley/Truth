module Pinafore.Language.Value.Model where

import Import
import Pinafore.Language.Value.Instances ()
import Pinafore.Language.Value.Task

class IsModel a where
    modelLens :: forall m. Functor m => (forall update. Model update -> m (Model update)) -> a -> m a

modelMap :: forall a. IsModel a => (forall update. Model update -> Model update) -> a -> a
modelMap f x = runIdentity $ modelLens (\model -> Identity $ f model) x

toLangModel :: forall a. IsModel a => a -> LangModel
toLangModel m = getConst $ modelLens (\model -> Const $ MkLangModel $ MkWModel model) m

instance IsModel (Model update) where
    modelLens f x = f x

instance IsModel (WModel update) where
    modelLens f (MkWModel model) = fmap MkWModel $ f model

instance IsModel (ImmutableWholeModel a) where
    modelLens f (MkImmutableWholeModel model) = fmap MkImmutableWholeModel $ modelLens f model

type InvertibleModelLens a =
    forall m.
    Functor m =>
    (forall update. Maybe (Dict (InvertibleEdit (UpdateEdit update))) -> Model update -> m (Model update)) -> a -> m a

class IsModel a => IsInvertibleModel a where
    invertibleModelLens :: InvertibleModelLens a

instance IsInvertibleModel (ImmutableWholeModel a) where
    invertibleModelLens f (MkImmutableWholeModel model) = fmap MkImmutableWholeModel $ wInvertibleModelLens f model

wUninvertibleModelLens :: forall update. InvertibleModelLens (WModel update)
wUninvertibleModelLens f (MkWModel model) = fmap MkWModel $ f Nothing model

wInvertibleModelLens ::
    forall update.
    InvertibleEdit (UpdateEdit update) =>
    InvertibleModelLens (WModel update)
wInvertibleModelLens f (MkWModel model) = fmap MkWModel $ f (Just Dict) model

data LangModel where
    MkLangModel :: forall update. WModel update -> LangModel

langModelSubscribe :: LangModel -> Action () -> Action ()
langModelSubscribe (MkLangModel (MkWModel model)) update =
    actionLiftView $ viewBindModel model Nothing (return ()) mempty $ \() _ -> runAction update

langModelUpdatesTask :: LangModel -> LangTask ()
langModelUpdatesTask (MkLangModel (MkWModel model)) = liftTask $ modelUpdatesTask model

langModelCommitsTask :: LangModel -> LangTask ()
langModelCommitsTask (MkLangModel (MkWModel model)) = liftTask $ modelCommitsTask model
