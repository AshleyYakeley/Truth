module Pinafore.Language.Value.Model where

import Import
import Pinafore.Language.Value.Instances ()
import Pinafore.Language.Value.Task

type InvertibleModelLens t =
    forall m.
    Functor m =>
    (forall update. Maybe (Dict (InvertibleEdit (UpdateEdit update))) -> Model update -> m (Model update)) -> t -> m t

class IsInvertibleModel t where
    invertibleModelLens :: InvertibleModelLens t

instance IsInvertibleModel (ImmutableWholeModel a) where
    invertibleModelLens f (MkImmutableWholeModel model) = fmap MkImmutableWholeModel $ wInvertibleModelLens f model

wUninvertibleModelLens :: forall update. InvertibleModelLens (WModel update)
wUninvertibleModelLens f (MkWModel model) = fmap MkWModel $ f Nothing model

wInvertibleModelLens ::
    forall update.
    InvertibleEdit (UpdateEdit update) =>
    InvertibleModelLens (WModel update)
wInvertibleModelLens f (MkWModel model) = fmap MkWModel $ f (Just Dict) model

toLangModel ::
    forall t.
    IsInvertibleModel t =>
    t ->
    LangModel
toLangModel m = getConst $ invertibleModelLens (\_ model -> Const $ MkLangModel $ MkWModel model) m

data LangModel where
    MkLangModel :: forall update. WModel update -> LangModel

langModelSubscribe :: LangModel -> Action () -> Action ()
langModelSubscribe (MkLangModel (MkWModel model)) update =
    actionLiftView $ viewBindModel model Nothing (return ()) mempty $ \() _ -> runAction update

langModelUpdatesTask :: LangModel -> LangTask ()
langModelUpdatesTask (MkLangModel (MkWModel model)) = liftTask $ modelUpdatesTask model

langModelCommitsTask :: LangModel -> LangTask ()
langModelCommitsTask (MkLangModel (MkWModel model)) = liftTask $ modelCommitsTask model
