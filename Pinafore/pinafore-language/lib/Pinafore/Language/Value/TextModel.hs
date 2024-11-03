module Pinafore.Language.Value.TextModel where

import Import
import Pinafore.Language.Value.Model
import Pinafore.Language.Value.WholeModel

newtype LangTextModel = MkLangTextModel
    { unLangTextModel :: WModel (StringUpdate Text)
    }

instance IsInvertibleModel LangTextModel where
    invertibleModelLens f (MkLangTextModel model) = fmap MkLangTextModel $ wInvertibleModelLens f model

newMemTextModel :: Action LangTextModel
newMemTextModel = do
    r <- liftIO $ makeMemoryReference mempty $ \_ -> True
    model :: Model (StringUpdate Text) <- actionLiftLifecycle $ makeReflectingModel $ convertReference r
    return $ MkLangTextModel $ MkWModel model

langTextModelToModel :: LangTextModel -> LangModel
langTextModelToModel (MkLangTextModel model) = MkLangModel model

langWholeModelToTextModel :: LangWholeModel '( Text, Text) -> LangTextModel
langWholeModelToTextModel wref =
    MkLangTextModel $ eaMap (convertChangeLens . unknownValueChangeLens mempty) $ langWholeModelToValue wref

langTextModelToWholeModel :: LangTextModel -> LangWholeModel '( Text, Text)
langTextModelToWholeModel (MkLangTextModel model) =
    MutableLangWholeModel $ eaMap (biToKnowWhole . singleBiChangeLens . convertChangeLens) model

langTextModelGetLength :: LangTextModel -> Action SequencePoint
langTextModelGetLength (MkLangTextModel model) = actionModelGet model $ readM StringReadLength

langTextModelGet :: LangTextModel -> Action Text
langTextModelGet (MkLangTextModel model) = actionModelGet model $ readableToSubject readM

langTextModelSet :: Text -> LangTextModel -> Action ()
langTextModelSet t (MkLangTextModel model) = actionModelPush model $ pure $ StringReplaceWhole t

langTextModelGetSection :: SequenceRun -> LangTextModel -> Action Text
langTextModelGetSection run (MkLangTextModel model) = actionModelGet model $ readM $ StringReadSection run

langTextModelSetSection :: SequenceRun -> Text -> LangTextModel -> Action ()
langTextModelSetSection run t (MkLangTextModel model) = actionModelPush model $ pure $ StringReplaceSection run t

langTextModelSection :: SequenceRun -> LangTextModel -> Action LangTextModel
langTextModelSection run (MkLangTextModel model) = do
    secmodel <- actionFloatMap (stringSectionLens run) model
    return $ MkLangTextModel secmodel
