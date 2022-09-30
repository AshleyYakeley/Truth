module Pinafore.Language.Value.TextModel where

import Changes.Core
import Pinafore.Base
import Pinafore.Language.Value.Model
import Pinafore.Language.Value.WholeModel
import Shapes

newtype LangTextModel =
    MkLangTextModel (WModel (StringUpdate Text))

langTextModelToModel :: LangTextModel -> LangModel
langTextModelToModel (MkLangTextModel model) = MkLangModel model

langWholeModelToTextModel :: LangWholeModel '( Text, Text) -> LangTextModel
langWholeModelToTextModel wref =
    MkLangTextModel $ eaMap (convertChangeLens . unknownValueChangeLens mempty) $ langWholeModelToValue wref

langTextModelToWholeModel :: LangTextModel -> LangWholeModel '( Text, Text)
langTextModelToWholeModel (MkLangTextModel model) =
    MutableLangWholeModel $ eaMap (biToKnowWhole . singleBiChangeLens . convertChangeLens) model

langTextModelGetLength :: LangTextModel -> PinaforeAction SequencePoint
langTextModelGetLength (MkLangTextModel model) = pinaforeModelGet model $ readM StringReadLength

langTextModelGet :: LangTextModel -> PinaforeAction Text
langTextModelGet (MkLangTextModel model) = pinaforeModelGet model $ readableToSubject readM

langTextModelSet :: Text -> LangTextModel -> PinaforeAction ()
langTextModelSet t (MkLangTextModel model) = pinaforeModelPush model $ pure $ StringReplaceWhole t

langTextModelGetSection :: SequenceRun -> LangTextModel -> PinaforeAction Text
langTextModelGetSection run (MkLangTextModel model) = pinaforeModelGet model $ readM $ StringReadSection run

langTextModelSetSection :: SequenceRun -> Text -> LangTextModel -> PinaforeAction ()
langTextModelSetSection run t (MkLangTextModel model) = pinaforeModelPush model $ pure $ StringReplaceSection run t

langTextModelSection :: SequenceRun -> LangTextModel -> PinaforeAction LangTextModel
langTextModelSection run (MkLangTextModel model) = do
    secmodel <- pinaforeFloatMap (stringSectionLens run) model
    return $ MkLangTextModel secmodel
