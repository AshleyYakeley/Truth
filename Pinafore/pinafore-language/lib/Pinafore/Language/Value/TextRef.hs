module Pinafore.Language.Value.TextRef where

import Changes.Core
import Pinafore.Base
import Pinafore.Language.Value.WholeRef
import Shapes

newtype LangTextRef =
    MkLangTextRef (WModel (StringUpdate Text))

langWholeRefToTextRef :: LangWholeRef '( Text, Text) -> LangTextRef
langWholeRefToTextRef wref =
    MkLangTextRef $ eaMap (convertChangeLens . unknownValueChangeLens mempty) $ langWholeRefToValue wref

langTextRefToWholeRef :: LangTextRef -> LangWholeRef '( Text, Text)
langTextRefToWholeRef (MkLangTextRef model) =
    MutableLangWholeRef $ eaMap (biToKnowWhole . singleBiChangeLens . convertChangeLens) model

langTextRefGetLength :: LangTextRef -> PinaforeAction SequencePoint
langTextRefGetLength (MkLangTextRef model) = pinaforeRefGet model $ readM StringReadLength

langTextRefGet :: LangTextRef -> PinaforeAction Text
langTextRefGet (MkLangTextRef model) = pinaforeRefGet model $ readableToSubject readM

langTextRefSet :: Text -> LangTextRef -> PinaforeAction ()
langTextRefSet t (MkLangTextRef model) = pinaforeRefPush model $ pure $ StringReplaceWhole t

langTextRefGetSection :: SequenceRun -> LangTextRef -> PinaforeAction Text
langTextRefGetSection run (MkLangTextRef model) = pinaforeRefGet model $ readM $ StringReadSection run

langTextRefSetSection :: SequenceRun -> Text -> LangTextRef -> PinaforeAction ()
langTextRefSetSection run t (MkLangTextRef model) = pinaforeRefPush model $ pure $ StringReplaceSection run t

langTextRefSection :: SequenceRun -> LangTextRef -> PinaforeAction LangTextRef
langTextRefSection run (MkLangTextRef model) = do
    secmodel <- pinaforeFloatMap (stringSectionLens run) model
    return $ MkLangTextRef secmodel
