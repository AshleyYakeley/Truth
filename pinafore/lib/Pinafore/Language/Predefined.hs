module Pinafore.Language.Predefined
    ( PinaforeContext
    , DefDoc(..)
    , DocTree(..)
    , runDocTree
    , predefinedBindings
    , predefinedPatternConstructors
    , predefinedSpecialForms
    , predefinedDoc
    , outputLn
    ) where

import Pinafore.Context
import Pinafore.Language.DocTree
import Pinafore.Language.Expression
import Pinafore.Language.Name
import Pinafore.Language.Predefined.Base
import Pinafore.Language.Predefined.Defs
import Pinafore.Language.Predefined.File
import Pinafore.Language.Predefined.SpecialForms
import Pinafore.Language.Predefined.UI
import Pinafore.Language.Type
import Shapes

predefinitions :: DocTree BindDoc
predefinitions =
    MkDocTree "Predefined Bindings" "Entries in italics are supertypes of existing types, for convenience." $
    special_forms <> base_predefinitions <> ui_predefinitions <> file_predefinitions

predefinedDoc :: DocTree DefDoc
predefinedDoc = fmap bdDoc $ predefinitions

toPredefined :: (DefBind -> Maybe t) -> Map Name t
toPredefined f =
    mapFromList $
    catMaybes $
    toList $
    fmap
        (\doc -> do
             (name, db) <- bdBind doc
             t <- f db
             return (name, t)) $
    predefinitions

predefinedBindings :: (?pinafore :: PinaforeContext) => Map Name QValue
predefinedBindings =
    toPredefined $ \db -> do
        val <-
            case db of
                ValueDefBind val -> Just val
                _ -> Nothing
        return $ val ?pinafore

predefinedPatternConstructors :: Map Name (QValue, PinaforePatternConstructor)
predefinedPatternConstructors =
    toPredefined $ \db ->
        case db of
            PatternDefBind val pat -> Just (val, pat)
            _ -> Nothing

predefinedSpecialForms :: Map Name PinaforeSpecialForm
predefinedSpecialForms =
    toPredefined $ \db ->
        case db of
            SpecialFormDefBind sf -> return sf
            _ -> Nothing
