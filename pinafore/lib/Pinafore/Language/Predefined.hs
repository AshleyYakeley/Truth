module Pinafore.Language.Predefined
    ( PinaforeContext
    , DefDoc(..)
    , DocTree(..)
    , runDocTree
    , predefinedBindings
    , predefinedPatternConstructors
    , predefinedDoc
    , outputLn
    ) where

import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.Expression
import Pinafore.Language.Name
import Pinafore.Language.Predefined.Base
import Pinafore.Language.Predefined.Defs
import Pinafore.Language.Predefined.File
import Pinafore.Language.Predefined.UI
import Pinafore.Language.TypeSystem
import Shapes

predefinitions :: DocTree (BindDoc baseupdate)
predefinitions = MkDocTree "" "" $ base_predefinitions <> ui_predefinitions <> file_predefinitions

predefinedDoc :: DocTree DefDoc
predefinedDoc = fmap bdDoc $ predefinitions

predefinedBindings :: (?pinafore :: PinaforeContext) => Map Name (QValue baseupdate)
predefinedBindings =
    mapFromList $
    catMaybes $
    toList $
    fmap
        (\doc -> do
             val <- bdValue doc
             return (bdName doc, val ?pinafore)) $
    predefinitions

predefinedPatternConstructors :: Map Name (PinaforePatternConstructor baseupdate)
predefinedPatternConstructors =
    mapFromList $
    catMaybes $
    toList $
    fmap
        (\doc -> do
             pat <- bdPattern doc
             return (bdName doc, pat)) $
    predefinitions
