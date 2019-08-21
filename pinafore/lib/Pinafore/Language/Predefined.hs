module Pinafore.Language.Predefined
    ( PinaforePredefinitions
    , PinaforeContext
    , DefDoc(..)
    , DocTree(..)
    , runDocTree
    , predefinedBindings
    , predefinedPatternConstructors
    , predefinedDoc
    , outputln
    ) where

import Data.Time
import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.Expression
import Pinafore.Language.Name
import Pinafore.Language.Predefined.Base
import Pinafore.Language.Predefined.Defs
import Pinafore.Language.Predefined.File
import Pinafore.Language.Predefined.UI
import Pinafore.Language.TypeSystem
import Pinafore.Storage
import Shapes
import Truth.Core

type PinaforePredefinitions baseedit
     = ( HasPinaforeEntityEdit baseedit
       , HasPinaforeFileEdit baseedit
       , BaseEditLens MemoryCellEdit baseedit
       , BaseEditLens (WholeEdit UTCTime) baseedit
       , BaseEditLens (WholeEdit TimeZone) baseedit)

predefinitions ::
       forall baseedit. PinaforePredefinitions baseedit
    => DocTree (BindDoc baseedit)
predefinitions = MkDocTree "" "" $ base_predefinitions <> ui_predefinitions <> file_predefinitions

predefinedDoc ::
       forall baseedit. PinaforePredefinitions baseedit
    => DocTree DefDoc
predefinedDoc = fmap bdDoc $ predefinitions @baseedit

predefinedBindings ::
       forall baseedit. (PinaforePredefinitions baseedit, ?pinafore :: PinaforeContext baseedit)
    => StrictMap Name (QValue baseedit)
predefinedBindings =
    mapFromList $
    catMaybes $
    toList $
    fmap
        (\doc -> do
             val <- bdValue doc
             return (bdName doc, val ?pinafore)) $
    predefinitions @baseedit

predefinedPatternConstructors ::
       forall baseedit. PinaforePredefinitions baseedit
    => StrictMap Name (PinaforePatternConstructor baseedit)
predefinedPatternConstructors =
    mapFromList $
    catMaybes $
    toList $
    fmap
        (\doc -> do
             pat <- bdPattern doc
             return (bdName doc, pat)) $
    predefinitions @baseedit
