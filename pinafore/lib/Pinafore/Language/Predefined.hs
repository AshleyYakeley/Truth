module Pinafore.Language.Predefined
    ( PinaforePredefinitions
    , PinaforeContext
    , DefDoc(..)
    , DocTree(..)
    , runDocTree
    , predefinedBindings
    , predefinedPatternConstructors
    , predefinedDoc
    , outputLn
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

type PinaforePredefinitions baseupdate
     = ( HasPinaforeEntityUpdate baseupdate
       , HasPinaforeFileUpdate baseupdate
       , BaseChangeLens MemoryCellUpdate baseupdate
       , BaseChangeLens (ROWUpdate UTCTime) baseupdate
       , BaseChangeLens (ROWUpdate TimeZone) baseupdate)

predefinitions ::
       forall baseupdate. PinaforePredefinitions baseupdate
    => DocTree (BindDoc baseupdate)
predefinitions = MkDocTree "" "" $ base_predefinitions <> ui_predefinitions <> file_predefinitions

predefinedDoc ::
       forall baseupdate. PinaforePredefinitions baseupdate
    => DocTree DefDoc
predefinedDoc = fmap bdDoc $ predefinitions @baseupdate

predefinedBindings ::
       forall baseupdate. (PinaforePredefinitions baseupdate, ?pinafore :: PinaforeContext baseupdate)
    => StrictMap Name (QValue baseupdate)
predefinedBindings =
    mapFromList $
    catMaybes $
    toList $
    fmap
        (\doc -> do
             val <- bdValue doc
             return (bdName doc, val ?pinafore)) $
    predefinitions @baseupdate

predefinedPatternConstructors ::
       forall baseupdate. PinaforePredefinitions baseupdate
    => StrictMap Name (PinaforePatternConstructor baseupdate)
predefinedPatternConstructors =
    mapFromList $
    catMaybes $
    toList $
    fmap
        (\doc -> do
             pat <- bdPattern doc
             return (bdName doc, pat)) $
    predefinitions @baseupdate
