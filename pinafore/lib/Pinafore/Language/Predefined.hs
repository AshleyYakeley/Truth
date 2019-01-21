module Pinafore.Language.Predefined
    ( PinaforeContext
    , DefDoc(..)
    , DocTree(..)
    , runDocTree
    , predefinedBindings
    , predefinedPatternConstructors
    , predefinedDoc
    , outputln
    ) where

import Pinafore.Base
import Pinafore.Language.Doc
import Pinafore.Language.Expression
import Pinafore.Language.Name
import Pinafore.Language.Predefined.Base
import Pinafore.Language.Predefined.Defs
import Pinafore.Language.Predefined.File
import Pinafore.Language.Predefined.UI
import Pinafore.Language.Type
import Pinafore.Storage.File
import Shapes

predefinitions ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => DocTree (BindDoc baseedit)
predefinitions =
    MkDocTree "Predefined" "" $
    base_predefinitions <> [TreeDocTreeEntry $ ui_predefinitions, TreeDocTreeEntry file_predefinitions]

predefinedDoc ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => DocTree DefDoc
predefinedDoc = fmap bdDoc $ predefinitions @baseedit

predefinedBindings ::
       forall baseedit.
       (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit, ?pinafore :: PinaforeContext baseedit)
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
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
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
