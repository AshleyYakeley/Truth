module Pinafore.Language.Value.Model where

import Changes.Core
import Pinafore.Base
import Pinafore.Context
import Pinafore.Language.Value.Instances ()
import Shapes

data LangModel where
    MkLangModel :: WModel update -> LangModel

langModelSubscribe :: (?qcontext :: QContext) => LangModel -> Action () -> Action ()
langModelSubscribe (MkLangModel (MkWModel model)) update =
    actionLiftView $ viewBindModel model Nothing (return ()) mempty $ \() _ -> runAction update
