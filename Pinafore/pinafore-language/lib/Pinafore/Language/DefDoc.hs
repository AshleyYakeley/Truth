module Pinafore.Language.DefDoc where

import Pinafore.Markdown
import Shapes

data DocType
    = ValueDocType
    | ValuePatternDocType
    | TypeDocType
    | SupertypeDocType
    | SubtypeRelationDocType
    deriving (Eq)

data DefDoc = MkDefDoc
    { docName :: Text
    , docValueType :: Text
    , docType :: DocType
    , docDescription :: Markdown
    }
