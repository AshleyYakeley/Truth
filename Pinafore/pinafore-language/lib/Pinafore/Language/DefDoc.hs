module Pinafore.Language.DefDoc where

import Pinafore.Markdown
import Shapes

data DocItem
    = ValueDocItem { diName :: Text
                   , diType :: Text }
    | ValuePatternDocItem { diName :: Text
                          , diType :: Text }
    | TypeDocItem { diName :: Text
                  , diParams :: [Text] }
    | SupertypeDocItem { diName :: Text
                       , diType :: Text }
    | SubtypeRelationDocItem { diSubtype :: Text
                             , diSupertype :: Text }

diMatchName :: Text -> DocItem -> Bool
diMatchName t ValueDocItem {..} = t == diName
diMatchName t ValuePatternDocItem {..} = t == diName
diMatchName t TypeDocItem {..} = t == diName
diMatchName t SupertypeDocItem {..} = t == diName
diMatchName _ SubtypeRelationDocItem {} = False

data DefDoc = MkDefDoc
    { docItem :: DocItem
    , docDescription :: Markdown
    }
