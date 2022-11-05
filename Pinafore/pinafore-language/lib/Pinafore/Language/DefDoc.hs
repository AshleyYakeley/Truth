module Pinafore.Language.DefDoc where

import Pinafore.Language.Name
import Pinafore.Markdown
import Shapes

data DocItem
    = ValueDocItem { diName :: FullName
                   , diType :: Text }
    | ValuePatternDocItem { diName :: FullName
                          , diType :: Text }
    | SpecialFormDocItem { diName :: FullName
                         , diParams :: [Text]
                         , diType :: Text }
    | TypeDocItem { diName :: FullName
                  , diParams :: [Text] }
    | SupertypeDocItem { diName :: FullName
                       , diType :: Text }
    | SubtypeRelationDocItem { diSubtype :: Text
                             , diSupertype :: Text }

diMatchName :: FullName -> DocItem -> Bool
diMatchName t ValueDocItem {..} = t == diName
diMatchName t ValuePatternDocItem {..} = t == diName
diMatchName t SpecialFormDocItem {..} = t == diName
diMatchName t TypeDocItem {..} = t == diName
diMatchName t SupertypeDocItem {..} = t == diName
diMatchName _ SubtypeRelationDocItem {} = False

data DefDoc = MkDefDoc
    { docItem :: DocItem
    , docDescription :: Markdown
    }
