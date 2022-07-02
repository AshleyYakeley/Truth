module Pinafore.Language.DefDoc where

import Pinafore.Language.Name
import Pinafore.Markdown
import Shapes

data DocItem
    = ValueDocItem { diName :: Name
                   , diType :: Text }
    | ValuePatternDocItem { diName :: Name
                          , diType :: Text }
    | SpecialFormDocItem { diName :: Name
                         , diParams :: [Text]
                         , diType :: Text }
    | TypeDocItem { diName :: Name
                  , diParams :: [Text] }
    | SupertypeDocItem { diName :: Name
                       , diType :: Text }
    | SubtypeRelationDocItem { diSubtype :: Text
                             , diSupertype :: Text }

diMatchName :: Name -> DocItem -> Bool
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
