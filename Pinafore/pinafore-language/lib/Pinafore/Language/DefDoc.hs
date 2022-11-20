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

diNameLens :: Applicative m => (FullName -> m FullName) -> DocItem -> m DocItem
diNameLens f ValueDocItem {..} = fmap (\n -> ValueDocItem {diName = n, ..}) $ f diName
diNameLens f ValuePatternDocItem {..} = fmap (\n -> ValuePatternDocItem {diName = n, ..}) $ f diName
diNameLens f SpecialFormDocItem {..} = fmap (\n -> SpecialFormDocItem {diName = n, ..}) $ f diName
diNameLens f TypeDocItem {..} = fmap (\n -> TypeDocItem {diName = n, ..}) $ f diName
diNameLens f SupertypeDocItem {..} = fmap (\n -> SupertypeDocItem {diName = n, ..}) $ f diName
diNameLens _ SubtypeRelationDocItem {..} = pure SubtypeRelationDocItem {..}

diMatchName :: FullName -> DocItem -> Bool
diMatchName n di = getAny $ execWriter $ diNameLens (\name -> (tell $ Any $ n == name) >> return name) di

data DefDoc = MkDefDoc
    { docItem :: DocItem
    , docDescription :: Markdown
    }

instance NamespaceRelative DocItem where
    namespaceRelative nsn di = runIdentity $ diNameLens (Identity . namespaceRelative nsn) di

instance NamespaceRelative DefDoc where
    namespaceRelative nsn MkDefDoc {..} = MkDefDoc {docItem = namespaceRelative nsn docItem, ..}
