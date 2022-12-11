module Pinafore.Language.DefDoc where

import Pinafore.Language.Name
import Pinafore.Markdown
import Shapes

data DocItem
    = HeadingDocItem { diTitle :: Markdown }
    | NamespaceDocItem { diNamespace :: NamespaceRef }
    | ValueDocItem { diName :: FullNameRef
                   , diType :: NamedText }
    | ValuePatternDocItem { diName :: FullNameRef
                          , diType :: NamedText }
    | SpecialFormDocItem { diName :: FullNameRef
                         , diParams :: [NamedText]
                         , diType :: NamedText }
    | TypeDocItem { diName :: FullNameRef
                  , diParams :: [NamedText] }
    | SupertypeDocItem { diName :: FullNameRef
                       , diType :: NamedText }
    | SubtypeRelationDocItem { diSubtype :: NamedText
                             , diSupertype :: NamedText }

instance Show DocItem where
    show (HeadingDocItem t) = "heading " <> show t
    show (NamespaceDocItem n) = "namespace " <> show n
    show (ValueDocItem n t) = "val " <> show n <> ": " <> unpack (toText t)
    show (ValuePatternDocItem n t) = "val+pat " <> show n <> ": " <> unpack (toText t)
    show (SpecialFormDocItem n pp t) =
        "spform " <> show n <> mconcat (fmap (\p -> " " <> unpack (toText p)) pp) <> ": " <> unpack (toText t)
    show (TypeDocItem n pp) = "type " <> show n <> mconcat (fmap (\p -> " " <> unpack (toText p)) pp)
    show (SupertypeDocItem n t) = "sval " <> show n <> ": " <> unpack (toText t)
    show (SubtypeRelationDocItem a b) = "subtype " <> unpack (toText a) <> " <: " <> unpack (toText b)

diNameLens :: Applicative m => (FullNameRef -> m FullNameRef) -> DocItem -> m DocItem
diNameLens f ValueDocItem {..} = fmap (\n -> ValueDocItem {diName = n, ..}) $ f diName
diNameLens f ValuePatternDocItem {..} = fmap (\n -> ValuePatternDocItem {diName = n, ..}) $ f diName
diNameLens f SpecialFormDocItem {..} = fmap (\n -> SpecialFormDocItem {diName = n, ..}) $ f diName
diNameLens f TypeDocItem {..} = fmap (\n -> TypeDocItem {diName = n, ..}) $ f diName
diNameLens f SupertypeDocItem {..} = fmap (\n -> SupertypeDocItem {diName = n, ..}) $ f diName
diNameLens _ di = pure di

diMatchNameOrSubtypeRel :: FullName -> DocItem -> Bool
diMatchNameOrSubtypeRel _ SubtypeRelationDocItem {} = True
diMatchNameOrSubtypeRel n di =
    getAny $
    execWriter $ diNameLens (\name -> (tell $ Any $ n == fullNameRefInNamespace RootNamespace name) >> return name) di

data DefDoc = MkDefDoc
    { docItem :: DocItem
    , docDescription :: Markdown
    }

instance Show DefDoc where
    show (MkDefDoc i d) = show (i, d)

instance NamespaceRelative DocItem where
    namespaceRelative nsn (NamespaceDocItem nr) = NamespaceDocItem $ namespaceRelative nsn nr
    namespaceRelative nsn di = runIdentity $ diNameLens (Identity . namespaceRelative nsn) di

instance NamespaceRelative DefDoc where
    namespaceRelative nsn MkDefDoc {..} = MkDefDoc {docItem = namespaceRelative nsn docItem, ..}
