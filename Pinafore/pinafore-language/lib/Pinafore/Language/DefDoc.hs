module Pinafore.Language.DefDoc where

import Pinafore.Language.Name
import Pinafore.Markdown
import Shapes

data DocItem
    = HeadingDocItem { diTitle :: MarkdownText }
    | NamespaceDocItem { diNamespace :: NamespaceRef }
    | ValueDocItem { diName :: FullNameRef
                   , diType :: NamedText }
    | SignatureDocItem { diSigName :: Name
                       , diType :: NamedText }
    | ValuePatternDocItem { diName :: FullNameRef
                          , diType :: NamedText }
    | SpecialFormDocItem { diName :: FullNameRef
                         , diParams :: [NamedText]
                         , diType :: NamedText }
    | TypeDocItem { diName :: FullNameRef
                  , diParams :: [NamedText] }
    | SubtypeRelationDocItem { diSubtype :: NamedText
                             , diSupertype :: NamedText }

instance Show DocItem where
    show (HeadingDocItem t) = "heading " <> show t
    show (NamespaceDocItem n) = "namespace " <> show n
    show (ValueDocItem n t) = "val " <> show n <> ": " <> unpack (toText t)
    show (SignatureDocItem n t) = "sig " <> show n <> ": " <> unpack (toText t)
    show (ValuePatternDocItem n t) = "val+pat " <> show n <> ": " <> unpack (toText t)
    show (SpecialFormDocItem n pp t) =
        "spform " <> show n <> mconcat (fmap (\p -> " " <> unpack (toText p)) pp) <> ": " <> unpack (toText t)
    show (TypeDocItem n pp) = "type " <> show n <> mconcat (fmap (\p -> " " <> unpack (toText p)) pp)
    show (SubtypeRelationDocItem a b) = "subtype " <> unpack (toText a) <> " <: " <> unpack (toText b)

diNameLens :: Applicative m => (FullNameRef -> m FullNameRef) -> DocItem -> m DocItem
diNameLens f ValueDocItem {..} = fmap (\n -> ValueDocItem {diName = n, ..}) $ f diName
diNameLens f ValuePatternDocItem {..} = fmap (\n -> ValuePatternDocItem {diName = n, ..}) $ f diName
diNameLens f SpecialFormDocItem {..} = fmap (\n -> SpecialFormDocItem {diName = n, ..}) $ f diName
diNameLens f TypeDocItem {..} = fmap (\n -> TypeDocItem {diName = n, ..}) $ f diName
diNameLens _ di = pure di

diMatchNameOrSubtypeRel :: FullName -> DocItem -> Bool
diMatchNameOrSubtypeRel n di =
    getAll $
    execWriter $ diNameLens (\name -> (tell $ All $ n == namespaceConcatFullName RootNamespace name) >> return name) di

data DefDoc = MkDefDoc
    { docItem :: DocItem
    , docDescription :: RawMarkdown
    }

instance Show DefDoc where
    show (MkDefDoc i d) = show (i, d)

instance NamespaceConcat DocItem where
    namespaceConcat nsn (NamespaceDocItem nr) = NamespaceDocItem $ namespaceConcat nsn nr
    namespaceConcat nsn di = runIdentity $ diNameLens (Identity . namespaceConcat nsn) di

instance NamespaceConcat DefDoc where
    namespaceConcat nsn MkDefDoc {..} = MkDefDoc {docItem = namespaceConcat nsn docItem, ..}
