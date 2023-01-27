module Pinafore.Language.DefDoc
    ( DocItem(..)
    , diNamesTraversal
    , diMatchNameOrSubtypeRel
    , DefDoc(..)
    ) where

import Pinafore.Language.Name
import Pinafore.Markdown
import Shapes

data DocItem
    = HeadingDocItem { diTitle :: MarkdownText }
    | NamespaceDocItem { diNamespace :: NamespaceRef }
    | ValueDocItem { diNames :: NonEmpty FullNameRef
                   , diType :: NamedText }
    | SignatureTypeDocItem { diSigName :: Name }
    | SignatureValueDocItem { diSigName :: Name
                            , diType :: NamedText }
    | ValuePatternDocItem { diNames :: NonEmpty FullNameRef
                          , diType :: NamedText }
    | SpecialFormDocItem { diNames :: NonEmpty FullNameRef
                         , diParams :: [NamedText]
                         , diType :: NamedText }
    | TypeDocItem { diNames :: NonEmpty FullNameRef
                  , diParams :: [NamedText] }
    | SubtypeRelationDocItem { diSubtype :: NamedText
                             , diSupertype :: NamedText }

instance Show DocItem where
    show (HeadingDocItem t) = "heading " <> show t
    show (NamespaceDocItem n) = "namespace " <> show n
    show (ValueDocItem n t) = "val " <> show n <> ": " <> unpack (toText t)
    show (SignatureTypeDocItem n) = "sig type " <> show n
    show (SignatureValueDocItem n t) = "sig " <> show n <> ": " <> unpack (toText t)
    show (ValuePatternDocItem n t) = "val+pat " <> show n <> ": " <> unpack (toText t)
    show (SpecialFormDocItem n pp t) =
        "spform " <> show n <> mconcat (fmap (\p -> " " <> unpack (toText p)) pp) <> ": " <> unpack (toText t)
    show (TypeDocItem n pp) = "type " <> show n <> mconcat (fmap (\p -> " " <> unpack (toText p)) pp)
    show (SubtypeRelationDocItem a b) = "subtype " <> unpack (toText a) <> " <: " <> unpack (toText b)

diNamesTraversal :: Applicative m => (NonEmpty FullNameRef -> m (NonEmpty FullNameRef)) -> DocItem -> m DocItem
diNamesTraversal f ValueDocItem {..} = fmap (\n -> ValueDocItem {diNames = n, ..}) $ f diNames
diNamesTraversal f ValuePatternDocItem {..} = fmap (\n -> ValuePatternDocItem {diNames = n, ..}) $ f diNames
diNamesTraversal f SpecialFormDocItem {..} = fmap (\n -> SpecialFormDocItem {diNames = n, ..}) $ f diNames
diNamesTraversal f TypeDocItem {..} = fmap (\n -> TypeDocItem {diNames = n, ..}) $ f diNames
diNamesTraversal _ di = pure di

diNameTraversal :: Applicative m => (FullNameRef -> m FullNameRef) -> DocItem -> m DocItem
diNameTraversal = diNamesTraversal . traverse

diMatchNameOrSubtypeRel :: FullName -> DocItem -> Bool
diMatchNameOrSubtypeRel n di =
    getAll $
    execWriter $
    diNameTraversal (\name -> (tell $ All $ n == namespaceConcatFullName RootNamespace name) >> return name) di

data DefDoc = MkDefDoc
    { docItem :: DocItem
    , docDescription :: RawMarkdown
    }

instance Show DefDoc where
    show (MkDefDoc i d) = show (i, d)

instance NamespaceConcat DocItem where
    namespaceConcat nsn (NamespaceDocItem nr) = NamespaceDocItem $ namespaceConcat nsn nr
    namespaceConcat nsn di = runIdentity $ diNameTraversal (Identity . namespaceConcat nsn) di

instance NamespaceConcat DefDoc where
    namespaceConcat nsn MkDefDoc {..} = MkDefDoc {docItem = namespaceConcat nsn docItem, ..}
