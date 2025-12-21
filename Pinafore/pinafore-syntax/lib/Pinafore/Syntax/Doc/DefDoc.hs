module Pinafore.Syntax.Doc.DefDoc
    ( DocTypeParameter (..)
    , DocItem (..)
    , diNamesTraversal
    , diMatchNameOrSubtypeRel
    , DefDoc (..)
    )
where

import Pinafore.Base
import Shapes

import Pinafore.Syntax.Name

data DocTypeParameter
    = CoDocTypeParameter NamedText
    | ContraDocTypeParameter NamedText
    | RangeDocTypeParameter
        NamedText
        NamedText
    | DoubleRangeDocTypeParameter NamedText
    deriving stock Eq

instance ShowNamedText DocTypeParameter where
    showNamedText (CoDocTypeParameter q) = "+" <> q
    showNamedText (ContraDocTypeParameter p) = "-" <> p
    showNamedText (RangeDocTypeParameter p q) = "(-" <> p <> ",+" <> q <> ")"
    showNamedText (DoubleRangeDocTypeParameter v) = v

instance Show DocTypeParameter where
    show p = unpack $ toText $ showNamedText p

data DocItem
    = HeadingDocItem {diTitle :: MarkdownText}
    | ValueDocItem
        { diNames :: NonEmpty FullNameRef
        , diType :: NamedText
        }
    | ValueSignatureDocItem
        { diSigName :: Name
        , diType :: NamedText
        , diHasDefault :: Bool
        }
    | SupertypeConstructorSignatureDocItem {diName :: FullNameRef}
    | ValuePatternDocItem
        { diNames :: NonEmpty FullNameRef
        , diType :: NamedText
        }
    | SpecialFormDocItem
        { diNames :: NonEmpty FullNameRef
        , diAnnotations :: [NamedText]
        , diType :: NamedText
        }
    | TypeDocItem
        { diNames :: NonEmpty FullNameRef
        , diStorable :: Bool
        , diParams :: [DocTypeParameter]
        , diGDS :: Maybe ([NamedText], NamedText)
        , diEquivalentDefn :: Maybe NamedText
        }
    | SubtypeRelationDocItem
        { diSubtype :: NamedText
        , diSupertype :: NamedText
        }
    deriving stock Eq

instance Show DocItem where
    show (HeadingDocItem t) = "heading " <> show t
    show (ValueDocItem n t) = "val " <> show n <> ": " <> unpack (toText t)
    show (ValueSignatureDocItem n t hd) = "sig " <> show n <> ": " <> unpack (toText t) <> mif hd " [optional]"
    show (SupertypeConstructorSignatureDocItem n) = "constructor " <> show n
    show (ValuePatternDocItem n t) = "val+pat " <> show n <> ": " <> unpack (toText t)
    show (SpecialFormDocItem n pp t) =
        "spform " <> show n <> concatmap (\p -> " " <> unpack (toText p)) pp <> ": " <> unpack (toText t)
    show (TypeDocItem n st pp mgds msyn) =
        "type "
            <> mif st "storable "
            <> show n
            <> concatmap (\p -> " " <> show p) pp
            <> (maybe mempty (\(vars, gds) -> " (" <> unpack (toText vars) <> " from " <> unpack (toText gds) <> ")") mgds)
            <> (maybe mempty (\syn -> " = " <> unpack (toText syn)) msyn)
    show (SubtypeRelationDocItem a b) = "subtype " <> unpack (toText a) <> " <: " <> unpack (toText b)

diNamesTraversal :: Applicative m => (NonEmpty FullNameRef -> m (NonEmpty FullNameRef)) -> DocItem -> m DocItem
diNamesTraversal f ValueDocItem{..} = fmap (\n -> ValueDocItem{diNames = n, ..}) $ f diNames
diNamesTraversal f ValuePatternDocItem{..} = fmap (\n -> ValuePatternDocItem{diNames = n, ..}) $ f diNames
diNamesTraversal f SpecialFormDocItem{..} = fmap (\n -> SpecialFormDocItem{diNames = n, ..}) $ f diNames
diNamesTraversal f TypeDocItem{..} = fmap (\n -> TypeDocItem{diNames = n, ..}) $ f diNames
diNamesTraversal _ di = pure di

diNameTraversal :: Applicative m => (FullNameRef -> m FullNameRef) -> DocItem -> m DocItem
diNameTraversal = diNamesTraversal . traverse

diMatchNameOrSubtypeRel :: FullName -> DocItem -> Bool
diMatchNameOrSubtypeRel n di =
    getAll
        $ execWriter
        $ diNameTraversal (\name -> (tell $ All $ n == namespaceConcatFullName RootNamespace name) >> return name) di

data DefDoc = MkDefDoc
    { docItem :: DocItem
    , docDescription :: RawMarkdown
    }

instance Show DefDoc where
    show (MkDefDoc i d) = show (i, d)

instance NamespaceConcat DocItem where
    namespaceConcat nsn di = runIdentity $ diNameTraversal (Identity . namespaceConcat nsn) di

instance NamespaceConcat DefDoc where
    namespaceConcat nsn MkDefDoc{..} = MkDefDoc{docItem = namespaceConcat nsn docItem, ..}
