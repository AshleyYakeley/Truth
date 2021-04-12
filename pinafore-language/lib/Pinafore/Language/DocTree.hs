module Pinafore.Language.DocTree where

import Shapes

data DocTree a = MkDocTree
    { docTreeName :: Text
    , docTreeDescription :: Text
    , docTreeEntries :: [DocTreeEntry a]
    }

data DocTreeEntry a
    = TreeDocTreeEntry (DocTree a)
    | EntryDocTreeEntry a

instance Functor DocTreeEntry where
    fmap ab (TreeDocTreeEntry tree) = TreeDocTreeEntry $ fmap ab tree
    fmap ab (EntryDocTreeEntry a) = EntryDocTreeEntry $ ab a

instance Functor DocTree where
    fmap ab (MkDocTree title desc entries) = MkDocTree title desc $ fmap (fmap ab) entries

instance Foldable DocTreeEntry where
    foldMap am (TreeDocTreeEntry tree) = foldMap am tree
    foldMap am (EntryDocTreeEntry a) = am a

instance Foldable DocTree where
    foldMap am dt = mconcat $ fmap (foldMap am) $ docTreeEntries dt

docTreeEntry :: Text -> Text -> [DocTreeEntry a] -> DocTreeEntry a
docTreeEntry title desc entries = TreeDocTreeEntry $ MkDocTree title desc entries

runDocTree ::
       Monad m => (Int -> Text -> m ()) -> (Int -> Text -> m ()) -> (Int -> a -> m ()) -> Int -> DocTree a -> m ()
runDocTree showTitle showDesc showEntry level MkDocTree {..} = do
    showTitle level docTreeName
    showDesc level docTreeDescription
    for_ docTreeEntries $ \case
        TreeDocTreeEntry tree -> runDocTree showTitle showDesc showEntry (succ level) tree
        EntryDocTreeEntry a -> showEntry level a

data DocType
    = ValueDocType
    | ValuePatternDocType
    | TypeDocType
    | SupertypeDocType
    | SubtypeRelationDocType

data DefDoc = MkDefDoc
    { docName :: Text
    , docValueType :: Text
    , docType :: DocType
    , docDescription :: Text
    }
