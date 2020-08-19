module Pinafore.Language.DocTree where

import Shapes

data DocTree a =
    MkDocTree Text
              Text
              [DocTreeEntry a]

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
    foldMap am (MkDocTree _ _ entries) = mconcat $ fmap (foldMap am) entries

docTreeEntry :: Text -> Text -> [DocTreeEntry a] -> DocTreeEntry a
docTreeEntry title desc entries = TreeDocTreeEntry $ MkDocTree title desc entries

runDocTree ::
       Monad m => (Int -> Text -> m ()) -> (Int -> Text -> m ()) -> (Int -> a -> m ()) -> Int -> DocTree a -> m ()
runDocTree showTitle showDesc showEntry level (MkDocTree title desc entries) = do
    showTitle level title
    showDesc level desc
    for_ entries $ \case
        TreeDocTreeEntry tree -> runDocTree showTitle showDesc showEntry (level + 1) tree
        EntryDocTreeEntry a -> showEntry level a

data DefDoc = MkDefDoc
    { docName :: Text
    , docValueType :: Text
    , docIsSupertype :: Bool
    , docIsPattern :: Bool
    , docDescription :: Text
    }
