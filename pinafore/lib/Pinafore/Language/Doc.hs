module Pinafore.Language.Doc where

import Pinafore.Language.Convert
import Pinafore.Language.Expression
import Shapes

data DocTree a =
    MkDocTree Text
              [DocTreeEntry a]

data DocTreeEntry a
    = TreeDocTreeEntry (DocTree a)
    | EntryDocTreeEntry a

instance Functor DocTreeEntry where
    fmap ab (TreeDocTreeEntry tree) = TreeDocTreeEntry $ fmap ab tree
    fmap ab (EntryDocTreeEntry a) = EntryDocTreeEntry $ ab a

instance Functor DocTree where
    fmap ab (MkDocTree title entries) = MkDocTree title $ fmap (fmap ab) entries

instance Foldable DocTreeEntry where
    foldMap am (TreeDocTreeEntry tree) = foldMap am tree
    foldMap am (EntryDocTreeEntry a) = am a

instance Foldable DocTree where
    foldMap am (MkDocTree _ entries) = mconcat $ fmap (foldMap am) entries

docTreeEntry :: Text -> [DocTreeEntry a] -> DocTreeEntry a
docTreeEntry title entries = TreeDocTreeEntry $ MkDocTree title entries

runDocTree :: Monad m => (Int -> Text -> m ()) -> (Int -> a -> m ()) -> Int -> DocTree a -> m ()
runDocTree showTitle showEntry level (MkDocTree title entries) = do
    showTitle level title
    for_ entries $ \case
        TreeDocTreeEntry tree -> runDocTree showTitle showEntry (level + 1) tree
        EntryDocTreeEntry a -> showEntry level a

data DefDoc =
    MkDefDoc Symbol
             Text
             Text

mkDefDoc ::
       forall t. HasQTypeDescription t
    => Symbol
    -> Text
    -> t
    -> DefDoc
mkDefDoc name desc _ = MkDefDoc name (qTypeDescription @t) desc
