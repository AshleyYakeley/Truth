module Pinafore.Language.Grammar.Docs
    ( Docs
    , exposeDocs
    ) where

import Pinafore.Language.DefDoc
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes

type instance EntryDoc QTypeSystem = DefDoc

popFilterTree :: (a -> Bool) -> Tree a -> Forest a
popFilterTree test (MkTree a tt) = let
    tt' = popFilterForest test tt
    in if test a
           then pureForest $ MkTree a tt'
           else tt'

popFilterForest :: (a -> Bool) -> Forest a -> Forest a
popFilterForest test tt = bindForest tt $ popFilterTree test

type Docs = Forest DefDoc

exposeDefDoc :: [FullName] -> DefDoc -> Bool
exposeDefDoc names dd = any (\name -> diMatchNameOrSubtypeRel name $ docItem dd) names

exposeDocs :: [FullName] -> Docs -> Docs
exposeDocs names = popFilterForest $ exposeDefDoc names
