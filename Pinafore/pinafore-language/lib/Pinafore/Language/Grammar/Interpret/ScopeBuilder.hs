module Pinafore.Language.Grammar.Interpret.ScopeBuilder
    ( ScopeBuilder
    , runScopeBuilder
    , Docs
    , exposeDocs
    , sourcePosScopeBuilder
    , interpScopeBuilder
    , refScopeBuilder
    , pureScopeBuilder
    , allocateVarScopeBuilder
    ) where

import Pinafore.Language.DefDoc
import Pinafore.Language.DocTree
import Pinafore.Language.Grammar.Interpret.RefNotation
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Language.VarID
import Shapes

popFilterTree :: (a -> Bool) -> Tree a -> [Tree a]
popFilterTree test (Node a tt) = let
    tt' = popFilterForest test tt
    in if test a
           then [Node a tt']
           else tt'

popFilterForest :: (a -> Bool) -> [Tree a] -> [Tree a]
popFilterForest test tt = mconcat $ fmap (popFilterTree test) tt

popFilterDTTEntry :: (a -> Bool) -> DocTreeEntry (Tree a) -> [DocTreeEntry (Tree a)]
popFilterDTTEntry test (EntryDocTreeEntry ta) = fmap EntryDocTreeEntry $ popFilterTree test ta
popFilterDTTEntry test (TreeDocTreeEntry dtt) = pure $ TreeDocTreeEntry $ popFilterDTT test dtt

popFilterDTTEntries :: (a -> Bool) -> [DocTreeEntry (Tree a)] -> [DocTreeEntry (Tree a)]
popFilterDTTEntries test ee = mconcat $ fmap (popFilterDTTEntry test) ee

popFilterDTT :: (a -> Bool) -> DocTree (Tree a) -> DocTree (Tree a)
popFilterDTT test dt = dt {docTreeEntries = popFilterDTTEntries test $ docTreeEntries dt}

-- double tree!
type Docs = [DocTreeEntry (Tree DefDoc)]

exposeDefDoc :: [FullNameRef] -> DefDoc -> Bool
exposeDefDoc names dd = any (\name -> diMatchNameOrSubtypeRel name $ docItem dd) names

exposeDocs :: [FullNameRef] -> Docs -> Docs
exposeDocs names =
    if True
        then id
        else popFilterDTTEntries $ exposeDefDoc names

type ScopeBuilder = TransformT RefNotation

runScopeBuilder :: ScopeBuilder a -> (a -> RefNotation b) -> RefNotation b
runScopeBuilder sb = unTransformT sb

sourcePosScopeBuilder :: SourcePos -> ScopeBuilder ()
sourcePosScopeBuilder spos = interpScopeBuilder $ refPut (transformParamRef sourcePosParam) spos

interpScopeBuilder :: QScopeInterpreter --> ScopeBuilder
interpScopeBuilder = liftTransformT . liftTransformT

refScopeBuilder :: RefNotation (ScopeBuilder a) -> ScopeBuilder a
refScopeBuilder = execMapTransformT

pureScopeBuilder :: QScope -> ScopeBuilder ()
pureScopeBuilder scope = interpScopeBuilder $ registerScope scope

allocateVarScopeBuilder :: Maybe FullNameRef -> ScopeBuilder (FullName, VarID)
allocateVarScopeBuilder n = interpScopeBuilder $ allocateVar n
