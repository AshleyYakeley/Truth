module Pinafore.Language.Grammar.Interpret.ScopeBuilder
    ( ScopeBuilder
    , runScopeBuilder
    , docScopeBuilder
    , defDocScopeBuilder
    , refNotationScopeBuilder
    , interpScopeBuilder
    , refScopeBuilder
    , pureScopeBuilder
    ) where

import Pinafore.Language.DefDoc
import Pinafore.Language.DocTree
import Pinafore.Language.Grammar.Interpret.RefNotation
import Pinafore.Language.Interpreter
import Pinafore.Language.Type
import Shapes

newtype ScopeBuilder = MkScopeBuilder
    { runScopeBuilder :: forall a. RefNotation a -> RefNotation ([DocTreeEntry DefDoc], a)
    }

instance Semigroup ScopeBuilder where
    MkScopeBuilder f <> MkScopeBuilder g =
        MkScopeBuilder $ \ra -> fmap (\(olddoc, (newdoc, a)) -> (olddoc <> newdoc, a)) $ f $ g ra

instance Monoid ScopeBuilder where
    mempty =
        MkScopeBuilder $ \ra -> do
            a <- ra
            return ([], a)

docScopeBuilder :: [DocTreeEntry DefDoc] -> ScopeBuilder
docScopeBuilder docs =
    MkScopeBuilder $ \ra -> do
        a <- ra
        return (docs, a)

defDocScopeBuilder :: DefDoc -> ScopeBuilder
defDocScopeBuilder doc = docScopeBuilder [EntryDocTreeEntry doc]

refNotationScopeBuilder :: MFunction RefNotation RefNotation -> ScopeBuilder
refNotationScopeBuilder rr =
    MkScopeBuilder $ \ra -> do
        a <- rr ra
        return ([], a)

interpScopeBuilder :: MFunction PinaforeInterpreter PinaforeInterpreter -> ScopeBuilder
interpScopeBuilder mf = refNotationScopeBuilder $ remonadRefNotation $ MkWMFunction mf

refScopeBuilder :: RefNotation ScopeBuilder -> ScopeBuilder
refScopeBuilder rsb =
    MkScopeBuilder $ \ra -> do
        MkScopeBuilder rr <- rsb
        rr ra

pureScopeBuilder :: PinaforeScope -> ScopeBuilder
pureScopeBuilder scope = interpScopeBuilder (importScope scope)
