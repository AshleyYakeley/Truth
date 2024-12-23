module Pinafore.Language.Interpreter.Scope
    ( QBindingMap
    , bindingMapNamespaceWith
    , bindingMapLookupInfo
    , bindingMapEntries
    , bindingInfoToMap
    , bindingInfosToMap
    , bindingInfosToScope
    , QScope(..)
    , emptyScope
    , joinScopes
    , joinAllScopes
    , QModule(..)
    ) where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpreter.Binding
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Subtype ()

newtype QBindingMap =
    MkQBindingMap (Map FullName QBindingInfo)

instance Semigroup QBindingMap where
    MkQBindingMap nsa <> MkQBindingMap nsb = let
        joinBindings _ bb = bb
        in MkQBindingMap $ unionWith joinBindings nsa nsb

instance Monoid QBindingMap where
    mempty = MkQBindingMap mempty

instance HasInterpreter => Show QBindingMap where
    show (MkQBindingMap m) =
        "{" <> intercalate "," (fmap (\(n, b) -> show n <> "=" <> show (biValue b)) $ mapToList m) <> "}"

bindingMapNamespaceWith :: Namespace -> Namespace -> (FullNameRef -> Bool) -> QBindingMap -> QBindingMap
bindingMapNamespaceWith sourcens destns ff (MkQBindingMap nm) = let
    matchNS :: forall a. (FullName, a) -> Maybe (FullName, a)
    matchNS (fn, a) = do
        fnr <- namespaceWithinFullNameRef sourcens fn
        guard $ ff fnr
        return (namespaceConcatFullName destns fnr, a)
    newEntries = mapMaybe matchNS $ mapToList nm
    in MkQBindingMap $ mapFromList newEntries

bindingMapLookupInfo :: QBindingMap -> FullName -> Maybe (FullName, QBindingInfo)
bindingMapLookupInfo (MkQBindingMap nspace) name = do
    bi <- lookup name nspace
    return (name, bi)

bindingMapEntries :: QBindingMap -> [(FullName, QBindingInfo)]
bindingMapEntries (MkQBindingMap nspace) = mapToList nspace

bindingInfoToMap :: (FullName, QBindingInfo) -> QBindingMap
bindingInfoToMap (fname, bi) = MkQBindingMap $ singletonMap fname bi

bindingInfosToMap :: [(FullName, QBindingInfo)] -> QBindingMap
bindingInfosToMap bis = MkQBindingMap $ mapFromList bis

bindingInfosToScope :: [(FullName, QBindingInfo)] -> QScope
bindingInfosToScope bis = emptyScope {scopeBindings = bindingInfosToMap bis}

data QScope = MkQScope
    { scopeBindings :: QBindingMap
    , scopeSubtypes :: HashMap Unique QSubtypeConversionEntry
    }

emptyScope :: QScope
emptyScope = MkQScope mempty mempty

checkEntryConsistency ::
       HasInterpreter => QSubtypeConversionEntry -> HashMap Unique QSubtypeConversionEntry -> Interpreter ()
checkEntryConsistency sce entries =
    case checkSubtypeConsistency (toList entries) sce of
        Nothing -> return ()
        Just (gta, gtb) -> throw $ InterpretSubtypeInconsistent (exprShow gta) (exprShow gtb)

addSCEntry ::
       HasInterpreter
    => (Unique, QSubtypeConversionEntry)
    -> HashMap Unique QSubtypeConversionEntry
    -> Interpreter (HashMap Unique QSubtypeConversionEntry)
addSCEntry (key, _) entries
    | member key entries = return entries
addSCEntry (key, entry) entries = do
    checkEntryConsistency entry entries
    return $ insertMap key entry entries

addSCEntries ::
       HasInterpreter
    => [(Unique, QSubtypeConversionEntry)]
    -> HashMap Unique QSubtypeConversionEntry
    -> Interpreter (HashMap Unique QSubtypeConversionEntry)
addSCEntries [] entries = return entries
addSCEntries (a:aa) entries = do
    entries' <- addSCEntry a entries
    addSCEntries aa entries'

-- old, new
-- should be associative
joinScopes :: HasInterpreter => QScope -> QScope -> Interpreter QScope
joinScopes old new = do
    let
        bb = scopeBindings old <> scopeBindings new
        newlist = mapToList $ scopeSubtypes new
    st <- addSCEntries newlist $ scopeSubtypes old
    return MkQScope {scopeBindings = bb, scopeSubtypes = st}

-- old, new (oldest first)
joinAllScopesTo :: HasInterpreter => QScope -> [QScope] -> Interpreter QScope
joinAllScopesTo b [] = return b
joinAllScopesTo b (s:ss) = do
    s' <- joinScopes b s
    joinAllScopesTo s' ss

-- oldest first
joinAllScopes :: HasInterpreter => [QScope] -> Interpreter QScope
joinAllScopes [] = return emptyScope
joinAllScopes (s:ss) = joinAllScopesTo s ss

data QModule = MkQModule
    { moduleDoc :: Forest DefDoc
    , moduleScope :: QScope
    }
