module Pinafore.Language.Library.LibraryModule
    ( ScopeEntry(..)
    , BindDoc(..)
    , bindDocNames
    , LibraryStuff
    , singleBindDoc
    , libraryContentsEntries
    , libraryContentsDocumentation
    , LibraryModule(..)
    ) where

import Import
import Pinafore.Language.Interpreter
import Pinafore.Language.Type

data ScopeEntry context
    = BindScopeEntry FullNameRef
                     [FullName]
                     (context -> QInterpreterBinding)
    | SubtypeScopeEntry QSubtypeConversionEntry

instance Contravariant ScopeEntry where
    contramap ab (BindScopeEntry name oname f) = BindScopeEntry name oname $ \c -> f $ ab c
    contramap _ (SubtypeScopeEntry entry) = SubtypeScopeEntry entry

scopeEntryName :: ScopeEntry context -> Maybe FullNameRef
scopeEntryName (BindScopeEntry name _ _) = Just name
scopeEntryName (SubtypeScopeEntry _) = Nothing

data BindDoc context = MkBindDoc
    { bdScopeEntry :: Maybe (ScopeEntry context)
    , bdDoc :: DefDoc
    }

bindDocNames :: BindDoc context -> Maybe FullNameRef
bindDocNames bd = mapMaybe scopeEntryName $ bdScopeEntry bd

instance Contravariant BindDoc where
    contramap ab (MkBindDoc se d) = MkBindDoc (fmap (contramap ab) $ se) d

type LibraryStuff context = Forest (BindDoc context)

singleBindDoc :: BindDoc context -> [LibraryStuff context] -> LibraryStuff context
singleBindDoc bd tt = pureForest $ MkTree bd $ mconcat tt

libraryContentsEntries :: LibraryStuff context -> [BindDoc context]
libraryContentsEntries = toList

libraryContentsDocumentation :: LibraryStuff context -> Forest DefDoc
libraryContentsDocumentation = fmap bdDoc

data LibraryModule context = MkLibraryModule
    { lmName :: ModuleName
    , lmContents :: LibraryStuff context
    }

instance Contravariant LibraryModule where
    contramap ab (MkLibraryModule n c) = MkLibraryModule n $ fmap (contramap ab) c
