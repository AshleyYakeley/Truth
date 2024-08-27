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

data ScopeEntry
    = BindScopeEntry FullNameRef
                     [FullName]
                     QInterpreterBinding
    | SubtypeScopeEntry QSubtypeConversionEntry

scopeEntryName :: ScopeEntry -> Maybe FullNameRef
scopeEntryName (BindScopeEntry name _ _) = Just name
scopeEntryName (SubtypeScopeEntry _) = Nothing

data BindDoc = MkBindDoc
    { bdScopeEntry :: Maybe ScopeEntry
    , bdDoc :: DefDoc
    }

bindDocNames :: BindDoc -> Maybe FullNameRef
bindDocNames bd = mapMaybe scopeEntryName $ bdScopeEntry bd

type LibraryStuff = Forest BindDoc

singleBindDoc :: BindDoc -> [LibraryStuff] -> LibraryStuff
singleBindDoc bd tt = pureForest $ MkTree bd $ mconcat tt

libraryContentsEntries :: LibraryStuff -> [BindDoc]
libraryContentsEntries = toList

libraryContentsDocumentation :: LibraryStuff -> Forest DefDoc
libraryContentsDocumentation = fmap bdDoc

data LibraryModule = MkLibraryModule
    { lmName :: ModuleName
    , lmContents :: LibraryStuff
    }
