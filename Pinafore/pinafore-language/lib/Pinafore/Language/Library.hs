module Pinafore.Language.Library
    ( DefDoc(..)
    , LibraryStuff
    , LibraryModule
    , builtInLibrary
    , FetchModule
    , directoryFetchModule
    , textFetchModule
    , libraryFetchModule
    , mkLibraryContext
    , nameIsInfix
    ) where

import Import
import Pinafore.Context
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Action
import Pinafore.Language.Library.Base
import Pinafore.Language.Library.Debug
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Env
import Pinafore.Language.Library.Eval
import Pinafore.Language.Library.FetchModule
import Pinafore.Language.Library.Interpret
import Pinafore.Language.Library.Lifecycle
import Pinafore.Language.Library.MIME
import Pinafore.Language.Library.Model
import Pinafore.Language.Library.ModelOrder
import Pinafore.Language.Library.Optics
import Pinafore.Language.Library.Storage
import Pinafore.Language.Library.Stream
import Pinafore.Language.Library.Task
import Pinafore.Language.Library.Undo

builtInLibrary :: [LibraryModule InvocationInfo]
builtInLibrary =
    pure $
    MkLibraryModule builtInModuleName $
    mconcat $
    baseLibSections <>
    [ actionLibSection
    , lifecycleLibSection
    , interpretLibSection
    , mimeLibSection
    , modelLibSection
    , opticsLibSection
    , modelOrderLibSection
    , taskLibSection
    , streamLibSection
    , storageLibSection
    , undoLibSection
    , envLibSection
    , evalLibSection
    , debugLibSection
    ]

mkLibraryContext :: InvocationInfo -> FetchModule -> LibraryContext
mkLibraryContext context fetchModule = let
    lcLoadModule = runFetchModule $ libraryFetchModule context builtInLibrary <> fetchModule
    in MkLibraryContext {..}
