module Pinafore.Language.Library
    ( DefDoc(..)
    , LibraryStuff
    , LibraryModule(..)
    , builtInLibrary
    , LoadModule
    , directoryLoadModule
    , textLoadModule
    , libraryLoadModule
    , mkLibraryContext
    , nameIsInfix
    ) where

import Import
import Pinafore.Context
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Action
import Pinafore.Language.Library.Base
import Pinafore.Language.Library.Debug
import Pinafore.Language.Library.Env
import Pinafore.Language.Library.Eval
import Pinafore.Language.Library.Interpret
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Lifecycle
import Pinafore.Language.Library.LoadModule
import Pinafore.Language.Library.MIME
import Pinafore.Language.Library.Model
import Pinafore.Language.Library.ModelOrder
import Pinafore.Language.Library.Optics
import Pinafore.Language.Library.Result
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
    [ resultLibSection
    , actionLibSection
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

builtInLoadModule :: InvocationInfo -> LoadModule
builtInLoadModule ii = libraryLoadModule ii builtInLibrary

mkLibraryContext :: InvocationInfo -> LoadModule -> LibraryContext
mkLibraryContext ii lm = let
    lcLoadModule = builtInLoadModule ii <> lm
    in MkLibraryContext {..}
