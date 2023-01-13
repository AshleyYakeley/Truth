module Pinafore.Language.Library
    ( DefDoc(..)
    , LibraryModule
    , FetchModule
    , directoryFetchModule
    , textFetchModule
    , libraryFetchModule
    , LibraryContext(..)
    , mkLibraryContext
    , nameIsInfix
    , allOperatorNames
    ) where

import Pinafore.Context
import Pinafore.Language.DefDoc
import Pinafore.Language.Library.Action
import Pinafore.Language.Library.Base
import Pinafore.Language.Library.Debug
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Env
import Pinafore.Language.Library.Eval
import Pinafore.Language.Library.FetchModule
import Pinafore.Language.Library.Lifecycle
import Pinafore.Language.Library.Model
import Pinafore.Language.Library.ModelOrder
import Pinafore.Language.Library.Optics
import Pinafore.Language.Library.Storage
import Pinafore.Language.Library.Stream
import Pinafore.Language.Library.Task
import Pinafore.Language.Library.Undo
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes

library :: [LibraryModule InvocationInfo]
library =
    pure $
    MkLibraryModule builtInModuleName $
    headingBDT "Built-In" "" $
    baseLibSections <>
    [ actionLibSection
    , lifecycleLibSection
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

allOperatorNames :: (DocItem -> Bool) -> [Name]
allOperatorNames test = let
    getDocName :: forall context. BindDoc context -> Maybe Name
    getDocName MkBindDoc {bdScopeEntries = (BindScopeEntry (MkFullNameRef name _) _):_, bdDoc = dd}
        | test $ docItem dd
        , nameIsInfix name = Just name
    getDocName _ = Nothing
    in mapMaybe getDocName $ mconcat $ fmap libraryModuleEntries library

data LibraryContext = MkLibraryContext
    { lcLoadModule :: ModuleName -> QInterpreter (Maybe QModule)
    }

mkLibraryContext :: InvocationInfo -> FetchModule InvocationInfo -> LibraryContext
mkLibraryContext context fetchModule =
    MkLibraryContext $ runFetchModule (libraryFetchModule library <> fetchModule) context
