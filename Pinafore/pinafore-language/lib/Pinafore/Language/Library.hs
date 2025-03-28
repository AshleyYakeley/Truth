module Pinafore.Language.Library
    ( DefDoc (..)
    , LibraryStuff
    , LibraryModule (..)
    , pinaforeLibrary
    , LoadModule
    , directoryLoadModule
    , textLoadModule
    , libraryLoadModule
    , mkLibraryContext
    , nameIsInfix
    )
where

import Import
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Action
import Pinafore.Language.Library.Comparison
import Pinafore.Language.Library.Debug
import Pinafore.Language.Library.Entity
import Pinafore.Language.Library.EntityMap
import Pinafore.Language.Library.Env
import Pinafore.Language.Library.Function
import Pinafore.Language.Library.Interpret
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Lifecycle
import Pinafore.Language.Library.List
import Pinafore.Language.Library.LoadModule
import Pinafore.Language.Library.Maybe
import Pinafore.Language.Library.Model
import Pinafore.Language.Library.ModelOrder
import Pinafore.Language.Library.Optics
import Pinafore.Language.Library.Pinafore
import Pinafore.Language.Library.Product
import Pinafore.Language.Library.Result
import Pinafore.Language.Library.Storage
import Pinafore.Language.Library.Stream
import Pinafore.Language.Library.Sum
import Pinafore.Language.Library.Task
import Pinafore.Language.Library.Undo

pinaforeLibrary :: [LibraryModule]
pinaforeLibrary =
    pure
        $ MkLibraryModule builtInModuleName
        $ mconcat
        $ [ entityLibSection
          , functionLibSection
          , comparisonLibSection
          , maybeLibSection
          , productLibSection
          , sumLibSection
          , listLibSection
          , mapLibSection
          , resultLibSection
          , actionLibSection
          , lifecycleLibSection
          , interpretLibSection
          , modelLibSection
          , opticsLibSection
          , modelOrderLibSection
          , taskLibSection
          , streamLibSection
          , storageLibSection
          , undoLibSection
          , envLibSection
          , pinaforeLibSection
          , debugLibSection
          ]

mkLibraryContext :: LoadModule -> LibraryContext
mkLibraryContext lm = let
    lcLoadModule = lm
    in MkLibraryContext{..}
