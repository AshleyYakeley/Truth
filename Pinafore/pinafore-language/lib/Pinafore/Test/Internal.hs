module Pinafore.Test.Internal
    ( module Pinafore.Syntax
    , module Pinafore.Language
    , module Pinafore.Language.Expression
    , module Pinafore.API
    , module Pinafore.Test
    , parseType
    , runInterpreter
    , QTypeSystem
    , VarID
    , mkVarID
    , szero
    , UVar
    , Var(..)
    , QGroundType(..)
    , StorableGroundType(..)
    , QValue
    , QOpenExpression
    , QExpression
    , QSingularShimWit
    , QBindingInfo(..)
    , SomeGroundType(..)
    , QInterpreterBinding(..)
    , QInterpreter
    , toJMShimWit
    , allocateVar
    , QScopeBuilder
    , withScopeBuilder
    , registerGroundType
    , registerLetBindings
    , registerLetBinding
    , registerPatternConstructor
    , QSubtypeHint
    , QSubtypeConversionEntry
    , registerSubtypeConversion
    , SomeValue(..)
    , bindsLibrary
    , showPinaforeModel
    , qInterpretText
    , qInteractHandles
    ) where

import Import
import Pinafore.API
import Pinafore.Language
import Pinafore.Language.Expression
import Pinafore.Language.Grammar
import Pinafore.Language.Grammar.Interpret.Interact
import Pinafore.Language.Interpreter
import Pinafore.Language.Type
import Pinafore.Language.VarID
import Pinafore.Main
import Pinafore.Storage
import Pinafore.Syntax
import Pinafore.Test

data SomeValue =
    forall t. HasQType 'Positive t => MkSomeValue t

bindsLibrary :: ModuleName -> [(FullName, SomeValue)] -> LibraryModule ()
bindsLibrary mname binds =
    MkLibraryModule mname $ mconcat $ fmap (\(name, MkSomeValue val) -> valBDS (fullNameRef name) "" val) binds
