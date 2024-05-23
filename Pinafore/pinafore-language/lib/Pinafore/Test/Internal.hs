module Pinafore.Test.Internal
    ( module Pinafore
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
    , module Pinafore.Language.Expression
    , SomeValue(..)
    , bindsLibrary
    , showPinaforeModel
    ) where

import Import
import Pinafore
import Pinafore.API
import Pinafore.Language.Expression
import Pinafore.Language.Grammar
import Pinafore.Language.Grammar.Interpret.Interact
import Pinafore.Language.Interpreter
import Pinafore.Language.Type
import Pinafore.Language.VarID
import Pinafore.Test

data SomeValue =
    forall t. HasQType 'Positive t => MkSomeValue t

bindsLibrary :: ModuleName -> [(FullName, SomeValue)] -> LibraryModule ()
bindsLibrary mname binds =
    MkLibraryModule mname $ mconcat $ fmap (\(name, MkSomeValue val) -> valBDS (fullNameRef name) "" val) binds
