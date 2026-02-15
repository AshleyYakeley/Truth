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
    , mkLambdaVarID
    , szero
    , UVar
    , QGroundType (..)
    , StorableGroundType (..)
    , QValue
    , QOpenExpression
    , QExpression
    , QSingularShimWit
    , QScopeItem (..)
    , SomeGroundType (..)
    , QItem (..)
    , QInterpreter
    , QRenameTypeM
    , QTypeM
    , toJMShimWit
    , allocateLambdaVar
    , QScopeBuilder
    , withScopeBuilder
    , registerGroundType
    , registerLetBindings
    , registerLetBinding
    , registerPatternConstructor
    , QSubtypeHint
    , QSubtypeConversionEntry
    , registerSubtypeConversion
    , SomeValue (..)
    , showExpressionType
    , parseExpressionToType
    , bindsLibrary
    , toTextQValue
    , qInterpretScriptText
    , qInteractHandles
    )
where

import Pinafore.Storage
import Pinafore.Syntax

import Import
import Pinafore.API
import Pinafore.Language
import Pinafore.Language.Expression
import Pinafore.Language.Interpret
import Pinafore.Language.Interpreter
import Pinafore.Language.Type
import Pinafore.Language.VarID
import Pinafore.Main
import Pinafore.Test

showVars :: QOpenExpression t -> [Text]
showVars = freeWitnesses $ \(MkNameWitness name (MkShimWit t _)) -> toText $ exprShow name <> " : " <> exprShow t

showExpressionType :: QExpression -> Text
showExpressionType (MkSealedExpression (MkShimWit t _) expr) =
    "{" <> intercalate ", " (nub $ showVars expr) <> "} -> " <> toText (exprShow t)

parseExpressionToType :: Text -> (Text -> IO ()) -> IO ()
parseExpressionToType text checkType =
    runTester defaultTester
        $ testerLiftInterpreter
        $ do
            expr <- parseTopExpression text
            expr' <- runRenamer @QTypeSystem [] [] $ unEndoM (finalRenameMappable @QTypeSystem) expr
            liftIO $ checkType $ showExpressionType expr'

data SomeValue
    = forall t. HasQType QPolyShim 'Positive t => MkSomeValue t

bindsLibrary :: ModuleName -> [(FullName, SomeValue)] -> LibraryModule
bindsLibrary mname binds =
    MkLibraryModule mname $ concatmap (\(name, MkSomeValue val) -> valBDS (fullNameRef name) "" val) binds

toTextQValue :: QValue -> QInterpreter Text
toTextQValue val = catch (fmap toText $ qUnifyValue @ToSource val) (\(_ :: QLocatedError) -> return "<?>")
