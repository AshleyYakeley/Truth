module Pinafore.Language
    ( LibraryStuff
    , LibraryModule(..)
    , LoadModule
    , directoryLoadModule
    , textLoadModule
    , libraryLoadModule
    , QModule(..)
    , getModule
    , LibraryContext(..)
    , mkLibraryContext
    , pinaforeLibrary
    , QSpecialVals(..)
    , QError
    , fromParseResult
    , InterpretResult
    , fromInterpretResult
    , runInterpretResult
    , Action
    , HasQType
    , parseTopExpression
    , parseToValue
    , parseToValueUnify
    , parseToValueSubsume
    , interact
    , initialPos
    , TopType(..)
    , Var
    , A
    , B
    , C
    , X
    , Y
    , Entity
    , runPinaforeScoped
    ) where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpret
import Pinafore.Language.Interpreter
import Pinafore.Language.Library
import Pinafore.Language.Type
import Pinafore.Language.Var

runPinaforeScoped :: (?library :: LibraryContext) => String -> QInterpreter a -> InterpretResult a
runPinaforeScoped sourcename ma =
    runInterpreter (initialPos sourcename) ?library spvals $ do
        sd <- interpretImportDeclaration builtInModuleName
        withScopeDocs sd ma

spvals :: (?library :: LibraryContext) => QSpecialVals
spvals = let
    specialEvaluate :: forall t. QType 'Positive t -> Text -> IO (Result QError t)
    specialEvaluate t text = do
        ier <- evaluate $ runPinaforeScoped "<evaluate>" $ parseToValueSubsume t text
        runInterpretResult ier
    in MkQSpecialVals {..}

parseToValue :: Text -> [(ImplicitName, QValue)] -> QInterpreter QValue
parseToValue text args = do
    expr <- parseTopExpression text
    let argExprs = fmap (fmap qConstValue) args
    expr' <- qImply argExprs expr
    qEvalExpr expr'

parseToValueUnify ::
       forall t. (HasQType QPolyShim 'Negative t)
    => Text
    -> [(ImplicitName, QValue)]
    -> QInterpreter t
parseToValueUnify text args = do
    val <- parseToValue text args
    qUnifyValue val

parseToValueSubsume :: forall t. QType 'Positive t -> Text -> QInterpreter t
parseToValueSubsume t text = do
    val <- parseToValue text []
    tsSubsumeValue @QTypeSystem t val

interact :: (?library :: LibraryContext) => Handle -> Handle -> Bool -> View ()
interact inh outh echo = do
    liftIO $ hSetBuffering outh NoBuffering
    runInteract inh outh echo $ fromInterpretResult . runPinaforeScoped "<UNKNOWN>"
