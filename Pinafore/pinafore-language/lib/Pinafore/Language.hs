module Pinafore.Language
    ( module Pinafore.Language.Name
    , LibraryModule
    , FetchModule
    , directoryFetchModule
    , textFetchModule
    , libraryFetchModule
    , QModule
    , Module(..)
    , LibraryContext(..)
    , mkLibraryContext
    , QSpecialVals
    , SpecialVals(..)
    , PinaforeError
    , InterpretResult
    , fromInterpretResult
    , runInterpretResult
    , Action
    , HasQType
    , parseTopExpression
    , parseValue
    , parseValueUnify
    , parseValueSubsume
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
    , exprShow
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Expression
import Pinafore.Language.Grammar
import Pinafore.Language.Grammar.Interpret
import Pinafore.Language.Interpreter
import Pinafore.Language.Library
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Language.Var
import Shapes

runPinaforeScoped :: (?library :: LibraryContext) => String -> QInterpreter a -> InterpretResult a
runPinaforeScoped sourcename scp =
    runInterpreter (initialPos sourcename) (lcLoadModule ?library) spvals $
    transformTMap (void $ interpretImportDeclaration builtInModuleName) scp

spvals :: (?library :: LibraryContext) => QSpecialVals
spvals = let
    specialEvaluate :: forall t. QType 'Positive t -> Text -> Action (Either Text t)
    specialEvaluate t text = do
        ier <- liftIO $ evaluate $ runPinaforeScoped "<evaluate>" $ parseValueSubsume t text
        result <- runInterpretResult ier
        return $
            case result of
                SuccessResult r -> Right r
                FailureResult err -> Left $ pack $ show err
    in MkSpecialVals {..}

parseValue :: Text -> QInterpreter QValue
parseValue text = do
    rexpr <- parseTopExpression text
    qEvalExpr rexpr

parseValueUnify ::
       forall t. (HasQType 'Negative t)
    => Text
    -> QInterpreter t
parseValueUnify text = do
    val <- parseValue text
    qUnifyValue val

parseValueSubsume :: forall t. QType 'Positive t -> Text -> QInterpreter t
parseValueSubsume t text = do
    val <- parseValue text
    tsSubsumeValue @QTypeSystem t val

interact :: (?library :: LibraryContext) => Handle -> Handle -> Bool -> View ()
interact inh outh echo = do
    liftIO $ hSetBuffering outh NoBuffering
    runInteract inh outh echo $ fromInterpretResult . runPinaforeScoped "<UNKNOWN>"
