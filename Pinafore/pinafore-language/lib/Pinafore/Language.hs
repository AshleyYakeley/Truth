module Pinafore.Language
    ( Name
    , ModuleName(..)
    , toModuleName
    , LibraryModule
    , FetchModule
    , directoryFetchModule
    , textFetchModule
    , libraryFetchModule
    , PinaforeModule
    , Module(..)
    , LibraryContext(..)
    , mkLibraryContext
    , PinaforeSpecialVals
    , SpecialVals(..)
    , PinaforeError
    , InterpretResult
    , throwInterpretResult
    , runInterpretResult
    , PinaforeAction
    , HasPinaforeType
    , parseTopExpression
    , parseValue
    , parseValueUnify
    , parseValueSubsume
    , interact
    , TopType(..)
    , Var
    , A
    , B
    , C
    , X
    , Y
    , Entity
    , showPinaforeRef
    , runPinaforeScoped
    , runPinaforeSourceScoped
    , exprShow
    ) where

import Changes.Core
import Control.Exception (Handler(..), catches)
import Pinafore.Base
import Pinafore.Context
import Pinafore.Language.Convert
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Expression
import Pinafore.Language.Grammar
import Pinafore.Language.Interpreter
import Pinafore.Language.Library
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Language.Var
import Pinafore.Markdown
import Shapes
import System.IO.Error

runPinaforeScoped ::
       (?pinafore :: PinaforeContext, ?library :: LibraryContext) => PinaforeInterpreter a -> InterpretResult a
runPinaforeScoped scp = runInterpreter (lcLoadModule ?library) spvals $ importScope (lcImplictScope ?library) scp

spvals :: (?pinafore :: PinaforeContext, ?library :: LibraryContext) => PinaforeSpecialVals
spvals = let
    specialEvaluate :: forall t. PinaforeType 'Positive t -> Text -> PinaforeAction (Either Text t)
    specialEvaluate t text = do
        ier <- liftIO $ evaluate $ runPinaforeSourceScoped "<evaluate>" $ parseValueSubsume t text
        result <- runInterpretResult ier
        return $
            case result of
                SuccessResult r -> Right r
                FailureResult err -> Left $ pack $ show err
    in MkSpecialVals {..}

runPinaforeSourceScoped ::
       (?pinafore :: PinaforeContext, ?library :: LibraryContext)
    => FilePath
    -> PinaforeSourceInterpreter a
    -> InterpretResult a
runPinaforeSourceScoped fpath scp = runPinaforeScoped $ runSourcePos (initialPos fpath) scp

parseValue :: (?pinafore :: PinaforeContext) => Text -> PinaforeSourceInterpreter QValue
parseValue text = do
    rexpr <- parseTopExpression text
    qEvalExpr rexpr

parseValueUnify ::
       forall t. (HasPinaforeType 'Negative t, ?pinafore :: PinaforeContext)
    => Text
    -> PinaforeSourceInterpreter t
parseValueUnify text = do
    val <- parseValue text
    typedAnyToPinaforeVal val

parseValueSubsume ::
       forall t. (?pinafore :: PinaforeContext)
    => PinaforeType 'Positive t
    -> Text
    -> PinaforeSourceInterpreter t
parseValueSubsume t text = do
    val <- parseValue text
    tsSubsumeValue @PinaforeTypeSystem t val

showPinaforeRef :: QValue -> PinaforeSourceInterpreter String
showPinaforeRef val = catch (fmap show $ typedAnyToPinaforeVal @Showable val) (\(_ :: PinaforeError) -> return "<?>")

type Interact = StateT SourcePos (ReaderStateT PinaforeInterpreter View)

interactRunSourceScoped :: PinaforeSourceInterpreter a -> Interact a
interactRunSourceScoped sa = do
    spos <- get
    lift $ liftRS $ runSourcePos spos sa

interactEvalExpression :: PinaforeInterpreter QExpr -> Interact QValue
interactEvalExpression texpr =
    interactRunSourceScoped $ do
        expr <- liftSourcePos texpr
        qEvalExpr expr

runValue :: Handle -> QValue -> Interact (PinaforeAction ())
runValue outh val =
    interactRunSourceScoped $
    (typedAnyToPinaforeVal val) <|>
    (fmap (\(text :: Text) -> liftIO $ hPutStrLn outh $ unpack text) $ typedAnyToPinaforeVal val) <|>
    (do
         s <- showPinaforeRef val
         return $ liftIO $ hPutStrLn outh s)

interactParse :: Text -> Interact InteractiveCommand
interactParse t = remonad throwInterpretResult $ parseInteractiveCommand t

interactLoop :: (?pinafore :: PinaforeContext, ?library :: LibraryContext) => Handle -> Handle -> Bool -> Interact ()
interactLoop inh outh echo = do
    liftIO $ hPutStr outh "pinafore> "
    eof <- liftIO $ hIsEOF inh
    if eof
        then return ()
        else do
            str <- liftIO $ hGetLine inh
            let inputstr = str <> "\n"
            if echo
                then liftIO $ hPutStr outh inputstr
                else return ()
            liftIOWithUnlift $ \unlift -> do
                catches
                    (unlift $ do
                         p <- interactParse $ pack inputstr
                         case p of
                             LetInteractiveCommand bind ->
                                 lift $ do
                                     liftRS $ bind $ return () -- check errors
                                     updateRS bind
                             ExpressionInteractiveCommand texpr -> do
                                 val <- interactEvalExpression texpr
                                 action <- runValue outh val
                                 lift $ lift $ runPinaforeAction action
                             ShowDocInteractiveCommand rname -> do
                                 md <- interactRunSourceScoped $ lookupDocBinding rname
                                 liftIO $
                                     case md of
                                         Nothing -> hPutStrLn outh $ "! " <> show rname <> " not found"
                                         Just ("", _) -> return ()
                                         Just (doc, _) -> hPutStrLn outh $ "#| " <> unpack (getRawMarkdown doc)
                             ShowTypeInteractiveCommand showinfo texpr -> do
                                 MkAnyValue (MkPosShimWit t shim) _ <- interactEvalExpression texpr
                                 liftIO $
                                     hPutStrLn outh $
                                     ": " <>
                                     unpack (exprShow t) <>
                                     if showinfo
                                         then " # " <> show shim
                                         else ""
                             SimplifyTypeInteractiveCommand polarity ttype -> do
                                 MkAnyW t <- lift $ liftRS ttype
                                 s :: Text <-
                                     case polarity of
                                         PositiveType -> do
                                             t' <-
                                                 interactRunSourceScoped $
                                                 runRenamer @PinaforeTypeSystem $
                                                 simplify @PinaforeTypeSystem $ MkAnyInKind t
                                             return $ exprShow t'
                                         NegativeType -> do
                                             t' <-
                                                 interactRunSourceScoped $
                                                 runRenamer @PinaforeTypeSystem $
                                                 simplify @PinaforeTypeSystem $ MkAnyInKind t
                                             return $ exprShow t'
                                 liftIO $ hPutStrLn outh $ unpack s
                             ErrorInteractiveCommand err -> liftIO $ hPutStrLn outh $ unpack err)
                    [ Handler $ \(err :: PinaforeError) -> hPutStrLn outh $ show err
                    , Handler $ \err -> hPutStrLn outh $ "! error: " <> ioeGetErrorString err
                    ]
            interactLoop inh outh echo

interact :: (?pinafore :: PinaforeContext, ?library :: LibraryContext) => Handle -> Handle -> Bool -> View ()
interact inh outh echo = do
    liftIO $ hSetBuffering outh NoBuffering
    evalReaderStateT (evalStateT (interactLoop inh outh echo) (initialPos "<input>")) $
        throwInterpretResult . runPinaforeScoped
