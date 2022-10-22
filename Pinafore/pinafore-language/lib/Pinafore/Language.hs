module Pinafore.Language
    ( Name
    , ModuleName(..)
    , toModuleName
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
    , showPinaforeModel
    , runPinaforeScoped
    , exprShow
    ) where

import Changes.Core
import Control.Exception (Handler(..), catches)
import Pinafore.Base
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

runPinaforeScoped :: (?library :: LibraryContext) => String -> QInterpreter a -> InterpretResult a
runPinaforeScoped sourcename scp =
    runInterpreter (initialPos sourcename) (lcLoadModule ?library) spvals $
    transformTMap (void $ interpretImportDeclaration stdModuleName) scp

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

showPinaforeModel :: QValue -> QInterpreter String
showPinaforeModel val = catch (fmap show $ qUnifyValue @Showable val) (\(_ :: PinaforeError) -> return "<?>")

type Interact = StateT SourcePos (ReaderStateT QInterpreter View)

interactRunSourceScoped :: QInterpreter a -> Interact a
interactRunSourceScoped sa = do
    spos <- get
    lift $ readerStateLift $ paramWith sourcePosParam spos $ sa

interactEvalExpression :: QInterpreter QExpression -> Interact QValue
interactEvalExpression texpr =
    interactRunSourceScoped $ do
        expr <- texpr
        qEvalExpr expr

runValue :: Handle -> QValue -> Interact (Action ())
runValue outh val =
    interactRunSourceScoped $
    (qUnifyValue val) <|> (fmap (\(text :: Text) -> liftIO $ hPutStrLn outh $ unpack text) $ qUnifyValue val) <|>
    (do
         s <- showPinaforeModel val
         return $ liftIO $ hPutStrLn outh s)

interactParse :: Text -> Interact InteractiveCommand
interactParse t = hoist fromInterpretResult $ parseInteractiveCommand t

interactLoop :: (?library :: LibraryContext) => Handle -> Handle -> Bool -> Interact ()
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
                             LetInteractiveCommand bind -> do
                                 interactRunSourceScoped $ bind $ return () -- check errors
                                 lift $ readerStateUpdate bind
                             ExpressionInteractiveCommand texpr -> do
                                 val <- interactEvalExpression texpr
                                 action <- runValue outh val
                                 lift $ lift $ runAction action
                             ShowDocInteractiveCommand rname -> do
                                 md <- interactRunSourceScoped $ lookupDocBinding rname
                                 liftIO $
                                     case md of
                                         Nothing -> hPutStrLn outh $ "! " <> show rname <> " not found"
                                         Just ("", _) -> return ()
                                         Just (doc, _) -> hPutStrLn outh $ "#| " <> unpack (getRawMarkdown doc)
                             ShowTypeInteractiveCommand showinfo texpr -> do
                                 MkSomeOf (MkPosShimWit t shim) _ <- interactEvalExpression texpr
                                 liftIO $
                                     hPutStrLn outh $
                                     ": " <>
                                     unpack (exprShow t) <>
                                     if showinfo
                                         then " # " <> show shim
                                         else ""
                             SimplifyTypeInteractiveCommand polarity ttype -> do
                                 MkSome t <- interactRunSourceScoped ttype
                                 s :: Text <-
                                     case polarity of
                                         PositiveType -> do
                                             t' <-
                                                 interactRunSourceScoped $
                                                 runRenamer @QTypeSystem [] $ simplify @QTypeSystem $ MkSome t
                                             return $ exprShow t'
                                         NegativeType -> do
                                             t' <-
                                                 interactRunSourceScoped $
                                                 runRenamer @QTypeSystem [] $ simplify @QTypeSystem $ MkSome t
                                             return $ exprShow t'
                                 liftIO $ hPutStrLn outh $ unpack s
                             ErrorInteractiveCommand err -> liftIO $ hPutStrLn outh $ unpack err)
                    [ Handler $ \(err :: PinaforeError) -> hPutStrLn outh $ show err
                    , Handler $ \err -> hPutStrLn outh $ "! error: " <> ioeGetErrorString err
                    ]
            interactLoop inh outh echo

interact :: (?library :: LibraryContext) => Handle -> Handle -> Bool -> View ()
interact inh outh echo = do
    liftIO $ hSetBuffering outh NoBuffering
    evalReaderStateT (evalStateT (interactLoop inh outh echo) (initialPos "<input>")) $
        fromInterpretResult . runPinaforeScoped "<UNKNOWN>"
