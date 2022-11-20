module Pinafore.Language.Grammar.Interpret.Interact
    ( runInteract
    , showPinaforeModel
    ) where

import Changes.Core
import Control.Exception (Handler(..), catches)
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Interpret.Expression
import Pinafore.Language.Grammar.Read
import Pinafore.Language.Interpreter
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes
import System.IO.Error

showPinaforeModel :: QValue -> QInterpreter String
showPinaforeModel val = catch (fmap show $ qUnifyValue @Showable val) (\(_ :: PinaforeError) -> return "<?>")

type Interact = StateT SourcePos (ReaderStateT QInterpreter View)

interactRunQInterpreter :: QInterpreter a -> Interact a
interactRunQInterpreter sa = do
    spos <- get
    lift $ readerStateLift $ paramWith sourcePosParam spos $ sa

interactEvalExpression :: QInterpreter QExpression -> Interact QValue
interactEvalExpression texpr =
    interactRunQInterpreter $ do
        expr <- texpr
        qEvalExpr expr

runValue :: Handle -> QValue -> Interact (Action ())
runValue outh val =
    interactRunQInterpreter $
    (qUnifyValue val) <|> (fmap (\(text :: Text) -> liftIO $ hPutStrLn outh $ unpack text) $ qUnifyValue val) <|>
    (do
         s <- showPinaforeModel val
         return $ liftIO $ hPutStrLn outh s)

interactParse :: Text -> Interact InteractiveCommand
interactParse t = hoist fromInterpretResult $ parseInteractiveCommand t

interactLoop :: Handle -> Handle -> Bool -> Interact ()
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
                             NullInteractiveCommand -> return ()
                             LetInteractiveCommand stdecls -> do
                                 let
                                     bind :: QInterpreter --> QInterpreter
                                     bind = interpretTopDeclarations stdecls
                                 interactRunQInterpreter $ bind $ return () -- check errors
                                 lift $ readerStateUpdate bind
                             ExpressionInteractiveCommand sexpr -> do
                                 val <- interactEvalExpression $ interpretTopExpression sexpr
                                 action <- runValue outh val
                                 lift $ lift $ runAction action
                             BindActionInteractiveCommand {} ->
                                 interactRunQInterpreter $ throw $ KnownIssueError 178 "NYI"
                             ShowDocInteractiveCommand rname -> do
                                 bmap <- interactRunQInterpreter $ getBindingMap
                                 liftIO $
                                     case fmap biDocumentation $ bmap rname of
                                         Nothing -> hPutStrLn outh $ "! " <> show rname <> " not found"
                                         Just "" -> return ()
                                         Just doc -> hPutStrLn outh $ "#| " <> unpack (getRawMarkdown doc)
                             ShowTypeInteractiveCommand showinfo sexpr -> do
                                 MkSomeOf (MkPosShimWit t shim) _ <-
                                     interactEvalExpression $ interpretTopExpression sexpr
                                 liftIO $
                                     hPutStrLn outh $
                                     ": " <>
                                     unpack (exprShow t) <>
                                     if showinfo
                                         then " # " <> show shim
                                         else ""
                             SimplifyTypeInteractiveCommand polarity stype -> do
                                 s :: Text <-
                                     case polarity of
                                         Positive -> do
                                             MkSome t <- interactRunQInterpreter $ interpretType @'Positive stype
                                             t' <-
                                                 interactRunQInterpreter $
                                                 runRenamer @QTypeSystem [] [] $ simplify @QTypeSystem $ MkSome t
                                             return $ exprShow t'
                                         Negative -> do
                                             MkSome t <- interactRunQInterpreter $ interpretType @'Negative stype
                                             t' <-
                                                 interactRunQInterpreter $
                                                 runRenamer @QTypeSystem [] [] $ simplify @QTypeSystem $ MkSome t
                                             return $ exprShow t'
                                 liftIO $ hPutStrLn outh $ unpack s
                             ErrorInteractiveCommand err -> liftIO $ hPutStrLn outh $ unpack err)
                    [ Handler $ \(err :: PinaforeError) -> hPutStrLn outh $ show err
                    , Handler $ \err -> hPutStrLn outh $ "! error: " <> ioeGetErrorString err
                    ]
            interactLoop inh outh echo

runInteract :: Handle -> Handle -> Bool -> (QInterpreter --> View) -> View ()
runInteract inh outh echo runqi =
    evalReaderStateT (evalStateT (interactLoop inh outh echo) (initialPos "<input>")) runqi
