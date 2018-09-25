module Pinafore.Language
    ( QType(..)
    , QActionM
    , QAction
    , QValue
    , HasQTypeDescription(..)
    , ToQValue(..)
    , resultTextToM
    , parseValue
    , interact
    , DefDoc(..)
    , DocTree
    , runDocTree
    , predefinedDoc
    ) where

import Control.Exception
import Pinafore.File
import Pinafore.Language.Convert
import Pinafore.Language.Expression
import Pinafore.Language.Predefined
import Pinafore.Language.Read
import Pinafore.Language.Value
import Pinafore.PredicateMorphism
import Pinafore.Types
import Shapes
import System.IO.Error

parseValue ::
       forall baseedit t. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit, FromQValue baseedit t)
    => String
    -> Text
    -> Result Text t
parseValue name text = do
    texpr <- parseExpression @baseedit name text
    rexpr <-
        runQTypeCheck $ do
            expr <- texpr
            qUncheckedBindingsLetExpr predefinedBindings expr
    val <- qEvalExpr rexpr
    fromQValue val

interactLoop ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => StateT (QExpr baseedit -> IO ()) IO ()
interactLoop = do
    liftIO $ putStr "pinafore> "
    eof <- liftIO isEOF
    if eof
        then return ()
        else do
            str <- liftIO getLine
            liftIOWithUnlift $ \unlift ->
                catch
                    (runUnliftIO unlift $ do
                         p <- resultTextToM $ parseInteractiveCommand @baseedit "<input>" $ pack str
                         case p of
                             LetInteractiveCommand bind ->
                                 modify $ \eval expr -> do
                                     case runQTypeCheck $ bind expr of
                                         SuccessResult expr' -> eval expr'
                                         FailureResult err -> putStrLn $ unpack err
                             ExpressionInteractiveCommand texpr -> do
                                 eval <- get
                                 liftIO $
                                     case runQTypeCheck texpr of
                                         SuccessResult expr -> eval expr
                                         FailureResult err -> putStrLn $ unpack err) $ \err ->
                    putStrLn $ ioeGetErrorString err
            interactLoop

interact ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => UnliftIO (QActionM baseedit)
    -> IO ()
interact runAction = do
    hSetBuffering stdout NoBuffering
    evalStateT interactLoop $ \expr ->
        runUnliftIO runAction $ do
            case runQTypeCheck $ qUncheckedBindingsLetExpr predefinedBindings expr of
                SuccessResult expr' -> do
                    val <- qLiftResult $ qEvalExpr expr'
                    case fromQValue val of
                        SuccessResult action -> action
                        _ ->
                            case fromQValue $ qapply (toQValue outputln) val of
                                SuccessResult action -> action
                                _ -> liftIO $ putStrLn $ show val
                FailureResult err -> liftIO $ putStrLn $ unpack err
