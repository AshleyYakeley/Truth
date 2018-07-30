module Pinafore.Language
    ( QType(..)
    , QActionM
    , QAction
    , QValue
    , HasQTypeDescription(..)
    , ToQValue(..)
    , QBindings
    , resultTextToM
    , parseValue
    , interact
    , DefDoc(..)
    , DocTree
    , runDocTree
    , predefinedDoc
    ) where

import Control.Exception
import Language.Expression.Unitype
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
    expr <- parseExpression @baseedit name text
    val <- evalExpression $ uncheckedBindingsLetExpression predefinedBindings expr
    fromQValue val

interactLoop ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => StateT (QValueExpr baseedit -> IO ()) IO ()
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
                             Left bind -> modify $ \eval -> eval . bind
                             Right expr -> do
                                 eval <- get
                                 liftIO $ eval expr) $ \err -> putStrLn $ ioeGetErrorString err
            interactLoop

interact ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => UnliftIO (QActionM baseedit)
    -> IO ()
interact runAction = do
    hSetBuffering stdout NoBuffering
    evalStateT interactLoop $ \expr ->
        runUnliftIO runAction $ do
            val <- qLiftResult $ evalExpression $ uncheckedBindingsLetExpression predefinedBindings expr
            case fromQValue val of
                SuccessResult action -> action
                _ ->
                    case fromQValue $ qapply (toQValue outputln) val of
                        SuccessResult action -> action
                        _ -> liftIO $ putStrLn $ show val
