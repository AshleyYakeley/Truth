module Pinafore.Query
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
    , predefinedDoc
    ) where

import Control.Exception
import Pinafore.File
import Pinafore.Query.Convert
import Pinafore.Query.Expression
import Pinafore.Query.Predefined
import Pinafore.Query.Read
import Pinafore.Query.Types
import Pinafore.Query.Value
import Pinafore.Table
import Shapes
import System.IO.Error

parseValue ::
       forall baseedit t. (HasPinaforeTableEdit baseedit, HasPinaforeFileEdit baseedit, FromQValue baseedit t)
    => String
    -> Text
    -> Result Text t
parseValue name text = do
    expr <- parseExpression @baseedit name text
    val <- qeval $ qlets predefinedBindings expr
    fromQValue val

interactLoop ::
       forall baseedit. HasPinaforeTableEdit baseedit
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
       forall baseedit. (HasPinaforeTableEdit baseedit, HasPinaforeFileEdit baseedit)
    => UnliftIO (QActionM baseedit)
    -> IO ()
interact runAction = do
    hSetBuffering stdout NoBuffering
    evalStateT interactLoop $ \expr ->
        runUnliftIO runAction $ do
            val <- liftInner $ qeval $ qlets predefinedBindings expr
            case fromQValue val of
                SuccessResult action -> action
                _ ->
                    case fromQValue $ qapply (toQValue outputln) val of
                        SuccessResult action -> action
                        _ -> liftIO $ putStrLn $ show val
