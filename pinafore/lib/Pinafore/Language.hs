module Pinafore.Language
    ( PinaforeActionM
    , PinaforeAction
    , HasQTypeDescription
    , qTypeDescription
    , ToPinaforeType
    , resultTextToM
    , parseValue
    , parseValueAtType
    , interact
    , DefDoc(..)
    , DocTree
    , runDocTree
    , predefinedDoc
    , Entity
    , showPinaforeValue
    ) where

import Control.Exception
import Pinafore.Action
import Pinafore.File
import Pinafore.Language.Convert
import Pinafore.Language.Entity
import Pinafore.Language.Expression
import Pinafore.Language.Literal
import Pinafore.Language.Predefined
import Pinafore.Language.Read
import Pinafore.Language.Type
import Pinafore.Literal
import Pinafore.PredicateMorphism
import Shapes
import System.IO.Error

parseValue ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => String
    -> Text
    -> Result Text (Any (PinaforeType baseedit 'PositivePolarity))
parseValue name text = do
    texpr <- parseExpression @baseedit name text
    rexpr <-
        runPinaforeTypeCheck $ do
            expr <- texpr
            qUncheckedBindingsLetExpr predefinedBindings expr
    qEvalExpr rexpr

parseValueAtType ::
       forall baseedit t. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit, FromPinaforeType baseedit t)
    => String
    -> Text
    -> Result Text t
parseValueAtType name text = do
    val <- parseValue @baseedit name text
    typedAnyToPinaforeVal @baseedit val

showPinaforeGroundValue ::
       PinaforeGroundType baseedit 'PositivePolarity dv t
    -> DolanArguments dv (PinaforeType baseedit) t 'PositivePolarity ta
    -> ta
    -> String
showPinaforeGroundValue (LiteralPinaforeGroundType t) NilDolanArguments v =
    case literalTypeAsLiteral t of
        Dict -> show $ toLiteral v
showPinaforeGroundValue PairPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) (a, b) =
    "(" <> showPinaforeValue ta a <> ", " <> showPinaforeValue tb b <> ")"
showPinaforeGroundValue ListPinaforeGroundType (ConsDolanArguments t NilDolanArguments) v =
    "[" <> intercalate "," (fmap (showPinaforeValue t) v) <> "]"
showPinaforeGroundValue _ _ _ = "<?>"

showPinaforeSingularValue :: PinaforeSingularType baseedit 'PositivePolarity t -> t -> String
showPinaforeSingularValue (VarPinaforeSingularType _) _ = "<?>"
showPinaforeSingularValue (GroundPinaforeSingularType gt args) v = showPinaforeGroundValue gt args v

showPinaforeValue :: PinaforeType baseedit 'PositivePolarity t -> t -> String
showPinaforeValue NilPinaforeType v = never v
showPinaforeValue (ConsPinaforeType ts tt) v = joinf (showPinaforeSingularValue ts) (showPinaforeValue tt) v

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
                                     case runPinaforeTypeCheck $ bind expr of
                                         SuccessResult expr' -> eval expr'
                                         FailureResult err -> putStrLn $ unpack err
                             ExpressionInteractiveCommand texpr -> do
                                 eval <- get
                                 liftIO $
                                     case runPinaforeTypeCheck texpr of
                                         SuccessResult expr -> eval expr
                                         FailureResult err -> putStrLn $ unpack err) $ \err ->
                    putStrLn $ ioeGetErrorString err
            interactLoop

interact ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => UnliftIO (PinaforeActionM baseedit)
    -> IO ()
interact runAction = do
    hSetBuffering stdout NoBuffering
    evalStateT interactLoop $ \expr ->
        runUnliftIO runAction $ do
            case runPinaforeTypeCheck $ qUncheckedBindingsLetExpr predefinedBindings expr of
                SuccessResult expr' -> do
                    val <- pinaforeLiftResult $ qEvalExpr expr'
                    case typedAnyToPinaforeVal val of
                        SuccessResult action -> action
                        _ ->
                            case typedAnyToPinaforeVal val of
                                SuccessResult v -> outputln v
                                _ ->
                                    case val of
                                        MkAny t v -> liftIO $ putStrLn $ showPinaforeValue t v
                FailureResult err -> liftIO $ putStrLn $ unpack err
