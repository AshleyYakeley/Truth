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
    , initialPos
    , DefDoc(..)
    , DocTree
    , runDocTree
    , predefinedDoc
    , Entity
    , showPinaforeValue
    ) where

import Control.Exception
import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.Expression
import Pinafore.Language.Literal
import Pinafore.Language.Predefined
import Pinafore.Language.Read
import Pinafore.Language.Read.Parser
import Pinafore.Language.Type
import Pinafore.Storage.File
import Shapes
import System.IO.Error

parseValue ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => SourcePos
    -> Text
    -> Result Text (QValue baseedit)
parseValue spos text = do
    texpr <- parseTopExpression @baseedit spos text
    rexpr <-
        runPinaforeTypeCheck $ do
            expr <- texpr
            runSourcePos spos $ qValuesLetExpr (\n -> lookup n predefinedBindings) expr
    qEvalExpr rexpr

parseValueAtType ::
       forall baseedit t. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit, FromPinaforeType baseedit t)
    => SourcePos
    -> Text
    -> Result Text t
parseValueAtType spos text = do
    val <- parseValue @baseedit spos text
    typedAnyToPinaforeVal @baseedit spos val

showPinaforeGroundValue ::
       PinaforeGroundType baseedit 'PositivePolarity dv t
    -> DolanArguments dv (PinaforeType baseedit) t 'PositivePolarity ta
    -> ta
    -> String
showPinaforeGroundValue (SimpleEntityPinaforeGroundType (LiteralSimpleEntityType t)) NilDolanArguments v =
    case literalTypeAsLiteral t of
        Dict -> unpack $ unLiteral $ toLiteral v
showPinaforeGroundValue PairPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) (a, b) =
    "(" <> showPinaforeValue ta a <> ", " <> showPinaforeValue tb b <> ")"
showPinaforeGroundValue EitherPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments _tb NilDolanArguments)) (Left x) =
    "Left " <> showPinaforeValue ta x
showPinaforeGroundValue EitherPinaforeGroundType (ConsDolanArguments _ta (ConsDolanArguments tb NilDolanArguments)) (Right x) =
    "Right " <> showPinaforeValue tb x
showPinaforeGroundValue ListPinaforeGroundType (ConsDolanArguments t NilDolanArguments) v =
    "[" <> intercalate "," (fmap (showPinaforeValue t) v) <> "]"
showPinaforeGroundValue _ _ _ = "<?>"

showPinaforeSingularValue :: PinaforeSingularType baseedit 'PositivePolarity t -> t -> String
showPinaforeSingularValue (VarPinaforeSingularType _) _ = "<?>"
showPinaforeSingularValue (GroundPinaforeSingularType gt args) v = showPinaforeGroundValue gt args v

showPinaforeValue :: PinaforeType baseedit 'PositivePolarity t -> t -> String
showPinaforeValue NilPinaforeType v = never v
showPinaforeValue (ConsPinaforeType ts tt) v = joinf (showPinaforeSingularValue ts) (showPinaforeValue tt) v

type Interact baseedit = StateT (SourcePos, QExpr baseedit -> Result Text (QExpr baseedit)) IO

interactEvalExpression ::
       (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => PinaforeTypeCheck (QExpr baseedit)
    -> Interact baseedit (QValue baseedit)
interactEvalExpression texpr = do
    (spos, bind) <- get
    let
        rval = do
            expr <- runPinaforeTypeCheck texpr
            expr' <- bind expr
            expr'' <-
                runPinaforeTypeCheck $
                runSourcePos spos $ qValuesLetExpr (\name -> lookup name predefinedBindings) expr'
            qEvalExpr expr''
    case rval of
        SuccessResult val -> return val
        FailureResult err -> fail $ unpack err

runValue :: SourcePos -> QValue baseedit -> PinaforeActionM baseedit ()
runValue spos val =
    case typedAnyToPinaforeVal spos val of
        SuccessResult action -> action
        _ ->
            case typedAnyToPinaforeVal spos val of
                SuccessResult v -> outputln v
                _ ->
                    case val of
                        MkAnyValue t v -> liftIO $ putStrLn $ showPinaforeValue t v

interactParse ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Text
    -> Interact baseedit (InteractiveCommand baseedit)
interactParse t = do
    (oldpos, bind) <- get
    (newpos, p) <- resultTextToM $ parseInteractiveCommand @baseedit oldpos t
    put (newpos, bind)
    return p

interactLoop ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => UnliftIO (PinaforeActionM baseedit)
    -> Interact baseedit ()
interactLoop runAction = do
    liftIO $ putStr "pinafore> "
    eof <- liftIO isEOF
    if eof
        then return ()
        else do
            str <- liftIO getLine
            liftIOWithUnlift $ \unlift ->
                catch
                    (runUnliftIO unlift $ do
                         p <- interactParse $ pack str
                         case p of
                             LetInteractiveCommand bind ->
                                 modify $ \(spos, oldbind) ->
                                     ( spos
                                     , \expr -> do
                                           expr' <- runPinaforeTypeCheck $ bind $ return expr
                                           oldbind expr')
                             ExpressionInteractiveCommand texpr -> do
                                 val <- interactEvalExpression texpr
                                 (spos, _) <- get
                                 lift $ runUnliftIO runAction $ runValue spos val
                             ShowTypeInteractiveCommand texpr -> do
                                 MkAnyValue t _ <- interactEvalExpression texpr
                                 lift $ putStrLn $ ":: " <> show t
                             ErrorInteractiveCommand err -> liftIO $ putStrLn $ unpack err) $ \err ->
                    putStrLn $ "error: " <> ioeGetErrorString err
            interactLoop runAction

interact ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => UnliftIO (PinaforeActionM baseedit)
    -> IO ()
interact runAction = do
    hSetBuffering stdout NoBuffering
    evalStateT (interactLoop runAction) (initialPos "<input>", return)
