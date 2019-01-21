module Pinafore.Language
    ( PinaforeAction
    , qTypeDescription
    , ToPinaforeType
    , resultTextToM
    , parseExpression
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

runPinaforeScoped ::
       (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit, ?pinafore :: PinaforeContext baseedit)
    => PinaforeScoped baseedit a
    -> Result Text a
runPinaforeScoped scp =
    runScoped $
    withNewPatternConstructors predefinedPatternConstructors $ withNewBindings (qValuesLetExpr predefinedBindings) scp

parseExpression ::
       forall baseedit.
       (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit, ?pinafore :: PinaforeContext baseedit)
    => SourcePos
    -> Text
    -> Result Text (QExpr baseedit)
parseExpression spos text = runPinaforeScoped $ runSourcePos spos $ parseTopExpression @baseedit text

parseValue ::
       forall baseedit.
       (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit, ?pinafore :: PinaforeContext baseedit)
    => SourcePos
    -> Text
    -> Result Text (QValue baseedit)
parseValue spos text =
    runPinaforeScoped $
    runSourcePos spos $ do
        rexpr <- parseTopExpression @baseedit text
        qEvalExpr rexpr

parseValueAtType ::
       forall baseedit t.
       ( HasPinaforeEntityEdit baseedit
       , HasPinaforeFileEdit baseedit
       , FromPinaforeType baseedit t
       , ?pinafore :: PinaforeContext baseedit
       )
    => SourcePos
    -> Text
    -> Result Text t
parseValueAtType spos text = do
    val <- parseValue @baseedit spos text
    typedAnyToPinaforeVal @baseedit spos val

showPinaforeGroundValue ::
       PinaforeGroundType baseedit 'Positive dv t
    -> DolanArguments dv (PinaforeType baseedit) t 'Positive ta
    -> ta
    -> String
showPinaforeGroundValue (SimpleEntityPinaforeGroundType (LiteralSimpleEntityType t)) NilDolanArguments v =
    case literalTypeAsLiteral t of
        Dict -> unpack $ unLiteral $ toLiteral v
showPinaforeGroundValue MaybePinaforeGroundType (ConsDolanArguments t NilDolanArguments) (Just x) =
    "Just " <> showPinaforeValue t x
showPinaforeGroundValue MaybePinaforeGroundType (ConsDolanArguments _t NilDolanArguments) Nothing = "Nothing"
showPinaforeGroundValue PairPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) (a, b) =
    "(" <> showPinaforeValue ta a <> ", " <> showPinaforeValue tb b <> ")"
showPinaforeGroundValue EitherPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments _tb NilDolanArguments)) (Left x) =
    "Left " <> showPinaforeValue ta x
showPinaforeGroundValue EitherPinaforeGroundType (ConsDolanArguments _ta (ConsDolanArguments tb NilDolanArguments)) (Right x) =
    "Right " <> showPinaforeValue tb x
showPinaforeGroundValue ListPinaforeGroundType (ConsDolanArguments t NilDolanArguments) v =
    "[" <> intercalate ", " (fmap (showPinaforeValue t) v) <> "]"
showPinaforeGroundValue _ _ _ = "<?>"

showPinaforeSingularValue :: PinaforeSingularType baseedit 'Positive t -> t -> String
showPinaforeSingularValue (VarPinaforeSingularType _) _ = "<?>"
showPinaforeSingularValue (GroundPinaforeSingularType gt args) v = showPinaforeGroundValue gt args v

showPinaforeValue :: PinaforeType baseedit 'Positive t -> t -> String
showPinaforeValue NilPinaforeType v = never v
showPinaforeValue (ConsPinaforeType ts tt) v = joinf (showPinaforeSingularValue ts) (showPinaforeValue tt) v

type Interact baseedit = StateT SourcePos (ReaderStateT (PinaforeScoped baseedit) IO)

interactEvalExpression ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => SourcePos
    -> PinaforeScoped baseedit (QExpr baseedit)
    -> Interact baseedit (QValue baseedit)
interactEvalExpression spos texpr =
    lift $
    liftRS $ do
        expr <- texpr
        runSourcePos spos $ qEvalExpr expr

runValue :: Handle -> SourcePos -> QValue baseedit -> PinaforeAction baseedit ()
runValue outh spos val =
    case typedAnyToPinaforeVal spos val of
        SuccessResult action -> action
        _ ->
            case typedAnyToPinaforeVal spos val of
                SuccessResult v -> outputln v
                _ ->
                    case val of
                        MkAnyValue t v -> liftIO $ hPutStrLn outh $ showPinaforeValue t v

interactParse ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Text
    -> Interact baseedit (InteractiveCommand baseedit)
interactParse t = remonad resultTextToM $ parseInteractiveCommand @baseedit t

interactLoop ::
       forall baseedit.
       (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit, ?pinafore :: PinaforeContext baseedit)
    => Handle
    -> Handle
    -> Bool
    -> Interact baseedit ()
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
            liftIOWithUnlift $ \unlift ->
                catch
                    (runTransform unlift $ do
                         spos <- get
                         p <- interactParse $ pack inputstr
                         case p of
                             LetInteractiveCommand fbind ->
                                 lift $ do
                                     MkTransform bind <- liftRS fbind
                                     updateRS bind
                             ExpressionInteractiveCommand texpr -> do
                                 val <- interactEvalExpression spos texpr
                                 lift $ lift $ runPinaforeAction $ runValue outh spos val
                             ShowTypeInteractiveCommand texpr -> do
                                 MkAnyValue t _ <- interactEvalExpression spos texpr
                                 lift $ lift $ hPutStrLn outh $ ":: " <> show t
                             ErrorInteractiveCommand err -> liftIO $ hPutStrLn outh $ unpack err) $ \err ->
                    hPutStrLn outh $ "error: " <> ioeGetErrorString err
            interactLoop inh outh echo

interact ::
       forall baseedit.
       (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit, ?pinafore :: PinaforeContext baseedit)
    => Handle
    -> Handle
    -> Bool
    -> IO ()
interact inh outh echo = do
    hSetBuffering outh NoBuffering
    evalReaderStateT (evalStateT (interactLoop inh outh echo) (initialPos "<input>")) $
        resultTextToM . runPinaforeScoped
