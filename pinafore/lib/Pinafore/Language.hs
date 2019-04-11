module Pinafore.Language
    ( PinaforePredefinitions
    , PinaforeAction
    , qTypeDescription
    , FromPinaforeType
    , ToPinaforeType
    , parseTopExpression
    , parseValue
    , parseValueAtType
    , interact
    , Entity
    , showPinaforeValue
    , runPinaforeSourceScoped
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
import Shapes
import System.IO.Error

runPinaforeScoped ::
       (PinaforePredefinitions baseedit, ?pinafore :: PinaforeContext baseedit)
    => PinaforeScoped baseedit a
    -> Result Text a
runPinaforeScoped scp =
    runScoped $
    withNewPatternConstructors predefinedPatternConstructors $ withNewBindings (qValuesLetExpr predefinedBindings) scp

runPinaforeSourceScoped ::
       (PinaforePredefinitions baseedit, ?pinafore :: PinaforeContext baseedit)
    => FilePath
    -> PinaforeSourceScoped baseedit a
    -> Result Text a
runPinaforeSourceScoped fpath scp = runPinaforeScoped $ runSourcePos (initialPos fpath) scp

parseValue ::
       forall baseedit. (PinaforePredefinitions baseedit, ?pinafore :: PinaforeContext baseedit)
    => Text
    -> PinaforeSourceScoped baseedit (QValue baseedit)
parseValue text = do
    rexpr <- parseTopExpression @baseedit text
    qEvalExpr rexpr

parseValueAtType ::
       forall baseedit t.
       (PinaforePredefinitions baseedit, FromPinaforeType baseedit t, ?pinafore :: PinaforeContext baseedit)
    => Text
    -> PinaforeSourceScoped baseedit t
parseValueAtType text = do
    val <- parseValue @baseedit text
    typedAnyToPinaforeVal @baseedit val

showEntityGroundValue ::
       CovaryType dv
    -> EntityGroundType f
    -> DolanArguments dv (PinaforeType baseedit) f 'Positive t
    -> t
    -> Maybe String
showEntityGroundValue NilListType (LiteralEntityGroundType t) NilDolanArguments v =
    case literalTypeAsLiteral t of
        Dict -> Just $ unpack $ unLiteral $ toLiteral v
showEntityGroundValue (ConsListType Refl NilListType) MaybeEntityGroundType (ConsDolanArguments t NilDolanArguments) (Just x) =
    Just $ "Just " <> showPinaforeValue t x
showEntityGroundValue (ConsListType Refl NilListType) MaybeEntityGroundType (ConsDolanArguments _t NilDolanArguments) Nothing =
    Just "Nothing"
showEntityGroundValue (ConsListType Refl NilListType) ListEntityGroundType (ConsDolanArguments t NilDolanArguments) v =
    Just $ "[" <> intercalate ", " (fmap (showPinaforeValue t) v) <> "]"
showEntityGroundValue (ConsListType Refl (ConsListType Refl NilListType)) PairEntityGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) (a, b) =
    Just $ "(" <> showPinaforeValue ta a <> ", " <> showPinaforeValue tb b <> ")"
showEntityGroundValue (ConsListType Refl (ConsListType Refl NilListType)) EitherEntityGroundType (ConsDolanArguments ta (ConsDolanArguments _tb NilDolanArguments)) (Left x) =
    Just $ "Left " <> showPinaforeValue ta x
showEntityGroundValue (ConsListType Refl (ConsListType Refl NilListType)) EitherEntityGroundType (ConsDolanArguments _ta (ConsDolanArguments tb NilDolanArguments)) (Right x) =
    Just $ "Right " <> showPinaforeValue tb x
showEntityGroundValue _ _ _ _ = Nothing

showPinaforeGroundValue ::
       PinaforeGroundType baseedit 'Positive dv t
    -> DolanArguments dv (PinaforeType baseedit) t 'Positive ta
    -> ta
    -> String
showPinaforeGroundValue (EntityPinaforeGroundType ct t) args v
    | Just str <- showEntityGroundValue ct t args v = str
showPinaforeGroundValue _ _ _ = "<?>"

showPinaforeSingularValue :: PinaforeSingularType baseedit 'Positive t -> t -> String
showPinaforeSingularValue (VarPinaforeSingularType _) _ = "<?>"
showPinaforeSingularValue (GroundPinaforeSingularType gt args) v = showPinaforeGroundValue gt args v

showPinaforeValue :: PinaforeType baseedit 'Positive t -> t -> String
showPinaforeValue NilPinaforeType v = never v
showPinaforeValue (ConsPinaforeType ts tt) v = joinf (showPinaforeSingularValue ts) (showPinaforeValue tt) v

type Interact baseedit = StateT SourcePos (ReaderStateT (PinaforeScoped baseedit) IO)

interactRunSourceScoped :: PinaforeSourceScoped baseedit a -> Interact baseedit a
interactRunSourceScoped sa = do
    spos <- get
    lift $ liftRS $ runSourcePos spos sa

interactEvalExpression ::
       forall baseedit. (PinaforePredefinitions baseedit)
    => PinaforeScoped baseedit (QExpr baseedit)
    -> Interact baseedit (QValue baseedit)
interactEvalExpression texpr =
    interactRunSourceScoped $ do
        expr <- liftSourcePos texpr
        qEvalExpr expr

runValue :: Handle -> QValue baseedit -> Interact baseedit (PinaforeAction baseedit ())
runValue outh val =
    interactRunSourceScoped $
    (typedAnyToPinaforeVal val) <|> (fmap outputln $ typedAnyToPinaforeVal val) <|>
    (case val of
         MkAnyValue t v -> return $ liftIO $ hPutStrLn outh $ showPinaforeValue t v)

interactParse ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Text
    -> Interact baseedit (InteractiveCommand baseedit)
interactParse t = remonad resultTextToM $ parseInteractiveCommand @baseedit t

interactLoop ::
       forall baseedit. (PinaforePredefinitions baseedit, ?pinafore :: PinaforeContext baseedit)
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
                         p <- interactParse $ pack inputstr
                         case p of
                             LetInteractiveCommand fbind ->
                                 lift $ do
                                     MkTransform bind <- liftRS fbind
                                     updateRS bind
                             ExpressionInteractiveCommand texpr -> do
                                 val <- interactEvalExpression texpr
                                 action <- runValue outh val
                                 lift $ lift $ runPinaforeAction action
                             ShowTypeInteractiveCommand texpr -> do
                                 MkAnyValue t _ <- interactEvalExpression texpr
                                 liftIO $ hPutStrLn outh $ ":: " <> show t
                             ErrorInteractiveCommand err -> liftIO $ hPutStrLn outh $ unpack err) $ \err ->
                    hPutStrLn outh $ "error: " <> ioeGetErrorString err
            interactLoop inh outh echo

interact ::
       forall baseedit. (PinaforePredefinitions baseedit, ?pinafore :: PinaforeContext baseedit)
    => Handle
    -> Handle
    -> Bool
    -> IO ()
interact inh outh echo = do
    hSetBuffering outh NoBuffering
    evalReaderStateT (evalStateT (interactLoop inh outh echo) (initialPos "<input>")) $
        resultTextToM . runPinaforeScoped
