module Pinafore.Language
    ( PinaforePredefinitions
    , PinaforeError
    , InterpretResult
    , ioRunInterpretResult
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
import Data.Shim
import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Predefined
import Pinafore.Language.Read
import Pinafore.Language.Read.Parser
import Pinafore.Language.Type.Literal
import Pinafore.Language.TypeSystem
import Pinafore.Language.TypeSystem.Simplify
import Shapes
import System.IO.Error

runPinaforeScoped ::
       (PinaforePredefinitions baseedit, ?pinafore :: PinaforeContext baseedit)
    => PinaforeScoped baseedit a
    -> InterpretResult a
runPinaforeScoped scp =
    runScoped $
    withNewPatternConstructors predefinedPatternConstructors $ withNewBindings (qValuesLetExpr predefinedBindings) scp

runPinaforeSourceScoped ::
       (PinaforePredefinitions baseedit, ?pinafore :: PinaforeContext baseedit)
    => FilePath
    -> PinaforeSourceScoped baseedit a
    -> InterpretResult a
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

entityTypedShowValue ::
       CovaryType dv
    -> EntityGroundType f
    -> DolanArguments dv (PinaforeType baseedit) f 'Positive t
    -> t
    -> Maybe String
entityTypedShowValue NilListType (LiteralEntityGroundType t) NilDolanArguments v =
    case literalTypeAsLiteral t of
        Dict -> Just $ unpack $ unLiteral $ toLiteral v
entityTypedShowValue (ConsListType Refl NilListType) MaybeEntityGroundType (ConsDolanArguments t NilDolanArguments) (Just x) =
    Just $ "Just " <> typedShowValue t x
entityTypedShowValue (ConsListType Refl NilListType) MaybeEntityGroundType (ConsDolanArguments _t NilDolanArguments) Nothing =
    Just "Nothing"
entityTypedShowValue (ConsListType Refl NilListType) ListEntityGroundType (ConsDolanArguments t NilDolanArguments) v =
    Just $ "[" <> intercalate ", " (fmap (typedShowValue t) v) <> "]"
entityTypedShowValue (ConsListType Refl (ConsListType Refl NilListType)) PairEntityGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) (a, b) =
    Just $ "(" <> typedShowValue ta a <> ", " <> typedShowValue tb b <> ")"
entityTypedShowValue (ConsListType Refl (ConsListType Refl NilListType)) EitherEntityGroundType (ConsDolanArguments ta (ConsDolanArguments _tb NilDolanArguments)) (Left x) =
    Just $ "Left " <> typedShowValue ta x
entityTypedShowValue (ConsListType Refl (ConsListType Refl NilListType)) EitherEntityGroundType (ConsDolanArguments _ta (ConsDolanArguments tb NilDolanArguments)) (Right x) =
    Just $ "Right " <> typedShowValue tb x
entityTypedShowValue _ _ _ _ = Nothing

groundTypedShowValue ::
       PinaforeGroundType baseedit 'Positive dv t
    -> DolanArguments dv (PinaforeType baseedit) t 'Positive ta
    -> ta
    -> String
groundTypedShowValue (EntityPinaforeGroundType ct t) args v
    | Just str <- entityTypedShowValue ct t args v = str
groundTypedShowValue _ _ _ = "<?>"

singularTypedShowValue :: PinaforeSingularType baseedit 'Positive t -> t -> String
singularTypedShowValue (VarPinaforeSingularType _) _ = "<?>"
singularTypedShowValue (GroundPinaforeSingularType gt args) v = groundTypedShowValue gt args v

typedShowValue :: PinaforeType baseedit 'Positive t -> t -> String
typedShowValue NilPinaforeType v = never v
typedShowValue (ConsPinaforeType ts tt) v = joinf (singularTypedShowValue ts) (typedShowValue tt) v

showPinaforeValue :: QValue baseedit -> String
showPinaforeValue (MkAnyValue (MkShimWit t conv) v) = typedShowValue t (fromEnhanced conv v)

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
    (return $ liftIO $ hPutStrLn outh $ showPinaforeValue val)

interactParse ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Text
    -> Interact baseedit (InteractiveCommand baseedit)
interactParse t = remonad ioRunInterpretResult $ parseInteractiveCommand @baseedit t

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
                catches
                    (runTransform unlift $ do
                         p <- interactParse $ pack inputstr
                         case p of
                             LetInteractiveCommand fbind ->
                                 lift $ do
                                     MkTransform bind <- liftRS fbind
                                     liftRS $ bind $ return () -- check errors
                                     updateRS bind
                             ExpressionInteractiveCommand texpr -> do
                                 val <- interactEvalExpression texpr
                                 action <- runValue outh val
                                 lift $ lift $ runPinaforeAction action
                             ShowTypeInteractiveCommand showinfo texpr -> do
                                 MkAnyValue (MkShimWit t shim) _ <- interactEvalExpression texpr
                                 liftIO $
                                     hPutStrLn outh $
                                     ":: " <>
                                     show t <>
                                     if showinfo
                                         then " # " <> show shim
                                         else ""
                             SimplifyTypeInteractiveCommand polarity ttype -> do
                                 MkAnyW t <- lift $ liftRS ttype
                                 liftIO $
                                     hPutStrLn outh $
                                     case polarity of
                                         PositiveType -> show $ pinaforeSimplifyTypes @baseedit $ MkAnyInKind t
                                         NegativeType -> show $ pinaforeSimplifyTypes @baseedit $ MkAnyInKind t
                             ErrorInteractiveCommand err -> liftIO $ hPutStrLn outh $ unpack err)
                    [ Handler $ \(err :: PinaforeError) -> hPutStrLn outh $ show err
                    , Handler $ \err -> hPutStrLn outh $ "error: " <> ioeGetErrorString err
                    ]
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
        ioRunInterpretResult . runPinaforeScoped
