module Pinafore.Language
    ( PinaforePredefinitions
    , PinaforeError
    , InterpretResult
    , ioRunInterpretResult
    , PinaforeAction
    , qPositiveTypeDescription
    , qNegativeTypeDescription
    , FromPinaforeType
    , ToPinaforeType
    , parseTopExpression
    , parseValue
    , parseValueAtType
    , interact
    , TopType(..)
    , Entity
    , showPinaforeRef
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
import Truth.Core

runPinaforeScoped ::
       (PinaforePredefinitions baseupdate, ?pinafore :: PinaforeContext baseupdate)
    => PinaforeScoped baseupdate a
    -> InterpretResult a
runPinaforeScoped scp =
    runScoped $
    withNewPatternConstructors predefinedPatternConstructors $ withNewBindings (qValuesLetExpr predefinedBindings) scp

runPinaforeSourceScoped ::
       (PinaforePredefinitions baseupdate, ?pinafore :: PinaforeContext baseupdate)
    => FilePath
    -> PinaforeSourceScoped baseupdate a
    -> InterpretResult a
runPinaforeSourceScoped fpath scp = runPinaforeScoped $ runSourcePos (initialPos fpath) scp

parseValue ::
       forall baseupdate. (PinaforePredefinitions baseupdate, ?pinafore :: PinaforeContext baseupdate)
    => Text
    -> PinaforeSourceScoped baseupdate (QValue baseupdate)
parseValue text = do
    rexpr <- parseTopExpression @baseupdate text
    qEvalExpr rexpr

parseValueAtType ::
       forall baseupdate t.
       (PinaforePredefinitions baseupdate, FromPinaforeType baseupdate t, ?pinafore :: PinaforeContext baseupdate)
    => Text
    -> PinaforeSourceScoped baseupdate t
parseValueAtType text = do
    val <- parseValue @baseupdate text
    typedAnyToPinaforeVal @baseupdate val

entityTypedShowValue ::
       CovaryType dv
    -> EntityGroundType f
    -> DolanArguments dv (PinaforeType baseupdate) f 'Positive t
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
       PinaforeGroundType baseupdate dv 'Positive t
    -> DolanArguments dv (PinaforeType baseupdate) t 'Positive ta
    -> ta
    -> String
groundTypedShowValue (EntityPinaforeGroundType ct t) args v
    | Just str <- entityTypedShowValue ct t args v = str
groundTypedShowValue _ _ _ = "<?>"

singularTypedShowValue :: PinaforeSingularType baseupdate 'Positive t -> t -> String
singularTypedShowValue (VarPinaforeSingularType _) _ = "<?>"
singularTypedShowValue (GroundPinaforeSingularType gt args) v = groundTypedShowValue gt args v

typedShowValue :: PinaforeType baseupdate 'Positive t -> t -> String
typedShowValue NilPinaforeType v = never v
typedShowValue (ConsPinaforeType ts tt) v = joinf (singularTypedShowValue ts) (typedShowValue tt) v

showPinaforeRef :: QValue baseupdate -> String
showPinaforeRef (MkAnyValue (MkShimWit t conv) v) = typedShowValue t (fromEnhanced conv v)

type Interact baseupdate = StateT SourcePos (ReaderStateT (PinaforeScoped baseupdate) View)

interactRunSourceScoped :: PinaforeSourceScoped baseupdate a -> Interact baseupdate a
interactRunSourceScoped sa = do
    spos <- get
    lift $ liftRS $ runSourcePos spos sa

interactEvalExpression ::
       forall baseupdate. (PinaforePredefinitions baseupdate)
    => PinaforeScoped baseupdate (QExpr baseupdate)
    -> Interact baseupdate (QValue baseupdate)
interactEvalExpression texpr =
    interactRunSourceScoped $ do
        expr <- liftSourcePos texpr
        qEvalExpr expr

runValue :: Handle -> QValue baseupdate -> Interact baseupdate (PinaforeAction ())
runValue outh val =
    interactRunSourceScoped $
    (typedAnyToPinaforeVal val) <|> (fmap outputLn $ typedAnyToPinaforeVal val) <|>
    (return $ liftIO $ hPutStrLn outh $ showPinaforeRef val)

interactParse ::
       forall baseupdate. HasPinaforeEntityUpdate baseupdate
    => Text
    -> Interact baseupdate (InteractiveCommand baseupdate)
interactParse t = remonad ioRunInterpretResult $ parseInteractiveCommand @baseupdate t

interactLoop ::
       forall baseupdate. (PinaforePredefinitions baseupdate, ?pinafore :: PinaforeContext baseupdate)
    => Handle
    -> Handle
    -> Bool
    -> Interact baseupdate ()
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
                             LetInteractiveCommand fbind ->
                                 lift $ do
                                     MkWMFunction bind <- liftRS fbind
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
                                         PositiveType -> show $ pinaforeSimplifyTypes @baseupdate $ MkAnyInKind t
                                         NegativeType -> show $ pinaforeSimplifyTypes @baseupdate $ MkAnyInKind t
                             ErrorInteractiveCommand err -> liftIO $ hPutStrLn outh $ unpack err)
                    [ Handler $ \(err :: PinaforeError) -> hPutStrLn outh $ show err
                    , Handler $ \err -> hPutStrLn outh $ "error: " <> ioeGetErrorString err
                    ]
            interactLoop inh outh echo

interact ::
       forall baseupdate. (PinaforePredefinitions baseupdate, ?pinafore :: PinaforeContext baseupdate)
    => Handle
    -> Handle
    -> Bool
    -> View ()
interact inh outh echo = do
    liftIO $ hSetBuffering outh NoBuffering
    evalReaderStateT (evalStateT (interactLoop inh outh echo) (initialPos "<input>")) $
        ioRunInterpretResult . runPinaforeScoped
