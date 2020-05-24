module Pinafore.Language
    ( PinaforeError
    , InterpretResult
    , throwResult
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

runPinaforeScoped :: (?pinafore :: PinaforeContext) => PinaforeScoped a -> InterpretResult a
runPinaforeScoped scp =
    runScoped $
    withNewPatternConstructors predefinedPatternConstructors $ withNewBindings (qValuesLetExpr predefinedBindings) scp

runPinaforeSourceScoped :: (?pinafore :: PinaforeContext) => FilePath -> PinaforeSourceScoped a -> InterpretResult a
runPinaforeSourceScoped fpath scp = runPinaforeScoped $ runSourcePos (initialPos fpath) scp

parseValue :: (?pinafore :: PinaforeContext) => Text -> PinaforeSourceScoped QValue
parseValue text = do
    rexpr <- parseTopExpression text
    qEvalExpr rexpr

parseValueAtType ::
       forall t. (FromPinaforeType t, ?pinafore :: PinaforeContext)
    => Text
    -> PinaforeSourceScoped t
parseValueAtType text = do
    val <- parseValue text
    typedAnyToPinaforeVal val

entityTypedShowValue ::
       CovaryType dv -> EntityGroundType f -> DolanArguments dv PinaforeType f 'Positive t -> t -> Maybe String
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

groundTypedShowValue :: PinaforeGroundType dv t -> DolanArguments dv PinaforeType t 'Positive ta -> ta -> String
groundTypedShowValue (EntityPinaforeGroundType ct t) args v
    | Just str <- entityTypedShowValue ct t args v = str
groundTypedShowValue _ _ _ = "<?>"

singularTypedShowValue :: PinaforeSingularType 'Positive t -> t -> String
singularTypedShowValue (VarPinaforeSingularType _) _ = "<?>"
singularTypedShowValue (GroundPinaforeSingularType gt args) v = groundTypedShowValue gt args v

typedShowValue :: PinaforeType 'Positive t -> t -> String
typedShowValue NilPinaforeType v = never v
typedShowValue (ConsPinaforeType ts tt) v = joinf (singularTypedShowValue ts) (typedShowValue tt) v

showPinaforeRef :: QValue -> String
showPinaforeRef (MkAnyValue (MkShimWit t conv) v) = typedShowValue t (fromEnhanced conv v)

type Interact = StateT SourcePos (ReaderStateT PinaforeScoped View)

interactRunSourceScoped :: PinaforeSourceScoped a -> Interact a
interactRunSourceScoped sa = do
    spos <- get
    lift $ liftRS $ runSourcePos spos sa

interactEvalExpression :: PinaforeScoped QExpr -> Interact QValue
interactEvalExpression texpr =
    interactRunSourceScoped $ do
        expr <- liftSourcePos texpr
        qEvalExpr expr

runValue :: Handle -> QValue -> Interact (PinaforeAction ())
runValue outh val =
    interactRunSourceScoped $
    (typedAnyToPinaforeVal val) <|> (fmap outputLn $ typedAnyToPinaforeVal val) <|>
    (return $ liftIO $ hPutStrLn outh $ showPinaforeRef val)

interactParse :: Text -> Interact InteractiveCommand
interactParse t = remonad throwResult $ parseInteractiveCommand t

interactLoop :: (?pinafore :: PinaforeContext) => Handle -> Handle -> Bool -> Interact ()
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
                                     ": " <>
                                     show t <>
                                     if showinfo
                                         then " # " <> show shim
                                         else ""
                             SimplifyTypeInteractiveCommand polarity ttype -> do
                                 MkAnyW t <- lift $ liftRS ttype
                                 liftIO $
                                     hPutStrLn outh $
                                     case polarity of
                                         PositiveType -> show $ pinaforeSimplifyTypes $ MkAnyInKind t
                                         NegativeType -> show $ pinaforeSimplifyTypes $ MkAnyInKind t
                             ErrorInteractiveCommand err -> liftIO $ hPutStrLn outh $ unpack err)
                    [ Handler $ \(err :: PinaforeError) -> hPutStrLn outh $ show err
                    , Handler $ \err -> hPutStrLn outh $ "error: " <> ioeGetErrorString err
                    ]
            interactLoop inh outh echo

interact :: (?pinafore :: PinaforeContext) => Handle -> Handle -> Bool -> View ()
interact inh outh echo = do
    liftIO $ hSetBuffering outh NoBuffering
    evalReaderStateT (evalStateT (interactLoop inh outh echo) (initialPos "<input>")) $ throwResult . runPinaforeScoped
