module Pinafore.Language
    ( Name
    , ModuleName(..)
    , FetchModule(..)
    , PinaforeSpecialVals
    , SpecialVals(..)
    , PinaforeError
    , InterpretResult
    , throwInterpretResult
    , runInterpretResult
    , PinaforeAction
    , qPositiveTypeDescription
    , qNegativeTypeDescription
    , FromPinaforeType
    , ToPinaforeType
    , typedShowValue
    , singularTypedShowValue
    , parseTopExpression
    , parseValue
    , parseValueUnify
    , parseValueSubsume
    , interact
    , TopType(..)
    , Var
    , A
    , B
    , C
    , X
    , Y
    , Entity
    , showPinaforeRef
    , runPinaforeSourceScoped
    , exprShow
    ) where

import Changes.Core
import Control.Exception (Handler(..), catches)
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Context
import Pinafore.Language.Convert
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Grammar
import Pinafore.Language.Interpreter
import Pinafore.Language.Library
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Language.Var
import Shapes
import System.IO.Error

runPinaforeScoped ::
       (?pinafore :: PinaforeContext, ?fetchModule :: FetchModule) => PinaforeInterpreter a -> InterpretResult a
runPinaforeScoped scp = runInterpreter loadModule spvals $ importScope implicitScope scp

loadModule ::
       (?pinafore :: PinaforeContext, ?fetchModule :: FetchModule)
    => ModuleName
    -> PinaforeInterpreter (Maybe PinaforeScope)
loadModule mname
    | Just scope <- getLibraryScope mname = return $ Just scope
loadModule mname = do
    mrr <- liftIO $ runFetchModule ?fetchModule mname
    case mrr of
        Nothing -> return Nothing
        Just (fpath, FailureResult err) ->
            throw $ MkErrorMessage (initialPos fpath) $ UnicodeDecodeError $ pack $ show err
        Just (fpath, SuccessResult text) ->
            fmap Just $ importScope implicitScope $ runSourcePos (initialPos fpath) $ parseModule text

spvals :: (?pinafore :: PinaforeContext, ?fetchModule :: FetchModule) => PinaforeSpecialVals
spvals = let
    specialEvaluate :: forall t. PinaforeType 'Positive t -> Text -> PinaforeAction (Either Text t)
    specialEvaluate t text = do
        ier <- liftIO $ evaluate $ runPinaforeSourceScoped "<evaluate>" $ parseValueSubsume t text
        result <- runInterpretResult ier
        return $
            case result of
                SuccessResult r -> Right r
                FailureResult err -> Left $ pack $ show err
    in MkSpecialVals {..}

runPinaforeSourceScoped ::
       (?pinafore :: PinaforeContext, ?fetchModule :: FetchModule)
    => FilePath
    -> PinaforeSourceInterpreter a
    -> InterpretResult a
runPinaforeSourceScoped fpath scp = runPinaforeScoped $ runSourcePos (initialPos fpath) scp

parseValue :: (?pinafore :: PinaforeContext) => Text -> PinaforeSourceInterpreter QValue
parseValue text = do
    rexpr <- parseTopExpression text
    qEvalExpr rexpr

parseValueUnify ::
       forall t. (FromPinaforeType t, ?pinafore :: PinaforeContext)
    => Text
    -> PinaforeSourceInterpreter t
parseValueUnify text = do
    val <- parseValue text
    typedAnyToPinaforeVal val

parseValueSubsume ::
       forall t. (?pinafore :: PinaforeContext)
    => PinaforeType 'Positive t
    -> Text
    -> PinaforeSourceInterpreter t
parseValueSubsume t text = do
    val <- parseValue text
    tsSubsumeValue @PinaforeTypeSystem t val

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
singularTypedShowValue (VarDolanSingularType _) _ = "<?>"
singularTypedShowValue (GroundDolanSingularType gt args) v = groundTypedShowValue gt args v
singularTypedShowValue (RecursiveDolanSingularType var pt) v =
    case unrollRecursiveType var pt of
        MkShimWit t iconv -> typedShowValue t $ shimToFunction (polarPolyIsoPositive iconv) v

typedShowValue :: PinaforeType 'Positive t -> t -> String
typedShowValue NilDolanType v = never v
typedShowValue (ConsDolanType ts tt) v = joinf (singularTypedShowValue ts) (typedShowValue tt) v

showPinaforeRef :: QValue -> String
showPinaforeRef (MkAnyValue (MkPosShimWit t conv) v) = typedShowValue t (shimToFunction conv v)

type Interact = StateT SourcePos (ReaderStateT PinaforeInterpreter View)

interactRunSourceScoped :: PinaforeSourceInterpreter a -> Interact a
interactRunSourceScoped sa = do
    spos <- get
    lift $ liftRS $ runSourcePos spos sa

interactEvalExpression :: PinaforeInterpreter QExpr -> Interact QValue
interactEvalExpression texpr =
    interactRunSourceScoped $ do
        expr <- liftSourcePos texpr
        qEvalExpr expr

runValue :: Handle -> QValue -> Interact (PinaforeAction ())
runValue outh val =
    interactRunSourceScoped $
    (typedAnyToPinaforeVal val) <|>
    (fmap (\(text :: Text) -> liftIO $ hPutStrLn outh $ unpack text) $ typedAnyToPinaforeVal val) <|>
    (return $ liftIO $ hPutStrLn outh $ showPinaforeRef val)

interactParse :: Text -> Interact InteractiveCommand
interactParse t = remonad throwInterpretResult $ parseInteractiveCommand t

interactLoop :: (?pinafore :: PinaforeContext, ?fetchModule :: FetchModule) => Handle -> Handle -> Bool -> Interact ()
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
                             LetInteractiveCommand bind ->
                                 lift $ do
                                     liftRS $ bind $ return () -- check errors
                                     updateRS bind
                             ExpressionInteractiveCommand texpr -> do
                                 val <- interactEvalExpression texpr
                                 action <- runValue outh val
                                 lift $ lift $ runPinaforeAction action
                             ShowTypeInteractiveCommand showinfo texpr -> do
                                 MkAnyValue (MkPosShimWit t shim) _ <- interactEvalExpression texpr
                                 liftIO $
                                     hPutStrLn outh $
                                     ": " <>
                                     unpack (exprShow t) <>
                                     if showinfo
                                         then " # " <> show shim
                                         else ""
                             SimplifyTypeInteractiveCommand polarity ttype -> do
                                 MkAnyW t <- lift $ liftRS ttype
                                 s :: Text <-
                                     case polarity of
                                         PositiveType -> do
                                             t' <-
                                                 interactRunSourceScoped $
                                                 runRenamer @PinaforeTypeSystem $
                                                 simplify @PinaforeTypeSystem $ MkAnyInKind t
                                             return $ exprShow t'
                                         NegativeType -> do
                                             t' <-
                                                 interactRunSourceScoped $
                                                 runRenamer @PinaforeTypeSystem $
                                                 simplify @PinaforeTypeSystem $ MkAnyInKind t
                                             return $ exprShow t'
                                 liftIO $ hPutStrLn outh $ unpack s
                             ErrorInteractiveCommand err -> liftIO $ hPutStrLn outh $ unpack err)
                    [ Handler $ \(err :: PinaforeError) -> hPutStrLn outh $ show err
                    , Handler $ \err -> hPutStrLn outh $ "error: " <> ioeGetErrorString err
                    ]
            interactLoop inh outh echo

interact :: (?pinafore :: PinaforeContext, ?fetchModule :: FetchModule) => Handle -> Handle -> Bool -> View ()
interact inh outh echo = do
    liftIO $ hSetBuffering outh NoBuffering
    evalReaderStateT (evalStateT (interactLoop inh outh echo) (initialPos "<input>")) $
        throwInterpretResult . runPinaforeScoped
