module Pinafore.Language.Interpret.Interact
    ( runInteract
    , showQValue
    )
where

import Control.Exception (Handler (..), catches)
import System.IO.Error

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpret.Expression
import Pinafore.Language.Interpreter
import Pinafore.Language.Type

showQValue :: QValue -> QInterpreter String
showQValue val = catch (fmap show $ qUnifyValue @Showable val) (\(_ :: QLocatedError) -> return "<?>")

type Interact = StateT SourcePos (ReaderStateT QInterpreter View)

interactRunQInterpreter :: QInterpreter a -> Interact a
interactRunQInterpreter sa = do
    spos <- get
    lift $ readerStateLift $ paramWith sourcePosParam spos $ sa

interactEvalExpression :: SyntaxExpression -> Interact QValue
interactEvalExpression sexpr =
    interactRunQInterpreter $ do
        expr <- interpretExpression sexpr
        qEvalExpr expr

runValue :: Handle -> QValue -> Interact (Action ())
runValue outh val =
    interactRunQInterpreter
        $ catchExc (qUnifyValue val)
        $ \_ -> do
            s <- showQValue val
            return $ liftIO $ hPutStrLn outh s

interactParse :: Text -> Interact InteractiveCommand
interactParse t = hoist fromParseResult $ parseInteractiveCommand t

actionWit :: QShimWit 'Negative t -> QShimWit 'Negative (Action t)
actionWit (MkShimWit t (MkPolarShim conv)) =
    shimWitToDolan
        $ mapShimWit (MkPolarShim $ cfmap conv)
        $ mkShimWit
        $ MkDolanGroundedType actionGroundType
        $ ConsCCRArguments (CoCCRPolarArgument t) NilCCRArguments

simplify' ::
    forall polarity t.
    Is PolarityType polarity =>
    EndoM QRenameTypeM (QShimWit polarity t)
simplify' =
    case polarityType @polarity of
        PositiveType -> simplify @QTypeSystem
        NegativeType -> simplify @QTypeSystem

interactSimplify :: Is PolarityType polarity => QShimWit polarity t -> Interact (QShimWit polarity t)
interactSimplify t = interactRunQInterpreter $ qRunTypeM $ runRenamer @QTypeSystem [] [] $ unEndoM simplify' t

interactLoop :: Handle -> Handle -> Bool -> Interact ()
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
                    ( unlift $ do
                        p <- interactParse $ pack inputstr
                        case p of
                            NullInteractiveCommand -> return ()
                            LetInteractiveCommand stdecl -> do
                                let
                                    bind :: QInterpreter --> QInterpreter
                                    bind = interpretDeclarationWith stdecl
                                interactRunQInterpreter $ bind $ return () -- check errors
                                lift $ readerStateUpdate bind
                            ExpressionInteractiveCommand sexpr -> do
                                val <- interactEvalExpression sexpr
                                action <- runValue outh val
                                lift $ lift $ runAction action
                            BindActionInteractiveCommand spat sexpr -> do
                                aval <- interactEvalExpression sexpr
                                MkSomeFor rwit action <- interactRunQInterpreter $ qUnifyF actionWit aval
                                r <- lift $ lift $ unliftActionOrFail action
                                let
                                    rval = MkSomeOf rwit r
                                    buildScope :: QScopeBuilder ()
                                    buildScope = do
                                        pat <- interpretPattern spat
                                        match <- builderLift $ qExpressionPatternMatch (qConstValue rval) pat
                                        registerMatchBindings match
                                    bind :: QInterpreter --> QInterpreter
                                    bind qia = withScopeBuilder buildScope $ \() -> qia
                                interactRunQInterpreter $ bind $ return () -- check errors
                                lift $ readerStateUpdate bind
                            ShowDocInteractiveCommand rname -> do
                                bmap <- interactRunQInterpreter $ getBindingInfoLookup
                                liftIO
                                    $ case fmap (docDescription . siDocumentation . snd) $ bmap rname of
                                        Nothing -> hPutStrLn outh $ "! " <> show rname <> " undefined"
                                        Just "" -> return ()
                                        Just doc -> hPutStrLn outh $ "#| " <> unpack (toText doc)
                            ShowTypeInteractiveCommand showinfo sexpr -> do
                                (tt, tshim) <-
                                    interactRunQInterpreter $ do
                                        expr <- interpretExpression sexpr
                                        expr'@(MkSealedExpression (MkShimWit _ shim) _) <- qSimplify expr
                                        ntt <- getRenderFullName
                                        return (ntt $ exprShow expr', show shim)
                                liftIO
                                    $ hPutStrLn outh
                                    $ ": "
                                    <> unpack tt
                                    <> if showinfo
                                        then " # " <> tshim
                                        else ""
                            SimplifyTypeInteractiveCommand polarity stype -> do
                                s :: NamedText <-
                                    case polarity of
                                        Positive -> do
                                            MkSome t <- interactRunQInterpreter $ interpretType @'Positive stype
                                            t' <- interactSimplify (mkShimWit t :: QShimWit 'Positive _)
                                            return $ exprShow t'
                                        Negative -> do
                                            MkSome t <- interactRunQInterpreter $ interpretType @'Negative stype
                                            t' <- interactSimplify (mkShimWit t :: QShimWit 'Negative _)
                                            return $ exprShow t'
                                ntt <- interactRunQInterpreter getRenderFullName
                                liftIO $ hPutStrLn outh $ unpack $ ntt s
                            ErrorInteractiveCommand err -> liftIO $ hPutStrLn outh $ unpack err
                    )
                    [ Handler $ \(err :: QLocatedError) -> hPutStrLn outh $ show err
                    , Handler $ \err -> hPutStrLn outh $ "! error: " <> ioeGetErrorString err
                    ]
            interactLoop inh outh echo

runInteract :: Handle -> Handle -> Bool -> (QInterpreter --> View) -> View ()
runInteract inh outh echo runqi =
    evalReaderStateT (evalStateT (interactLoop inh outh echo) (initialPos "<input>")) runqi
