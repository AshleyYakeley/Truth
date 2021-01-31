module Pinafore.Language.Grammar.Interpret.RefNotation
    ( RefNotation
    , RefExpression
    , varRefExpr
    , liftRefNotation
    , remonadRefNotation
    , runRefNotation
    , refNotationUnquote
    , refNotationQuote
    ) where

import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Language.Var
import Shapes

type RefNotation = WriterT [(Name, QExpr)] (StateT Int PinaforeInterpreter)

runRefWriterT :: MonadThrow ErrorMessage m => SourcePos -> MFunction (WriterT [(Name, QExpr)] m) m
runRefWriterT spos wma = do
    (a, w) <- runWriterT wma
    case w of
        [] -> return a
        _ -> throw $ MkErrorMessage spos NotationBareUnquoteError

liftRefNotation :: MFunction PinaforeInterpreter RefNotation
liftRefNotation = lift . lift

remonadRefNotation :: WMFunction PinaforeInterpreter PinaforeInterpreter -> MFunction RefNotation RefNotation
remonadRefNotation (MkWMFunction mm) = remonad $ remonad mm

runRefNotation :: SourcePos -> MFunction RefNotation PinaforeInterpreter
runRefNotation spos rexpr = evalStateT (runRefWriterT spos rexpr) 0

type RefExpression = RefNotation QExpr

varRefExpr :: SourcePos -> ReferenceName -> RefExpression
varRefExpr spos name =
    liftRefNotation $ do
        mexpr <- runSourcePos spos $ lookupLetBinding name
        case mexpr of
            Just expr -> return expr
            Nothing ->
                case name of
                    UnqualifiedReferenceName n -> return $ qVarExpr n
                    _ -> throw $ MkErrorMessage spos $ LookupRefNameUnknownError name

refNotationUnquote :: SourcePos -> RefExpression -> RefExpression
refNotationUnquote spos rexpr = do
    i <- lift get
    lift $ put $ i + 1
    let varname = fromString $ "%ref" <> show i
    expr <- lift $ runRefWriterT spos rexpr
    tell $ pure (varname, expr)
    return $ qVarExpr varname

purerefExpr :: QExpr
purerefExpr = qConstExpr (pure :: A -> PinaforeImmutableWholeRef A)

aprefExpr :: QExpr
aprefExpr =
    qConstExpr
        ((<*>) :: PinaforeImmutableWholeRef (A -> B) -> PinaforeImmutableWholeRef A -> PinaforeImmutableWholeRef B)

aplist :: QExpr -> [QExpr] -> PinaforeSourceInterpreter QExpr
aplist expr [] = return expr
aplist expr (arg:args) = do
    expr' <- qApplyAllExpr aprefExpr [expr, arg]
    aplist expr' args

refNotationQuote :: SourcePos -> RefExpression -> RefExpression
refNotationQuote spos rexpr =
    lift $ do
        (expr, binds) <- runWriterT rexpr
        lift $
            runSourcePos spos $ do
                aexpr <- qAbstractsExpr (fmap fst binds) expr
                raexpr <- qApplyExpr purerefExpr aexpr
                aplist raexpr $ fmap snd binds
