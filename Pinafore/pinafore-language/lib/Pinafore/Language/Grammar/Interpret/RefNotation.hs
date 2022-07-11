module Pinafore.Language.Grammar.Interpret.RefNotation
    ( RefNotation
    , RefExpression
    , liftRefNotation
    , hoistRefNotation
    , sourcePosRefNotation
    , runRefNotation
    , refNotationUnquote
    , refNotationQuote
    ) where

import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Language.VarID
import Shapes

type RefNotation = WriterT [(VarID, QExpr)] (StateT VarIDState PinaforeInterpreter)

sourcePosRefNotation :: SourcePos -> RefNotation --> RefNotation
sourcePosRefNotation = paramWith $ liftParam $ liftParam sourcePosParam

runRefWriterT :: RefNotation --> StateT VarIDState PinaforeInterpreter
runRefWriterT wma = do
    (a, w) <- runWriterT wma
    case w of
        [] -> return a
        _ -> throw NotationBareUnquoteError

liftRefNotation :: PinaforeInterpreter --> RefNotation
liftRefNotation = lift . lift

hoistRefNotation :: WMFunction PinaforeInterpreter PinaforeInterpreter -> RefNotation --> RefNotation
hoistRefNotation (MkWMFunction mm) = hoist $ hoist mm

runRefNotation :: RefNotation --> PinaforeInterpreter
runRefNotation rexpr = evalStateT (runRefWriterT rexpr) firstVarIDState

type RefExpression = RefNotation QExpr

allocateVarRefNotation :: Name -> RefNotation VarID
allocateVarRefNotation name = do
    i <- lift get
    lift $ put $ nextVarIDState i
    return $ mkVarID i name

refNotationUnquote :: RefExpression -> RefExpression
refNotationUnquote rexpr = do
    v <- allocateVarRefNotation "%ref"
    expr <- lift $ runRefWriterT rexpr
    tell $ pure (v, expr)
    return $ qVarExpr v

stdModuleName :: ModuleName
stdModuleName = MkModuleName $ pure "Std"

aplist :: QExpr -> [QExpr] -> PinaforeInterpreter QExpr
aplist expr [] = return expr
aplist expr (arg:args) = do
    aprefExpr <- qName $ QualifiedReferenceName stdModuleName "applyWhole"
    expr' <- qApplyAllExpr aprefExpr [expr, arg]
    aplist expr' args

refNotationQuote :: RefExpression -> RefExpression
refNotationQuote rexpr =
    lift $ do
        (expr, binds) <- runWriterT rexpr
        lift $ do
            purerefExpr <- qName $ QualifiedReferenceName stdModuleName "pureWhole"
            aexpr <- qAbstractsExpr (fmap fst binds) expr
            raexpr <- qApplyExpr purerefExpr aexpr
            aplist raexpr $ fmap snd binds
