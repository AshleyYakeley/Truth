module Pinafore.Language.Interpret.Value
    ( interpretValue
    ) where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpreter
import Pinafore.Language.Type
import Pinafore.Language.VarID

interpretRecordValue :: QRecordValue -> Maybe [(Name, QExpression)] -> QInterpreter QExpression
interpretRecordValue (MkQRecordValue items rvexpr@(MkSealedFExpression vtype _)) marglist = do
    let
        getsigname :: forall a. QSignature 'Positive a -> Maybe Name
        getsigname (ValueSignature _ name _ _) = Just name
        signames :: [Name]
        signames = catMaybes $ listTypeToList getsigname items
    margmap <-
        for marglist $ \arglist -> do
            for_ arglist $ \(n, _) -> do
                if elem n signames
                    then return ()
                    else throw $ RecordConstructorExtraName n
            return $ mapFromList arglist
    let
        freeFixedVars = freeTypeVariables vtype
        freeFixedNames = fmap someTypeVarName $ toList freeFixedVars
        getName :: Maybe QExpression -> Name -> QInterpreter QExpression
        getName mdefexpr iname =
            case margmap of
                Nothing ->
                    case mdefexpr of
                        Just defexpr ->
                            interpretValueWithDefault (UnqualifiedFullNameRef iname) Nothing $ return defexpr
                        Nothing -> interpretValue (UnqualifiedFullNameRef iname) Nothing
                Just (argmap :: Map Name QExpression) ->
                    case lookup iname argmap of
                        Just arg -> return arg
                        Nothing ->
                            case mdefexpr of
                                Just defexpr -> return defexpr
                                Nothing -> throw $ RecordConstructorMissingName iname
    runRenamer @QTypeSystem [] freeFixedNames $ do
        sexpr <-
            listTypeFor items $ \case
                ValueSignature _ iname itype mdefault -> do
                    iexpr <- lift $ getName (fmap (MkSealedExpression (mkShimWit itype)) mdefault) iname
                    itype' <- unEndoM (renameType @QTypeSystem freeFixedNames RigidName) itype
                    iexpr' <- renameMappable @QTypeSystem [] FreeName iexpr
                    subsumerExpressionTo @QTypeSystem itype' iexpr'
        (resultExpr, ssubs) <- solveSubsumerExpression @QTypeSystem $ listProductSequence sexpr
        unEndoM (subsumerSubstitute @QTypeSystem ssubs) $ applySealedFExpression rvexpr resultExpr

interpretValueWithDefault ::
       FullNameRef -> Maybe [(Name, QExpression)] -> QInterpreter QExpression -> QInterpreter QExpression
interpretValueWithDefault name margmap mdefexpr = do
    mbv <- lookupMaybeValue name
    case (mbv, margmap) of
        (Just (ValueBoundValue e), Nothing) -> return e
        (Nothing, Nothing) -> mdefexpr
        (Nothing, Just _) -> throw $ LookupNotDefinedError name
        (Just (ValueBoundValue _), Just _) -> throw $ LookupNotRecordConstructorError name
        (Just (RecordBoundValue rv), _) -> interpretRecordValue rv margmap

interpretValue :: FullNameRef -> Maybe [(Name, QExpression)] -> QInterpreter QExpression
interpretValue name margmap =
    interpretValueWithDefault name margmap $ do
        spos <- paramAsk sourcePosParam
        return $ qVar $ mkBadVarID spos name
