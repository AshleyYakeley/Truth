module Pinafore.WebAPI.OpenAPI
    ( openAPIImporter
    ) where

import Data.Aeson as Aeson hiding (Result)
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi hiding (items, name)
import Data.Shim
import Pinafore.Language
import Pinafore.Language.API
import Pinafore.WebAPI.Fetch
import Shapes hiding (Param)

pathItemOperations :: PathItem -> [(Text, Operation)]
pathItemOperations PathItem {..} = let
    getOp (opname, mop) = do
        op <- mop
        return (opname, op)
    in mapMaybe
           getOp
           [ ("GET", _pathItemGet)
           , ("PUT", _pathItemPut)
           , ("POST", _pathItemPost)
           , ("DELETE", _pathItemDelete)
           , ("OPTIONS", _pathItemOptions)
           , ("HEAD", _pathItemHead)
           , ("PATCH", _pathItemPatch)
           , ("TRACE", _pathItemTrace)
           ]

type M = Result Text

runM :: M a -> ResultT Text IO a
runM = liftInner

runMText :: M Text -> Text
runMText (SuccessResult t) = t
runMText (FailureResult err) = "<error: " <> err <> ">"

getReferenced :: Referenced a -> M a
getReferenced =
    \case
        Ref ref -> throwExc $ "missing reference " <> getReference ref
        Inline a -> return a

mangle :: Text -> Name
mangle = let
    m :: Char -> String
    m ' ' = "__"
    m '_' = "_U"
    m c = pure c
    in MkName . mconcat . fmap (pack . m) . unpack

operationToFunction :: Operation -> M (Name, [Param])
operationToFunction Operation {..} = do
    case _operationDeprecated of
        Just True -> throwExc "deprecated"
        _ -> return ()
    opid <- maybeToM "missing _operationOperationId" _operationOperationId
    params <- for _operationParameters getReferenced
    return (mangle opid, params)

showSchema :: Schema -> M Text
showSchema Schema {..}
    | Just t <- _schemaType =
        case t of
            OpenApiArray -> do
                items <- maybeToM "missing _schemaItems" _schemaItems
                case items of
                    OpenApiItemsObject rs -> do
                        itemschema <- getReferenced rs
                        itemtext <- showSchema itemschema
                        return $ "List " <> itemtext
                    OpenApiItemsArray rss -> do
                        sct <-
                            for rss $ \rs -> do
                                itemschema <- getReferenced rs
                                showSchema itemschema
                        return $ intercalate " *: " sct
            OpenApiString -> return "Text"
            OpenApiInteger -> return "Integer"
            _ -> return $ showText t
showSchema _ = throwExc "missing _schemaType"

showParam :: Param -> Text
showParam Param {..} = let
    sig =
        runMText $ do
            ref <- maybeToM "missing _paramSchema" _paramSchema
            sch <- getReferenced ref
            t <- showSchema sch
            return $ ": " <> t
    in _paramName <> sig

showOperation :: Operation -> M Text
showOperation op = do
    (opid, params) <- operationToFunction op
    return $ showText opid <> "(" <> intercalate ", " (fmap showParam params) <> ")"

data Func r where
    MkFunc :: QShimWit 'Positive t -> ((Object -> r) -> t) -> Func r

data IOShimWit v where
    MkIOShimWit :: QShimWit 'Positive t -> (v -> IO t) -> IOShimWit v

tupleParamList :: [QShimWit 'Negative Value] -> QShimWit 'Negative [Value]
tupleParamList (w:ww) = mapNegShimWit (functionToShim "cons" $ \(a, aa) -> a : aa) $ pairShimWit w (tupleParamList ww)
tupleParamList [] = mapNegShimWit (functionToShim "nil" $ \() -> []) nullShimWit

tupleResponseList :: [IOShimWit Value] -> IOShimWit [Value]
tupleResponseList [] =
    MkIOShimWit nullShimWit $ \case
        [] -> return ()
        _ -> fail "tuple too long"
tupleResponseList (MkIOShimWit t1 f1:ww) =
    case tupleResponseList ww of
        MkIOShimWit tr fr ->
            MkIOShimWit (pairShimWit t1 tr) $ \case
                v1:vr -> liftA2 (,) (f1 v1) (fr vr)
                _ -> fail "tuple too short"

mkResponse :: Schema -> M (IOShimWit Value)
mkResponse Schema {..} = do
    t <- maybeToM "missing _schemaType" _schemaType
    case t of
        OpenApiArray -> do
            items <- maybeToM "missing _schemaItems" _schemaItems
            MkIOShimWit tl fl <-
                case items of
                    OpenApiItemsObject rs -> do
                        itemschema <- getReferenced rs
                        MkIOShimWit itemp vt <- mkResponse itemschema
                        return $ MkIOShimWit (listShimWit itemp) $ \vv -> for vv vt
                    OpenApiItemsArray rss -> do
                        pp <-
                            for rss $ \rs -> do
                                itemschema <- getReferenced rs
                                mkResponse itemschema
                        return $ tupleResponseList pp
            return $
                MkIOShimWit tl $ \case
                    Array x -> fl $ toList x
                    _ -> fail "not List"
        OpenApiString ->
            return $
            MkIOShimWit qType $ \case
                String x -> return x
                _ -> fail "not Text"
        OpenApiBoolean ->
            return $
            MkIOShimWit qType $ \case
                Bool x -> return x
                _ -> fail "not Boolean"
        OpenApiNull ->
            return $
            MkIOShimWit qType $ \case
                Null -> return ()
                _ -> fail "not Unit"
        -- OpenApiInteger -> return $ mapPosShimWit (functionToShim "JSON.Number" $ Number . fromInteger) qType
        _ -> throwExc $ "unknown _schemaType: " <> showText t

mkParam :: Schema -> M (QShimWit 'Negative Value)
mkParam Schema {..} = do
    t <- maybeToM "missing _schemaType" _schemaType
    case t of
        OpenApiArray -> do
            items <- maybeToM "missing _schemaItems" _schemaItems
            itemlist <-
                case items of
                    OpenApiItemsObject rs -> do
                        itemschema <- getReferenced rs
                        itemp <- mkParam itemschema
                        return $ listShimWit itemp
                    OpenApiItemsArray rss -> do
                        pp <-
                            for rss $ \rs -> do
                                itemschema <- getReferenced rs
                                mkParam itemschema
                        return $ tupleParamList pp
            return $ mapNegShimWit (functionToShim "JSON.Array" $ Array . fromList) itemlist
        OpenApiString -> return $ mapNegShimWit (functionToShim "JSON.String" String) qType
        OpenApiInteger -> return $ mapNegShimWit (functionToShim "JSON.Number" $ Number . fromInteger) qType
        OpenApiBoolean -> return $ mapNegShimWit (functionToShim "JSON.Bool" Bool) qType
        OpenApiNull -> return $ mapNegShimWit (functionToShim "JSON.Null" $ \() -> Null) qType
        _ -> throwExc $ "unknown _schemaType: " <> showText t

mkFunc :: QShimWit 'Positive r -> [Param] -> M (Func r)
mkFunc tr [] = return $ MkFunc tr $ \call -> call mempty
mkFunc tr (p:pp) = do
    ref <- maybeToM "missing _paramSchema" $ _paramSchema p
    sch <- getReferenced ref
    pw <- mkParam sch
    func <- mkFunc tr pp
    return $
        case pw of
            MkShimWit ta (MkPolarShim pf) ->
                case func of
                    MkFunc tf f ->
                        MkFunc (funcShimWit (mkShimWit ta) tf) $ \call a ->
                            f $ \obj -> call $ Aeson.insert (Aeson.fromText $ _paramName p) (shimToFunction pf a) obj

importOpenAPI :: Text -> ResultT Text IO (LibraryStuff ())
importOpenAPI t = do
    bs <- fetch t
    jsonval <-
        case eitherDecode bs of
            Left err -> liftInner $ FailureResult $ "invalid JSON: " <> pack err
            Right jsonval -> return jsonval
    root :: OpenApi <-
        case fromJSON jsonval of
            Error err -> liftInner $ FailureResult $ pack err
            Success val -> return val
    let
        operations :: [(Operation, Text, Text)]
        operations = do
            (path, pathitem) <- InsOrd.toList $ _openApiPaths root
            (opname, op) <- pathItemOperations pathitem
            return (op, opname, pack path)
        mkOperationFunction :: (Operation, Text, Text) -> M (LibraryStuff ())
        mkOperationFunction (op, opname, path) = do
            (name, params) <- operationToFunction op
            responseref <-
                maybeToM "no default response" $ InsOrd.lookup 200 $ _responsesResponses $ _operationResponses op
            response <- getReferenced responseref
            mto <-
                maybeToM "no known response content-type" $ InsOrd.lookup "application/json" $ _responseContent response
            rschemaref <- maybeToM "no response schema" $ _mediaTypeObjectSchema mto
            rschema <- getReferenced rschemaref
            MkIOShimWit responseType responseF <- mkResponse rschema
            func <- mkFunc (actionShimWit responseType) params
            let
                call :: Object -> IO Value
                call _ = return $ String $ opname <> " " <> path
            return $
                case func of
                    MkFunc qt f ->
                        valWitBDS
                            (UnqualifiedFullNameRef name)
                            (MkRawMarkdown $ fromMaybe "" $ _operationDescription op)
                            qt $
                        f $ \paramobj ->
                            liftIO $ do
                                respvalue <- call paramobj
                                responseF respvalue
    functions <- runM $ for operations mkOperationFunction
    return $
        mconcat $
        [ namespaceBDS
              "Info"
              [ valBDS "schemas" "" $ fmap showText $ InsOrd.toList $ _componentsSchemas $ _openApiComponents root
              , valBDS "servers" "" $ fmap showText $ _openApiServers root
              , valBDS "paths" "" $ fmap showText $ InsOrd.toList $ _openApiPaths root
              , valBDS "functions" "" $ do
                    (op, opname, path) <- operations
                    return $ runMText (showOperation op) <> " = " <> opname <> " " <> path
              ]
        ] <>
        functions

openAPIImporter :: Importer
openAPIImporter = MkImporter "openapi" importOpenAPI
