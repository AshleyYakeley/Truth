module Pinafore.WebAPI.OpenAPI
    ( openAPIImporter
    ) where

import Data.Aeson as Aeson hiding (Result)
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi hiding (items, name, schema)
import Data.Shim
import Pinafore.Language
import Pinafore.Language.API
import Pinafore.WebAPI.Fetch
import Pinafore.WebAPI.JSONType
import Pinafore.WebAPI.OpenAPI.Schema
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
    params <- for _operationParameters $ getReferenced _componentsParameters
    return (mangle opid, params)

data Func r where
    MkFunc :: QShimWit 'Positive t -> (([(Text, Value)] -> r) -> t) -> Func r

data IOShimWit v where
    MkIOShimWit :: QShimWit 'Positive t -> (v -> IO t) -> IOShimWit v

tupleParamList :: [QShimWit 'Negative Value] -> QShimWit 'Negative [Value]
tupleParamList (w:ww) = mapNegShimWit (functionToShim "cons" $ \(a, aa) -> a : aa) $ pairShimWit w (tupleParamList ww)
tupleParamList [] = mapNegShimWit (functionToShim "nil" $ \() -> []) nullShimWit

objectFromList :: [(Text, Value)] -> Object
objectFromList = Aeson.fromList . fmap (\(t, v) -> (Aeson.fromText t, v))

objectParamList :: [(Text, QShimWit 'Negative (Maybe Value))] -> QShimWit 'Negative [(Text, Value)]
objectParamList ((k, w):ww) = let
    ff (Nothing, o) = o
    ff (Just v, o) = (k, v) : o
    in mapNegShimWit (functionToShim "cons" ff) $ pairShimWit w (objectParamList ww)
objectParamList [] = mapNegShimWit (functionToShim "nil" $ \() -> []) nullShimWit

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

objectResponseList :: [(Text, IOShimWit (Maybe Value))] -> IOShimWit Object
objectResponseList [] = MkIOShimWit nullShimWit $ \_ -> return ()
objectResponseList ((k, MkIOShimWit w1 f1):ww) =
    case objectResponseList ww of
        MkIOShimWit wr fr ->
            MkIOShimWit (pairShimWit w1 wr) $ \obj -> do
                t1 <- f1 $ Aeson.lookup (Aeson.fromText k) obj
                tr <- fr obj
                return (t1, tr)

mkResponse :: JSONType -> M (IOShimWit Value)
mkResponse =
    \case
        NullJSONType ->
            return $
            MkIOShimWit qType $ \case
                Null -> return ()
                _ -> fail "not Unit"
        BoolJSONType ->
            return $
            MkIOShimWit qType $ \case
                Bool x -> return x
                _ -> fail "not Boolean"
        StringJSONType ->
            return $
            MkIOShimWit qType $ \case
                String x -> return x
                _ -> fail "not Text"
        ListArrayJSONType t -> do
            MkIOShimWit itemp vt <- mkResponse t
            return $
                MkIOShimWit (listShimWit itemp) $ \case
                    Array x -> for (toList x) vt
                    _ -> fail "not List"
        TupleArrayJSONType tt -> do
            pp <- for tt mkResponse
            MkIOShimWit tl fl <- return $ tupleResponseList pp
            return $
                MkIOShimWit tl $ \case
                    Array x -> fl $ toList x
                    _ -> fail "not List"
        ObjectJSONType tt -> do
            pp <- for tt mkParamResponse
            MkIOShimWit tl fl <- return $ objectResponseList pp
            return $
                MkIOShimWit tl $ \case
                    Object x -> fl x
                    _ -> fail "not List"
        t -> throwExc $ "response NYI: " <> showText t

mkParamResponse :: JSONParamType -> M (Text, IOShimWit (Maybe Value))
mkParamResponse (MkJSONParamType k (MkJSONOptType opt jt)) = do
    MkIOShimWit t f <- mkResponse jt
    let
        w =
            case opt of
                Required ->
                    MkIOShimWit t $ \case
                        Just v -> f v
                        Nothing -> fail $ "missing key: " <> unpack k
                Optional -> MkIOShimWit (maybeShimWit t) $ \mv -> for mv f
    return (k, w)

mkParam :: JSONType -> M (QShimWit 'Negative Value)
mkParam =
    \case
        NullJSONType -> return $ mapNegShimWit (functionToShim "JSON.Null" $ \() -> Null) qType
        BoolJSONType -> return $ mapNegShimWit (functionToShim "JSON.Bool" Bool) qType
        IntegerJSONType -> return $ mapNegShimWit (functionToShim "JSON.Number" $ Number . fromInteger) qType
        StringJSONType -> return $ mapNegShimWit (functionToShim "JSON.String" String) qType
        ListArrayJSONType t -> do
            itemp <- mkParam t
            return $ mapNegShimWit (functionToShim "JSON.Array" $ Array . fromList) $ listShimWit itemp
        TupleArrayJSONType tt -> do
            pp <- for tt mkParam
            return $ mapNegShimWit (functionToShim "JSON.Array" $ Array . fromList) $ tupleParamList pp
        ObjectJSONType tt -> do
            pp <-
                for tt $ \(MkJSONParamType k (MkJSONOptType o t)) -> do
                    p <- mkParam t
                    let
                        mp =
                            case o of
                                Optional -> maybeShimWit p
                                Required -> mapNegShimWit (functionToShim "Just" Just) p
                    return (k, mp)
            return $ mapNegShimWit (functionToShim "JSON.Object" $ Object . objectFromList) $ objectParamList pp

mkFunc :: QShimWit 'Positive r -> [Param] -> M (Func r)
mkFunc tr [] = return $ MkFunc tr $ \call -> call []
mkFunc tr (p:pp) = do
    ref <- maybeToM "missing _paramSchema" $ _paramSchema p
    sch <- getReferenced _componentsSchemas ref
    pjt <- schemaToJSONType sch
    pw <- mkParam pjt
    func <- mkFunc tr pp
    return $
        case pw of
            MkShimWit ta (MkPolarShim pf) ->
                case func of
                    MkFunc tf f ->
                        MkFunc (funcShimWit (mkShimWit ta) tf) $ \call a ->
                            f $ \obj -> call $ (_paramName p, shimToFunction pf a) : obj

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
            response <- getReferenced _componentsResponses responseref
            mto <-
                maybeToM "no known response content-type" $ InsOrd.lookup "application/json" $ _responseContent response
            rschemaref <- maybeToM "no response schema" $ _mediaTypeObjectSchema mto
            rschema <- getReferenced _componentsSchemas rschemaref
            rjt <- schemaToJSONType rschema
            MkIOShimWit responseType responseF <- mkResponse rjt
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
                                respvalue <- call $ objectFromList paramobj
                                responseF respvalue
    functions <- runM (_openApiComponents root) $ for operations mkOperationFunction
    return $
        mconcat $
        [ namespaceBDS
              "Info"
              [ valBDS "schemas" "" $ fmap showText $ InsOrd.toList $ _componentsSchemas $ _openApiComponents root
              , valBDS "servers" "" $ fmap showText $ _openApiServers root
              , valBDS "paths" "" $ fmap showText $ InsOrd.toList $ _openApiPaths root
              ]
        ] <>
        functions

openAPIImporter :: Importer
openAPIImporter = MkImporter "openapi" importOpenAPI
