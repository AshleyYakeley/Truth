module Pinafore.WebAPI.OpenAPI
    ( openAPIImporter
    ) where

import Data.Aeson hiding (Result)
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi hiding (items)
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

runM :: M Text -> Text
runM (SuccessResult t) = t
runM (FailureResult err) = "<error: " <> err <> ">"

getReferenced :: Referenced a -> M a
getReferenced =
    \case
        Ref ref -> throwExc $ "missing reference " <> getReference ref
        Inline a -> return a

mangle :: Text -> Text
mangle = let
    m :: Char -> String
    m ' ' = "__"
    m '_' = "_U"
    m c = pure c
    in mconcat . fmap (pack . m) . unpack

operationToFunction :: Operation -> M (Text, [Param])
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
        runM $ do
            ref <- maybeToM "missing _paramSchema" _paramSchema
            sch <- getReferenced ref
            t <- showSchema sch
            return $ ": " <> t
    in _paramName <> sig

showOperation :: Operation -> M Text
showOperation op = do
    (opid, params) <- operationToFunction op
    return $ opid <> "(" <> intercalate ", " (fmap showParam params) <> ")"

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
    return $
        mconcat
            [ valBDS "schemas" "" $ fmap showText $ InsOrd.toList $ _componentsSchemas $ _openApiComponents root
            , valBDS "servers" "" $ fmap showText $ _openApiServers root
            , valBDS "paths" "" $ fmap showText $ InsOrd.toList $ _openApiPaths root
            , valBDS "functions" "" $ do
                  (path, pathitem) <- InsOrd.toList $ _openApiPaths root
                  (opname, op) <- pathItemOperations pathitem
                  return $ runM (showOperation op) <> " = " <> opname <> " " <> pack path
            ]

openAPIImporter :: Importer
openAPIImporter = MkImporter "openapi" importOpenAPI
